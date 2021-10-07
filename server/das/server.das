options indenting = 4
options no_aot = true
options stack = 4194304
options heap_size_hint = 8388608
options string_heap_size_hint = 8388608
options intern_strings = true
options persistent_heap = true
options multiple_contexts = true

require math
require daslib/ast_boost
require daslib/strings_boost
require daslib/defer
require network
require fio
require daslib/json_boost
require daslib/jobque_boost
require json_tools
require lib
require logger
require lsp_types
require rtti_tools
require completion
require ast_token
require config
require debugapi
require fs_tools

// rebuild all files from defPaths - references

// requires - options multiple_contexts = true
def clone(var a: array<AstToken?>&; b: array<AstToken?> implicit)
    a |> resize(length(b))
    for idx, bV in iter_range(b), b
        if a[idx] == null
            a[idx] = new AstToken
        *(a[idx]) := *bV

var
    telnet : TelnetServer?

struct Dascript
    uri : DocumentUri
    path : string
    text : string
    version : double

    isOpen: bool
    validated : bool

    completionData : Completions
    ast : AstTokens
    program: smart_ptr<rtti::Program>
    fs: smart_ptr<rtti::FileAccess>
    [[do_not_delete]] mg: ModuleGroup?


struct ValidateResult
    path: string
    ok: bool
    completionData: Completions
    ast: AstTokens
    diag: table<string; array<Diagnostic>>
    program: smart_ptr<rtti::Program>
    fs: smart_ptr<rtti::FileAccess>
    [[do_not_delete]] mg: ModuleGroup?


def Dascript(txt : TextDocumentItem)
    return <- [[Dascript uri = txt.uri, path = uri_to_path(txt.uri), version = txt.version, text = txt.text, isOpen = true]]

def Dascript(doc : DidChangeTextDocumentParams)
    return <- [[Dascript uri = doc.textDocument.uri, path = uri_to_path(doc.textDocument.uri), version = doc.textDocument.version, text = doc.contentChanges[0].text, isOpen = true]]

def Dascript(doc : DidSaveTextDocumentParams)
    return <- [[Dascript uri = doc.textDocument.uri, path = uri_to_path(doc.textDocument.uri), version = -1lf, text = doc.text, isOpen = true]]

def Dascript(doc : HoverParams)
    return <- [[Dascript uri = doc.textDocument.uri, path = uri_to_path(doc.textDocument.uri), version = -1lf, text = "", isOpen = true]]

typedef
    ReqCb = lambda<(data, error : JsonValue?):void>
    ValidDataCb = lambda<(data: Dascript const?):void>

struct Req
    id : double
    cb : ReqCb

struct ValidDataReq
    path: string
    cb: ValidDataCb

def find_token(data: Dascript const?; path: string; pos : Position; valid : block<(tok : AstToken?) : bool>) : AstToken?
    var token : AstToken?
    if data == null
        return token
    var minRatio = FLT_MAX
    collect_token(data.ast, path, pos) <| $(tok : AstToken?)
        if !valid |> invoke(tok)
            return
        let ratio = float(range_ratio(tok._range)) + (empty(tok.defPath) ? 100f : 0f) + (tok.topLevel ? 0. : 10.)
        if token == null || ratio < minRatio
            minRatio = ratio
            token = tok
    return token

def compile(path, dataText: string; withContent: bool; config: PluginConfig; var globalCompletion: Completions): ValidateResult
    var cloneData: ValidateResult
    cloneData.path = path

    let uri = path_to_uri(path)
    // info("> validate '{path}' project: '{config.project.file}'\n", get_line_info())

    var access <- make_file_access(config.project.file)
    if withContent && !empty(dataText)
        set_file_source(access, path, dataText)

    var mg = new [[ModuleGroup()]]
    var cp = CodeOfPolicies()
    cp.ignore_shared_modules = config.policies.ignoreSharedModules
    cp.no_global_variables = config.policies.noGlobalVariables
    cp.no_unused_block_arguments = config.policies.noUnusedBlockArguments
    cp.no_unused_function_arguments = config.policies.noUnusedFunctionArguments
    cp.fail_on_lack_of_aot_export = config.policies.failOnLackOfAotExport
    try
        compile_file(path, access, mg, cp) <| $(ok, program, error)
            // info("< compiled {path} {ok}\n'{error}'", get_line_info())

            if program != null
                cloneData.program := program
                cloneData.fs := access
                cloneData.mg = mg
                cloneData.ok = ok
            let compiled = ok && program != null

            if compiled
                var visitor <- new AstData()
                unsafe
                    visitor.tokens = addr(cloneData.ast)
                var adapter <- make_visitor(*visitor)
                visit(program, adapter)
                visitor.tokens = null
                unsafe
                    delete visitor
                adapter := null

                collect_all_token(cloneData.ast) <| $(var tok : AstToken?)
                    tok.path = resolve_path(tok.path, config)
                    if !empty(tok.defPath)
                        tok.defPath = resolve_path(tok.defPath, config)
                    if !empty(tok.typeDefPath)
                        tok.typeDefPath = resolve_path(tok.typeDefPath, config)

                completion::programCompletion(program, globalCompletion, cloneData.completionData)

                var diags : array<Diagnostic>
                collect_diag(cloneData.ast, path, diags)

                cloneData.diag[uri] <- diags
                return

            if program == null
                var diags : array<Diagnostic>
                diags |> emplace([[Diagnostic message := "{error}", severity = DiagnosticSeverity Error, _range = [[Range]] ]])
                cloneData.diag[uri] <- diags
                return

            for err in program.errors
                let localUri = err.at.fileInfo != null ? path_to_uri(resolve_path(string(err.at.fileInfo.name), config)) : uri
                let at = line_info_to_range(err.at)
                var diag <- [[Diagnostic message = "{err.what}", _range = at, severity = DiagnosticSeverity Error, code = double(int(err.cerr)) ]]
                if !empty(err.extra) || !empty(err.fixme)
                    var extra = "{err.extra}"
                    if !empty(err.fixme)
                        extra += "\n{err.fixme}"
                    var info = [[DiagnosticRelatedInformation location = [[Location _range = at, uri = localUri ]], message = extra ]]
                    diag.relatedInformation <- [{ auto[] info }]
                unsafe
                    var list = find_for_edit(cloneData.diag, localUri) // map is locked
                    if list != null
                        *list |> emplace(diag)
                    else
                        cloneData.diag[localUri] <- [{ auto[] diag }]
    recover
        if !empty(this_context().exception)
            error("failed to compile '{path}'\n{this_context().exceptionAt |> to_string()}:{this_context().exception}", get_line_info())
        else
            error("failed to compile '{path}'", get_line_info())

    access := null
    if cloneData.mg == null
        unsafe
            delete mg
    return <- cloneData

class TelnetServer : Server
    jobsNum = 1
    port : int // port number, for debug
    done : bool = false // server is done
    current_string : array<uint8> // raw input
    toRead : int = 0 // message length
    inited : bool = false // is LSP inited

    channel: jobque::Channel?

    // client capabilities
    hasConfigurationCapability : bool
    hasDiagnosticRelatedInformationCapability : bool

    completionData <- CompletionData() // global completion data

    workspaceFolders : array<WorkspaceFolder> // workspace data
    workspaceFiles: table<string; void?>
    workspaceFilesCursor: int = 0

    registry : table<string; Dascript> // path => Dascript
    // configurations
    configs : table<string; PluginConfig?> // path => configuration
    configReqs : table<string; double> // path => configuration request

    requestId : double = 1000lf // current request id
    requestCallbacks : array<Req> // active requests

    sendingError : bool = false

    validateQueue: table<string; tuple<first: bool; withContent: bool>>
    validDataReqs: array<ValidDataReq>
    validating: bool = false
    lazyQueue: table<string; tuple<first: bool; withContent: bool>>

    def TelnetServer()
        Server`Server(cast<Server> self)

    def override onError(msg:string; code:int)
        if sendingError
            print("server recursive error: {msg} - {code}\n")
            return
        sendingError = true
        info("server error:{code} - {msg}\n", get_line_info())
        sendingError = false
        done = true

    def override onConnect()
        info("connected port : {port}\n", get_line_info())

    def override onDisconnect()
        if length(current_string) > 0
            info(string(current_string), get_line_info())
            clear(current_string)
        info("disconnected port : {port}\n", get_line_info())

    def restart()
        if _server != null
            var session : smart_ptr<NetworkServer>
            self->save(session)
            gc0_save_smart_ptr("telnet-session", session)

    def updateGlobalCompletion(force: bool = false)
        if completionData->updateGlobalList(force)
            for data in values(registry)
                completionData->appendToGlobalList(data.completionData)

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#responseMessage
    def sendRawData(var data : JsonValue?)
        var msg = write_json(data)
        var packStr = "Content-Length: {length(msg)}\r\n\r\n{msg}"
        // print("<------\n{packStr}\n-------\n")
        // print("<---- send data {length(packStr)}\n") // only print!
        unsafe
            self->send(reinterpret<uint8?> packStr, length(packStr))
            delete data

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#responseMessage
    def sendResponse(id : double; var data : JsonValue?)
        self->sendRawData(JV({{ "jsonrpc" => JV("2.0"); "id" => JV(id); "result" => data }}))

    def sendRequest(method : string; var data : JsonValue?; var cb : ReqCb) : double
        let reqId = requestId
        self->onRequest(reqId, cb)
        self->sendRawData(JV({{ "jsonrpc" => JV("2.0"); "id" => JV(reqId); "method" => JV(method); "params" => data }}))
        requestId += 1lf
        return reqId

    def onRequest(id : double; var cb : ReqCb)
        requestCallbacks |> emplace([[Req id = id, cb <- cb]])

    def sendError(id : double; var data : ResponseError)
        self->sendRawData(JV({{ "jsonrpc" => JV("2.0"); "id" => JV(id); "error" => JV(data) }}))
        delete data

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#notificationMessage
    def sendNotification(method : string; var data : JsonValue?)
        self->sendRawData(JV({{ "jsonrpc" => JV("2.0"); "method" => JV(method); "params" => data }}))

    def registerCapability(methods : array<string>)
        var registrations : array<Registration>
        for it in methods
            registrations |> emplace([[Registration method = it, id = it]])
        var registrationParams <- [[RegistrationParams registrations <- registrations]]
        defer_delete(registrationParams)
        self->sendRequest("client/registerCapability", JV(registrationParams)) <| @(data, error : JsonValue?)
            if error != null
                info("error: registerCapability", get_line_info())
                info(error, get_line_info())

    def getConfig(path : string; var cb : lambda<(var config : PluginConfig?):void>)
        var found = false
        configs |> find_for_edit_if_exists(path) <| $(config)
            found = true
            invoke(cb, *config)
            delete cb
        if found
            return

        var localCb <- @ <| [[<-cb]](data, error : JsonValue?)
            configReqs |> erase(path)
            if error == null && data != null && data.value is _array
                let arr & = data.value as _array
                if length(arr) > 0
                    var res <- new PluginConfig(arr[0])
                    // info("> got config {path}", get_line_info())
                    // info(res, get_line_info())
                    invoke(cb, res)
                    delete cb
                    configs[path] <- res
                    return

            error("error: config for {path}\n", get_line_info())
            error(error, get_line_info())
            configs[path] = null
            var dummy: PluginConfig?
            invoke(cb, dummy)
            delete cb

        found = configReqs |> find_if_exists(path) <| $(reqId)
            self->onRequest(*reqId, localCb)
        if found
            return
        var item <- [[ConfigurationItem section = "dascript", scopeUri = path_to_uri(path) ]]
        var configParams <- [[ConfigurationParams items <- [{auto[] item }] ]]
        var reqId = self->sendRequest("workspace/configuration", JV(configParams), localCb)
        configReqs[path] = reqId

    def log(msg : string; level : LogLevel)
        if !inited || done
            print(msg)
            return
        var res = [[LogMessageParams message = msg]]
        unsafe
            res._type = reinterpret<MessageType>(level)
        self->sendNotification("window/logMessage", JV(res))
        delete res

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize
    def initialize(id : double; params : JsonValue?)
        var ini <- InitializeParams(params)
        defer_delete(ini)
        workspaceFolders := ini.workspaceFolders
        delete workspaceFiles
        hasConfigurationCapability = ini.capabilities.workspace.configuration
        hasDiagnosticRelatedInformationCapability = ini.capabilities.textDocument.publishDiagnostics.relatedInformation
        var workspace <- [[WorkspaceFoldersServerCapabilities supported = true, changeNotifications = true]]
        var cmpl <- [[CompletionOptions resolveProvider = true, triggerCharacters <- [{ auto[] "." }] ]]
        var cap <- [[ServerCapabilities
            textDocumentSync = TextDocumentSyncKind Full,
            completionProvider <- cmpl,
            hoverProvider = true,
            definitionProvider = true,
            typeDefinitionProvider = true,
            referencesProvider = true,
            documentSymbolProvider = true,
            colorProvider = true,
            workspaceSymbolProvider = true,
            renameProvider = [[RenameOptions prepareProvider = true]],
            signatureHelpProvider <- [[SignatureHelpOptions triggerCharacters <- [{ auto[] "(" }] ]],
            workspace = [[ServerCapabilitiesWorkspace workspaceFolders <- workspace]],
            semanticTokensProvider <- [[SemanticTokensOptions
                legend <- [[SemanticTokensLegend
                    tokenTypes <- [{ auto[]
                        SemanticToken_class;
                        SemanticToken_enum;
                        SemanticToken_struct;
                        SemanticToken_variable;
                        SemanticToken_property;
                        SemanticToken_enumMember;
                        SemanticToken_function;
                        SemanticToken_method;
                        SemanticToken_macro;
                        SemanticToken_modifier;
                        SemanticToken_string;
                        SemanticToken_number
                    }]
                ]],
                full = true
            ]]
        ]]

        var res <- [[InitializeResult capabilities <- cap, serverInfo = [[ServerInfo name = "das_server"]] ]]
        self->sendResponse(id, JV(res))
        delete res

    def initialized()
        inited = true
        info("> initialized! port: {port}\n", get_line_info())
        self->updateGlobalCompletion(true)

        var capabilities <- [{ string[] "workspace/didChangeWorkspaceFolders" }]
        if hasConfigurationCapability
            capabilities |> push("workspace/didChangeConfiguration")
        if capabilities |> length() > 0
            self->registerCapability(capabilities)
        delete capabilities

    def getValidData(path: string; var cb: ValidDataCb)
        if !key_exists(registry, path) && !key_exists(validateQueue, path)
            error("unknown file {path}, forcibly validate", get_line_info())
            self->validate(path, false, true)

        var found = false
        registry |> find_for_edit_if_exists(path) <| $(data)
            if data.validated
                found = true
                cb |> invoke(data)
            else
                validateQueue[path] = [[auto true, true]]
        if found
            return
        if validateQueue |> key_exists(path)
            info("wait for valid data {path}", get_line_info())
            validDataReqs |> emplace([[ValidDataReq path = path, cb <- cb]])
            return

        error("unknown file data {path}", get_line_info())
        var data: Dascript?
        cb |> invoke(data) // unknown file

    def invalidateQueue()
        if !channel.isEmpty
            for item in each(channel, type<ValidateResult>)
                var cloneData: ValidateResult
                cloneData.ok = item.ok
                cloneData.path = clone_string(item.path)
                cloneData.ast := item.ast
                cloneData.completionData := item.completionData
                cloneData.diag := item.diag
                unsafe
                    var rwItem & = reinterpret<ValidateResult -const&> item
                    cloneData.program <- rwItem.program
                    cloneData.fs <- rwItem.fs
                    cloneData.mg <- rwItem.mg
                self->validationDone(cloneData)
            validating = false
            if channel.size == 0
                channel |> append(jobsNum)

        if validating
            return

        if validateQueue |> length == 0 // equeue workspace files
            for it in values(configs)
                if !it.project.scanWorkspace
                    return

            if workspaceFiles |> length == 0
                workspaceFilesCursor = 0
                for it in workspaceFolders
                    it.uri |> uri_to_path() |> scan_dir(workspaceFiles)

            if workspaceFiles |> length == 0 || workspaceFilesCursor > workspaceFiles |> length
                return

            for i, path in range(length(workspaceFiles)), keys(workspaceFiles)
                if i == workspaceFilesCursor
                    if registry |> key_exists(path)
                        workspaceFilesCursor += 1
                        info("> {workspaceFilesCursor + 1}/{length(workspaceFiles)} skip '{path}'. Already registered", get_line_info())
                    else
                        info("> {workspaceFilesCursor + 1}/{length(workspaceFiles)} enqueue '{path}'", get_line_info())
                        registry[path] <- [[Dascript uri = path_to_uri(path), path = path, version = -1lf]]
                        validateQueue[path] = [[auto false, false]]
                        break
            workspaceFilesCursor += 1

            if validateQueue |> length == 0
                return

        var toValidate = ""
        for name, data in keys(validateQueue), values(validateQueue)
            if data.first
                toValidate = name
                break
        if toValidate |> empty
            for name in keys(validateQueue)
                toValidate = name
                break
        if toValidate |> empty
            return

        var data = validateQueue |> find(toValidate)
        self->validate(toValidate, data.withContent, data.first)


    def validate(path : string; withContent : bool = false; first: bool = false)
        let data = registry |> find(path)
        let valid = data != null && data.validated
        if valid
            info("> '{path}' is valid. skip validation", get_line_info())
            return
        if validating
            var found = false
            validateQueue |> find_for_edit_if_exists(path) <| $(var item)
                found = true
                item.withContent ||= withContent
                item.first ||= first
            if !found
                validateQueue[path] = [[auto first, withContent]]
            elif !lazyQueue |> key_exists(path)
                lazyQueue[path] = [[auto first, withContent]]
            return

        validating = true
        if !validateQueue |> key_exists(path)
            validateQueue[path] = [[auto first, withContent]]

        self->getConfig(path) <| @(var config : PluginConfig?)
            if config == null
                validating = false
                error("> validate error: '{path}'. Config is null", get_line_info())
            else
                self->validate_with_config(path, withContent, *config)

    def validate_with_config(path : string; withContent : bool; var config : PluginConfig)
        if !config.project.scanWorkspace
            var t0 = ref_time_ticks()
            registry |> find_for_edit_if_exists(path) <| $(var data)
                data.validated = true
                info("> validate '{path}' project: '{config.project.file}'\n", get_line_info())
                var cloneData <- compile(path, data.text, withContent, config, completionData.globalCompletion)
                self->validationDone(cloneData)

            let dt = get_time_usec(t0)
            let sec = double(dt)/1000000.0lf
            info("validation in {sec} s\n", get_line_info())
            validating = false
            self->dispatchValidData(path)
            return // if !config.project.scanWorkspace

        var dataText = ""
        var found = false
        registry |> find_if_exists(path) <| $(var data)
            found = true
            if withContent
                dataText = data.text
        if !found
            error("validation failed: unregistered file '{path}'\n", get_line_info())
            return

        for i in range(jobsNum)
            new_thread <| @[[:=config, :=dataText]]
                var cloneData <- compile(path, dataText, withContent, config, completionData.globalCompletion)
                channel |> push_clone(cloneData)
                channel |> notify()
                cloneData.program := null
                cloneData.fs := null
                delete cloneData
                delete config

    def validationDone(var item: ValidateResult)
        var path = item.path
        info("validation done: {path}\n", get_line_info())

        validating = false
        var isOpen = false
        var data: Dascript?
        unsafe
            data = registry |> find_for_edit(path) // registry is locked
        if data != null
            isOpen = data.isOpen

            if item.ok && item.program != null // otherwise keeps previous ast and completion data
                delete data.ast
                data.ast <- item.ast
                delete data.completionData
                data.completionData <- item.completionData
            else
                delete item.ast
                delete item.completionData

            unsafe
                delete data.mg
            data.program := null
            data.fs := null
            if isOpen
                data.program := item.program
                data.fs := item.fs
                data.mg <- item.mg
                item.mg = null

            data.validated = true

            completionData->appendToGlobalList(data.completionData)

        if isOpen
            for k, v in keys(item.diag), values(item.diag)
                let localPath = uri_to_path(k)
                registry |> find_if_exists(localPath) <| $(fileData)
                    collect_diag(fileData.ast, localPath, v)
                var params <- [[PublishDiagnosticsParams uri = k, diagnostics <- v ]]
                self->sendNotification("textDocument/publishDiagnostics", JV(params))
                delete params

        item.program := null
        item.fs := null
        unsafe
            delete item.mg
        delete item

        self->updateGlobalCompletion(false)
        self->dispatchValidData(path)

        var revalidate = false
        lazyQueue |> find_for_edit_if_exists(path) <| $(var lazyData)
            unsafe
                data = registry |> find_for_edit(path) // registry is locked
            if data != null
                info("re validate file {path}", get_line_info())
                revalidate = true
                data.validated = false
                // self->validate(path, lazyData.first, lazyData.withContent)
                validateQueue[path] <- *lazyData
        lazyQueue |> erase(path)

        // if revalidate
        //     return

    def dispatchValidData(path: string)
        if validateQueue |> key_exists(path)
            delete validateQueue[path]
            validateQueue |> erase(path)
        if length(validDataReqs) == 0
            return
        var data = registry |> find(path)
        var idx = 0
        while idx < length(validDataReqs)
            var req & = validDataReqs[idx]
            if req.path == path
                req.cb |> invoke(data)
                delete validDataReqs[idx]
                validDataReqs |> erase(idx)
            else
                idx += 1

    [unused_argument(id)] def didOpen(id : double; params : JsonValue?)
        var ini = DidOpenTextDocumentParams(params)
        defer_delete(ini)
        let path = uri_to_path(ini.textDocument.uri)
        var found = false
        registry |> find_for_edit_if_exists(path) <| $(var data)
            found = true
            data.isOpen = true
            data.validated = false
            data.text = ini.textDocument.text
            data.version = ini.textDocument.version
        if !found
            registry[path] <- Dascript(ini.textDocument)
            info("> open file {path} len {length(ini.textDocument.text)}, total files {length(registry)}\n", get_line_info())
        self->validate(path, /*withContent*/false, /*first*/true)

    [unused_argument(id)] def didSave(id : double; params : JsonValue?)
        var doc = DidSaveTextDocumentParams(params)
        defer_delete(doc)
        let path = uri_to_path(doc.textDocument.uri)
        info("> save file {path} {doc}. len {length(doc.text)}, total files {length(registry)}\n", get_line_info())
        var data: Dascript?
        unsafe
            data = registry |> find_for_edit(path) // registry is locked
        if data != null
            data.validated = false
        else
            registry[path] <- Dascript(doc)
            info("> unknown? register again, total files {length(registry)}\n", get_line_info())
        self->validate(path, /*withContent*/false, /*first*/true)

    [unused_argument(id)] def didChange(id : double; params : JsonValue?)
        var doc <- DidChangeTextDocumentParams(params)
        defer_delete(doc)
        if length(doc.contentChanges) == 0
            return

        let path = uri_to_path(doc.textDocument.uri)
        var data: Dascript?
        unsafe
            data = registry |> find_for_edit(path) // registry is locked
        if data != null
            data.text <- doc.contentChanges[0].text
            data.version = doc.textDocument.version
        else
            info("> file changed {path} len {length(doc.contentChanges[0].text)}, unknown? register again, total files {length(registry)}\n", get_line_info())
            registry[path] <- Dascript(doc)

        self->getConfig(path) <| @(var config : PluginConfig?)
            if !(config?.project?.scanWorkspace ?? false)
                return
            var data2: Dascript?
            unsafe
                data2 = registry |> find_for_edit(path) // registry is locked
            if data2 != null
                if data2.validated
                    data2.validated = false
                    self->validate(path, /*withContent*/true, /*first*/true)
                elif validateQueue |> key_exists(path) && !lazyQueue |> key_exists(path)
                    lazyQueue[path] = [[auto /*withContent*/true, /*first*/true]]

    [unused_argument(id)] def didClose(id : double; params : JsonValue?)
        var doc = DidCloseTextDocumentParams(params)
        defer_delete(doc)
        let path = uri_to_path(doc.textDocument.uri)
        registry |> find_for_edit_if_exists(path) <| $(var data)
            data.isOpen = false
            data.program := null
            data.fs := null
            data.validated = false
            unsafe
                delete data.mg
        info("> close file {path}, total files {length(registry)}\n", get_line_info())

        var res <- [[PublishDiagnosticsParams uri = path_to_uri(path) ]]
        self->sendNotification("textDocument/publishDiagnostics", JV(res))
        delete res

    def hover(id : double; params : JsonValue?)
        var ini = HoverParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        self->getConfig(path) <| @[[<-ini]](var config : PluginConfig?)
            let verbose = config?.hovers?.verbose ?? false
            let experimental = config?.experimental ?? false
            self->hover_with_config(id, ini, verbose, experimental)

    def hover_with_config(id : double; var ini : HoverParams&; verbose, experimental : bool)
        let path = uri_to_path(ini.textDocument.uri)
        if !registry |> key_exists(path)
            registry[path] <- Dascript(ini)
            warning("> {path} unknown? register again, total files {length(registry)}\n", get_line_info())
            self->validate(path, /*withContent*/false, /*first*/true)

        self->getValidData(path) <| @[[<-ini]](data: Dascript const?)
            defer_delete(ini)
            var found = false
            if data != null
                var hoversData : array<tuple<float; Range; string>>
                collect_token(data.ast, path, ini.position) <| $(it : AstToken?)
                    let ratio = float(range_ratio(it._range)) + (empty(it.defPath) ? 100f : 0f) + (it.topLevel ? 0. : 10.)
                    hoversData |> emplace([[auto ratio, it._range, to_hover(it, verbose, experimental)]])
                    completionData->findCompletion(data.completionData, it._type) <| $(val : Completion)
                        hoversData |> emplace([[auto ratio + 0.5, it._range, "```dascript\n{join(val.impls, "\n")}\n```"]])
                    // TODO: filter huge tokens?
                if hoversData |> length() > 0
                    hoversData |> sort() <| $(a; b)
                        return a._0 < b._0
                    var hovers <- [{ for it in hoversData; it._2 }]
                    var h <- [[ Hover _range = hoversData[0]._1, contents <- hovers ]]
                    self->sendResponse(id, JV(h))
                    delete h
                    found = true
                delete hoversData
            if !found
                warning("hover info not found @ {to_string(ini.position)}\n", get_line_info())
                self->sendResponse(id, JV(null))

    def completion(id : double; params : JsonValue?)
        var ini <- CompletionParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        self->getConfig(path) <| @[[<-ini]](var config : PluginConfig?)
            if config == null
                warning("unknowm file {path}. Send global completion {length(completionData.globalCompletion)}\n", get_line_info())
                self->sendResponse(id, JV(completionData.globalCompletion))
                return
            self->completion_with_config(id, ini, *config)

    def completion_with_config(id : double; var ini : CompletionParams&; var config : PluginConfig)
        let path = uri_to_path(ini.textDocument.uri)

        self->getValidData(path) <| @[[<-ini, :=config]](data: Dascript const?)
            defer_delete(ini)
            if data == null
                warning("< send global completion {length(completionData.globalCompletion)}\n", get_line_info())
                self->sendResponse(id, JV(completionData.globalCompletion))
                return
            let t0 = ref_time_ticks()
            var list : array<JsonValue?>
            // info(data.text, get_line_info())
            let idx = pos_to_index(ini.position, data.text)
            if idx > 0
                var searchToken = false
                var i = 0
                let maxI = min(idx, ini.position.character)
                unsafe
                    while i < maxI && is_white_space(character_uat(data.text, idx - i))
                        i++
                    while i < maxI
                        let ch = character_uat(data.text, idx - i)
                        if ch == '.' || ch == ':' || (ch == '>' && idx - i >= 1 && character_uat(data.text, idx - i - 1) == '-')
                            searchToken = true
                            i += ch == '.' && (idx - i < 1 || character_uat(data.text, idx - i - 1) != '?') ? 1 : 2
                            break
                        if (ch == 's' && idx - i >= 1 && (character_uat(data.text, idx - i - 1) == 'a' || character_uat(data.text, idx - i - 1) == 'i')
                            && character_uat(data.text, idx - i - 2) == ' ' || (character_uat(data.text, idx - i - 2) == '?' && idx - i >= 3 && character_uat(data.text, idx - i - 3) == ' ')
                        )
                            searchToken = true
                            i += (character_uat(data.text, idx - i - 2) == ' ' ? 3 : 4)
                            break
                        if ch == '[' || ch == '-'
                            break
                        i++
                if searchToken
                    let beforeSpacesPos = i
                    unsafe
                        while i < maxI && is_white_space(character_uat(data.text, idx - i))
                            i++
                        var insideO = 0
                        var insideD = 0
                        while i < maxI
                            let ch = character_uat(data.text, idx - i)
                            if ch == ')'
                                insideO ++
                            elif ch == ']'
                                insideD ++
                            elif ch == '('
                                if insideO > 0
                                    insideO --
                                if insideO == 0 && insideD == 0
                                    i ++
                                    break
                            elif ch == '['
                                if insideD > 0
                                    insideD --
                                if insideO == 0 && insideD == 0
                                    i ++
                                    break

                            if insideO > 0 || insideD > 0
                                i ++
                                continue
                            break
                    var pos = Position(ini.position.line, ini.position.character - i)
                    defer_delete(pos)
                    // let search = reverse(string(word))
                    // info("@@@@ search '{search}' {pos}", get_line_info())
                    // info("> validate '{path}' project: '{config.project.file}'\n", get_line_info())
                    // info(pos, get_line_info())
                    var prog: smart_ptr<rtti::Program>
                    var access: smart_ptr<rtti::FileAccess>
                    var mg: ModuleGroup?
                    defer <| $()
                        access := null
                        unsafe
                            delete mg
                    if config.project.scanWorkspace
                        prog := data.program
                    else
                        unsafe
                            access <- make_file_access(config.project.file)
                        set_file_source(access, path, data.text)

                        mg = new [[ModuleGroup()]]
                        var cp = CodeOfPolicies()
                        cp.ignore_shared_modules = true
                        compile_file(path, access, mg, cp) <| $[unused_argument(ok, error)](ok, program, error)
                            prog := program

                    if prog != null
                        var visitor <- new AstCompletionData()
                        visitor.path = path
                        visitor.pos = pos
                        var adapter <- make_visitor(*visitor)
                        visit(prog, adapter)

                        for it in visitor.res
                            list |> emplace(JV(it))

                        if visitor.found && !empty(visitor._type)
                            completionData->findFuncCompletion(data.completionData, visitor._type) <| $(val : Completion)
                                var comp = JV(val)
                                let isOperator = is_operator(val.name)
                                var obj & = comp.value as _object
                                var addEdit <- [[TextEdit
                                    _range = Range(ini.position.line, ini.position.character - beforeSpacesPos, ini.position.line, ini.position.character),
                                    newText = " "
                                ]]
                                obj["additionalTextEdits"] = JV([{ auto[] JV(addEdit) }])
                                var edit <- [[TextEdit
                                    _range = Range(ini.position.line, ini.position.character, ini.position.line, ini.position.character + 1),
                                    newText = isOperator ? "{val.name} " : "|> {val.name}("
                                ]]
                                obj["textEdit"] = JV(edit)
                                list |> emplace(comp)

                        unsafe
                            delete visitor
                        adapter := null
                    prog := null

            if list |> length() == 0
                for it in values(data.completionData)
                    list |> emplace <| JV(it)
                for val in values(completionData.globalCompletion)
                    list |> emplace <| JV(val)
                info("< merge program completion {length(data.completionData)} with global {length(completionData.globalCompletion)} => {length(list)}\n", get_line_info())
                var tokensList : table<string; void?>
                collect_token(data.ast, path) <| $(token : AstToken?)
                    if (token.kind == AstTokenKind Constant // ignore constants (strings for example)
                        || data.completionData |> contains(token.name)
                        || completionData.globalCompletion |> contains(token.name)
                        || tokensList |> key_exists(token.name)
                    )
                        return
                    tokensList[token.name] = null
                    list |> emplace(JV({{ "label" => JV(token.name); "kind" => JV(CompletionItemKind Text) }}))
                delete tokensList

            var res <- JV({{ "isIncomplete" => JV(false); "items" => JV(list)}})
            self->sendResponse(id, res)
            let sec = double(get_time_usec(t0))/1000000.0lf
            info("< completion time {sec}s", get_line_info())


    def resolveCompletion(id : double; var params : JsonValue?)
        self->sendResponse(id, clone_to_move(params))

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_definition
    def definition(id : double; var params : JsonValue?)
        var ini <- DefinitionParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        self->getValidData(path) <| @[[<-ini]](data: Dascript const?)
            defer_delete(ini)
            var token = find_token(data, path, ini.position) <| $(tok : AstToken?)
                return !empty(tok.defPath)
            if token == null
                self->sendResponse(id, JV(null))
                return
            var loc = [[Location uri = path_to_uri(token.defPath), _range = token.defRange]]
            self->sendResponse(id, JV(loc))
            delete loc

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_typeDefinition
    def typeDefinition(id : double; var params : JsonValue?)
        var ini <- TypeDefinitionParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        self->getValidData(path) <| @[[<-ini]](data: Dascript const?)
            defer_delete(ini)
            var token = find_token(data, path, ini.position) <| $(tok : AstToken?)
                return !empty(tok.typeDefPath)
            if token == null
                token = find_token(data, path, ini.position) <| $(tok : AstToken?)
                    return !empty(tok.defPath)
            if token == null
                self->sendResponse(id, JV(null))
                return
            let hasType = !empty(token.typeDefPath)
            var defPath = hasType ? token.typeDefPath : token.defPath
            let _range & = hasType ? token.typeDefRange : token.defRange
            var loc = [[Location uri = path_to_uri(defPath), _range = _range ]]
            self->sendResponse(id, JV(loc))
            delete loc

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_documentSymbol
    def documentSymbol(id : double; var params : JsonValue?)
        var ini <- DocumentSymbolParams(params)
        defer_delete(ini)
        let path = uri_to_path(ini.textDocument.uri)
        self->getValidData(path) <| @(data: Dascript const?)
            if data == null
                self->sendResponse(id, JV(null))
                return
            var res : array<JsonValue?>
            for it in data.ast
                it |> build_document_symbol(path, res)
            self->sendResponse(id, JV(res))

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#workspace_symbol
    def workspaceSymbol(id : double; var params : JsonValue?)
        var ini <- WorkspaceSymbolParams(params)
        defer_delete(ini)
        var res : array<JsonValue?>
        for data in values(registry)
            for it in data.ast
                it |> build_symbol_information(ini.query, 300, res)
        self->sendResponse(id, JV(res))

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#workspace_didChangeWorkspaceFolders
    [unused_argument(id)] def didChangeWorkspaceFolders(id : double; var params : JsonValue?)
        var ini <- DidChangeWorkspaceFoldersParams(params)
        defer_delete(ini)
        for it in ini.event.removed
            let idx = find_index_if(workspaceFolders) <| $(fldr) => fldr.uri == it.uri
            if idx >= 0
                workspaceFolders |> erase(idx)
        for it in ini.event.added
            workspaceFolders |> push(it)
        delete workspaceFiles

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#workspace_didChangeConfiguration
    [unused_argument(id, params)] def didChangeConfiguration(id : double; var params : JsonValue?)
        delete configs

    def getToken(data: Dascript const?; path: string; position: Position): AstToken?
        var token = find_token(data, path, position) <| $(tok : AstToken?)
            return !empty(tok.defPath) && !is_zero(tok.defRange)
        if token == null
            token = find_token(data, path, position) <| $(tok : AstToken?)
                return tok != null
        return token

    def getRefs(token: AstToken?): array<Location>
        var res : array<Location>
        if token == null
            return <- res
        // defer_delete(res)
        for dasItem in values(registry)
            var addDef = false
            if !empty(token.defPath)
                // same references
                collect_ref_token(dasItem.ast, token.defPath, token.defRange) <| $(tok : AstToken?)
                    addDef = true
                    res |> add_unique_reference(tok._range, path_to_uri(tok.path))
                if addDef
                    res |> add_unique_reference(token.defRange, path_to_uri(token.defPath))
            // refs to this token
            collect_ref_token(dasItem.ast, token.path, token._range) <| $(tok : AstToken?)
                res |> add_unique_reference(tok._range, path_to_uri(tok.path))
            if !addDef
                res |> add_unique_reference(token._range, path_to_uri(token.path))
        return <- res

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_references
    def references(id : double; var params : JsonValue?)
        var ini <- ReferenceParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        self->getValidData(path) <| @[[<-ini]](data: Dascript const?)
            defer_delete(ini)
            var token = self->getToken(data, path, ini.position)
            var res <- self->getRefs(token)
            var jsRes <- [{ for it in res; JV(it) }]
            self->sendResponse(id, JV(jsRes))
            delete res

    def signatureHelp(id : double; var params : JsonValue?)
        var ini <- SignatureHelpParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        self->getConfig(path) <| @[[<-ini]](var config : PluginConfig?)
            if config == null
                warning("unknowm file {path}. Send global completion {length(completionData.globalCompletion)}\n", get_line_info())
                self->sendResponse(id, JV(completionData.globalCompletion))
                return
            self->signatureHelp_with_config(id, ini, config)

    // TODO: config???
    [unused_argument(config)] def signatureHelp_with_config(id : double; var ini : SignatureHelpParams; config : PluginConfig?)
        let path = uri_to_path(ini.textDocument.uri)
        self->getValidData(path) <| @[[<-ini]](data: Dascript const ?)
            defer_delete(ini)
            var found = false
            if data != null
                let idx = pos_to_index(ini.position, data.text)
                if idx > 0
                    var list : array<SignatureInformation>
                    var searchToken = false
                    var i = 0
                    let maxI = min(idx, ini.position.character)
                    while i < maxI
                        var insideO = 0
                        unsafe
                            while i < maxI
                                let ch = character_uat(data.text, idx - i)
                                if ch == ')'
                                    insideO ++
                                elif ch == '('
                                    if insideO > 0
                                        insideO --
                                    else
                                        i ++
                                        searchToken = true
                                        break
                                i++
                            while i < maxI && is_white_space(character_uat(data.text, idx - i))
                                i++
                        if searchToken
                            var pos = Position(ini.position.line, ini.position.character - i)
                            var token = find_token(data, path, pos) <| $(tok : AstToken?)
                                return tok != null
                            if token != null && (token.kind == AstTokenKind Function || token.kind == AstTokenKind Method || token.kind == AstTokenKind Operator)
                                completionData->findCompletion(data.completionData, token._type) <| $(val : Completion)
                                    for it in val.impls
                                        list |> emplace([[SignatureInformation _label = it]])
                                if list |> length() == 0
                                    list |> emplace([[SignatureInformation _label = token.desc]])
                            if list |> length() > 0
                                self->sendResponse(id, JV([[SignatureHelp signatures <- list ]]))
                                found = true
                                break
            if !found
                self->sendResponse(id, JV(null))

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_documentColor
    def documentColor(id : double; var params : JsonValue?)
        var ini <- DocumentColorParams(params)
        defer_delete(ini)
        let path = uri_to_path(ini.textDocument.uri)
        self->getValidData(path) <| @(data: Dascript const?)
            var found = false
            if data != null
                var res : array<ColorInformation>
                defer_delete(res)
                var line = 0
                var charPos = 0
                var step = -1
                var currentNum : array<int>
                for i, ch in iter_range(data.text), data.text
                    if ch == '\n'
                        line++
                        charPos = 0
                        step = -1
                        clear(currentNum)
                        continue
                    if step == -1
                        step = ch == '0' ? 0 : -1
                    elif step == 0
                        step = ch == 'x' || ch == 'X' ? 1 : -1
                    elif step == 1
                        let isLowH = ch >= 'a' && ch <= 'f'
                        let isHighH = ch >= 'A' && ch <= 'F'
                        if is_number(ch) || isLowH || isHighH
                            currentNum |> push(isLowH ? ch - 'a' + 10 : isHighH ? ch - 'A' + 10 : ch - '0')
                        else
                            let len = length(currentNum)
                            if len == 6 || len == 8
                                var color : array<double>
                                if len == 6
                                    color |> push(1lf)
                                var k = 0
                                while k < len
                                    let h = currentNum[k++]
                                    let l = currentNum[k++]
                                    color |> push(double(h * 16 + l) / 255lf)
                                res |> emplace([[ColorInformation
                                    _range = Range(line, charPos - len - 2, line, charPos),
                                    color = [[Color alpha=color[0], red=color[1], green=color[2], blue=color[3]]]
                                ]])
                                delete color
                            step = -1
                            clear(currentNum)
                    charPos++
                delete currentNum
                if length(res) > 0
                    found = true
                    var arr <- [{ for it in res; JV(it)}]
                    self->sendResponse(id, JV(arr))
            if !found
                self->sendResponse(id, JV(null))

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_colorPresentation
    def colorPresentation(id : double; var params : JsonValue?)
        var ini <- ColorPresentationParams(params)
        defer_delete(ini)
        let len = length(ini._range)
        let color = ini.color |> to_string(len)
        var res <- [[ColorPresentation _label = color]]
        self->sendResponse(id, JV([{ auto[] JV(res) }]))
        delete res

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_rename
    [unused_argument(id)] def prepareRename(id : double; var params : JsonValue?)
        var ini <- PrepareRenameParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        self->getValidData(path) <| @[[<-ini]](data: Dascript const?)
            var token = self->getToken(data, path, ini.position)
            if token == null || !token._range |> range_oneline()
                self->sendResponse(id, JV(null))
            else
                self->sendResponse(id, JV(token._range))
                // self->sendResponse(id, JV({{ "defaultBehavior" => JV(true) }}))
            delete ini

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_prepareRename
    [unused_argument(id)] def rename(id : double; var params : JsonValue?)
        var ini <- RenameParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        self->getValidData(path) <| @[[<-ini]](data: Dascript const?)
            var token = self->getToken(data, path, ini.position)
            // info("rename token", get_line_info())
            // info(token, get_line_info())
            if token == null || !token._range |> range_oneline()
                self->sendResponse(id, JV(null))
                delete ini
                return
            // split module::name
            // split Struct : Parent
            var res <- self->getRefs(token)
            if length(res) == 0
                delete res
                self->sendResponse(id, JV(null))
                delete ini
                return
            let tokenLength = length(token._range)
            var edit: WorkspaceEdit
            for it in res
                // info(it, get_line_info())
                // info("{length(it._range)} != {tokenLength}", get_line_info())
                if length(it._range) != tokenLength
                    continue
                var found = false
                edit.changes |> find_for_edit_if_exists(it.uri) <| $(edits)
                    found = true
                    *edits |> emplace <| [[TextEdit _range = it._range, newText = ini.newName]]
                if !found
                    edit.changes[it.uri] <- [{auto[] [[TextEdit _range = it._range, newText = ini.newName]] }]
            // info(edit, get_line_info())
            self->sendResponse(id, JV(edit))
            delete ini
            delete res


    def add_semantic_token(idx: int; ast: AstToken?; var line, ch: int&; var res: SemanticTokens)
        if !range_oneline(ast._range) || length(ast._range) == 0
            return // TODO: multiline tokens
        var newLine = ast._range.start.line
        var newChar = ast._range.start.character
        if newLine == line
            newLine = 0
            newChar -= ch
            // if newChar < 0
            //     error("remove token. wrong char", get_line_info())
            //     error(ast, get_line_info())
            //     return
            ch += newChar
        else
            newLine -= line
            // if newLine < 0
            //     error("remove token. wrong line", get_line_info())
            //     error(ast, get_line_info())
            //     return
            line += newLine
            ch = newChar
        if newLine == 0 && newChar == 0
            // error("same token {newLine} {newChar} {length(ast._range)} {idx}", get_line_info())
            // error(ast, get_line_info())
            return // multiple tokens at one pos
        res.data |> push(newLine)
        res.data |> push(newChar)
        res.data |> push(length(ast._range))
        res.data |> push(idx)
        res.data |> push(0) // no tokens modifiers
        // info("{newLine} {newChar} {length(ast._range)} {idx}", get_line_info())
        // info(ast, get_line_info())

// kenTypes <- [{ auto[]
//                         SemanticToken_class;
//                         SemanticToken_enum;
//                         SemanticToken_struct;
//                         SemanticToken_variable;
                        // SemanticToken_property
//                         SemanticToken_enumMember;
//                         SemanticToken_function;
//                         SemanticToken_method
//                         SemanticToken_macro;
//                         SemanticToken_modifier
//                         SemanticToken_string;
//                         SemanticToken_number
//                     }]


    def collect_semantic_tokens(ast: AstToken?; var line, ch: int&; var res: SemanticTokens)
        if ast.kind == AstTokenKind Class
            self->add_semantic_token(0, ast, line, ch, res)
        elif ast.kind == AstTokenKind Enum
            self->add_semantic_token(1, ast, line, ch, res)
        elif ast.kind == AstTokenKind Struct
            self->add_semantic_token(2, ast, line, ch, res)
        elif ast.kind == AstTokenKind Variable
            self->add_semantic_token(3, ast, line, ch, res)
        elif ast.kind == AstTokenKind Field // property
            self->add_semantic_token(4, ast, line, ch, res)
        elif ast.kind == AstTokenKind EnumMember
            self->add_semantic_token(5, ast, line, ch, res)
        elif ast.kind == AstTokenKind Function
            self->add_semantic_token(6, ast, line, ch, res)
        elif ast.kind == AstTokenKind Method
            self->add_semantic_token(7, ast, line, ch, res)
        elif ast.kind == AstTokenKind Annotation
            self->add_semantic_token(8, ast, line, ch, res)
        elif ast.kind == AstTokenKind AnnotationArgument
            self->add_semantic_token(9, ast, line, ch, res)
        if ast.kind == AstTokenKind Constant
            if ast._type |> starts_with("string")
                self->add_semantic_token(10, ast, line, ch, res)
            else
                self->add_semantic_token(11, ast, line, ch, res)


    [unused_argument(id)] def semanticTokensFull(id : double; var params : JsonValue?)
        var ini <- SemanticTokensParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        self->getValidData(path) <| @[[<-ini]](data: Dascript const?)
            var tokens: array<AstToken?>
            data.ast |> collect_token(path) <| $(tok)
                tokens |> push(tok)
            var res: SemanticTokens
            var line, ch: int
            tokens |> sort <| $(a, b)
                return a._range.start < b._range.start
            var i = length(tokens) - 1
            while i > 0
                // remove type with variable/const in same pos
                if ((tokens[i].kind == AstTokenKind Variable || tokens[i].kind == AstTokenKind Constant)
                    && (tokens[i - 1].kind == AstTokenKind Struct || tokens[i - 1].kind == AstTokenKind Class || tokens[i - 1].kind == AstTokenKind EnumMember)
                    && tokens[i]._range == tokens[i - 1]._range)
                    tokens |> erase(i - 1)
                i -= 1
            for t in tokens
                self->collect_semantic_tokens(t, line, ch, res)

            self->sendResponse(id, JV(res))
            tokens |> clear()
            delete tokens
            delete res
            delete ini


    def override onData(message:uint8?; size:int)
        for i in range(size)
            var uch : uint8
            unsafe
                uch = message[i]
            let ch = int(uch)

            if toRead == 0
                push(current_string, uch)
                let prefix = "Content-Length: "
                if length(current_string) <= length(prefix)
                    continue
                if ch != '\n'
                    continue
                var str = string(current_string)
                if str |> starts_with(prefix)
                    unsafe
                        var lenStr = chop(str, 16, length(current_string) - length(prefix) - 2) // \r\n length
                        toRead = to_int(lenStr)
                        clear(current_string)
                continue

            if ch == '\n' || ch == '\r'
                continue

            push(current_string, uch)
            if length(current_string) < toRead
                continue

            toRead = 0
            var error : string
            var msg = read_json(current_string, error)
            clear(current_string)
            // if inited
            //     self->updateGlobalCompletion(false)

            if msg == null
                error("Json read error: {error}\n", get_line_info())
                return
            if msg.value is _object
                var obj & = msg.value as _object
                if obj |> key_exists("result")
                    let id = jon(msg, "id")
                    // info("> result {id} callbacks num: {length(requestCallbacks)}\n", get_line_info())
                    var found = false
                    var idx = 0
                    while idx < length(requestCallbacks)
                        if requestCallbacks[idx].id == id
                            found = true
                            invoke(requestCallbacks[idx].cb, joj(msg, "result"), joj(msg, "error"))
                            delete requestCallbacks[idx]
                            requestCallbacks |> erase(idx)
                        else
                            idx++
                    // info("> result after: cb num: {length(requestCallbacks)}\n", get_line_info())
                    if !found
                        warning("\n@@@@@@@@@@@@@@@@@@\nTODO: unhandled result: {id} \n@@@@@@@@@@@@@@@@@@\n", get_line_info())
                        warning(write_json(msg), get_line_info())

                elif obj |> key_exists("method")
                    let method = jos(msg, "method")
                    info("> method {method}\n", get_line_info())
                    if method == "initialize"
                        self->initialize(jon(msg, "id"), joj(msg, "params"))

                    elif method == "initialized"
                        self->initialized()

                    elif method == "textDocument/didOpen"
                        self->didOpen(jon(msg, "id"), joj(msg, "params"))

                    elif method == "textDocument/didSave"
                        self->didSave(jon(msg, "id"), joj(msg, "params"))

                    elif method == "textDocument/didChange"
                        self->didChange(jon(msg, "id"), joj(msg, "params"))

                    elif method == "textDocument/didClose"
                        self->didClose(jon(msg, "id"), joj(msg, "params"))

                    elif method == "textDocument/hover"
                        self->hover(jon(msg, "id"), joj(msg, "params"))

                    elif method == "textDocument/completion"
                        self->completion(jon(msg, "id"), joj(msg, "params"))

                    elif method == "completionItem/resolve"
                        self->resolveCompletion(jon(msg, "id"), joj(msg, "params"))

                    elif method == "textDocument/definition"
                        self->definition(jon(msg, "id"), joj(msg, "params"))

                    elif method == "textDocument/typeDefinition"
                        self->typeDefinition(jon(msg, "id"), joj(msg, "params"))

                    elif method == "textDocument/documentSymbol"
                        self->documentSymbol(jon(msg, "id"), joj(msg, "params"))

                    elif method == "workspace/symbol"
                        self->workspaceSymbol(jon(msg, "id"), joj(msg, "params"))

                    elif method == "workspace/didChangeWorkspaceFolders"
                        self->didChangeWorkspaceFolders(jon(msg, "id"), joj(msg, "params"))

                    elif method == "textDocument/references"
                        self->references(jon(msg, "id"), joj(msg, "params"))

                    elif method == "workspace/didChangeConfiguration"
                        self->didChangeConfiguration(jon(msg, "id"), joj(msg, "params"))

                    elif method == "textDocument/signatureHelp"
                        self->signatureHelp(jon(msg, "id"), joj(msg, "params"))

                    elif method == "textDocument/documentColor"
                        self->documentColor(jon(msg, "id"), joj(msg, "params"))

                    elif method == "textDocument/colorPresentation"
                        self->colorPresentation(jon(msg, "id"), joj(msg, "params"))

                    elif method == "textDocument/rename"
                        self->rename(jon(msg, "id"), joj(msg, "params"))

                    elif method == "textDocument/prepareRename"
                        self->prepareRename(jon(msg, "id"), joj(msg, "params"))

                    elif method == "textDocument/semanticTokens/full"
                        self->semanticTokensFull(jon(msg, "id"), joj(msg, "params"))

                    elif method == "exit"
                        done = true

                    elif method == "shutdown"
                        done = true

                    elif method == "$/cancelRequest"
                        pass // ignore???

                    elif method == "$/setTraceNotification" || method == "$/setTrace"
                        info(write_json(msg), get_line_info())
                        pass // ignore

                    else
                        warning("\n@@@@@@@@@@@@@@@@@@\nTODO: support: {method}\n@@@@@@@@@@@@@@@@@@\n", get_line_info())
                        warning(write_json(msg), get_line_info())
            else
                error(write_json(msg), get_line_info())
                error("\n@@@@@@@@@@@@@@@@@@\nError: unknown content\n@@@@@@@@@@@@@@@@@@\n", get_line_info())
            unsafe
                delete msg

[export]
def main(fn : string)
    return mainLoop(fn)

[export]
def main()
    mainLoop("")

def mainLoop(fn : string)
    let args <- get_command_line_arguments()
    debug(args)
    let port = get_int_arg(args, "--port", 9000)
    let main_time = stat(fn).mtime
    info("server start: port: {port} file:{fn} ts:{main_time}\n", get_line_info())

    telnet = new TelnetServer()
    telnet.port = port

    var res = false
    with_channel(telnet.jobsNum) <| $(channel)

        telnet.channel = channel

        var session : smart_ptr<NetworkServer> <- gc0_restore_smart_ptr("telnet-session")
        if session != null
            info("restoring server after restart\n", get_line_info())
            telnet->restore(session)
            telnet->initialized()
        elif telnet->init(port)
            info("server at port {port}\n", get_line_info())
        else
            info("server failed to initialize\n", get_line_info())
            return

        // var loggerFn <- @ <| (msg : string; level : LogLevel)
        //     if telnet != null
        //         telnet->log(msg, level)
        // logger::onLog <- loggerFn

        let initialstrLimit = uint64(2 * 1024 * 1024)
        var strLimit = initialstrLimit
        var step = 1u
        while !telnet.done
            telnet->tick()

            telnet->invalidateQueue()

            var alloc = string_heap_bytes_allocated()
            if alloc > strLimit
                // string_heap_report()
                var strReport = "STRING HEAP COLLECT: {bytes_hr(alloc)} -> "
                var heapReport = "HEAP COLLECT: {bytes_hr(heap_bytes_allocated())} -> "
                unsafe
                    // string_heap_collect()
                    heap_collect(true)
                alloc = string_heap_bytes_allocated()
                info("{strReport}{bytes_hr(alloc)} limit : {bytes_hr(strLimit)}\n{heapReport}{bytes_hr(heap_bytes_allocated())}", get_line_info())
                if alloc > strLimit
                    strLimit *= uint64(2)
                    warning("UP string heap limit: {bytes_hr(strLimit)}\n", get_line_info())
                elif strLimit > initialstrLimit && alloc < strLimit / uint64(2)
                    strLimit = strLimit / uint64(2)
                    warning("DOWN string heap limit: {bytes_hr(strLimit)}\n", get_line_info())

            step --
            if step == 0u
                let h = heap_bytes_allocated()
                let hd = heap_depth()
                var shd = string_heap_depth()

                step = 2000u

                info("string heap ({shd}): {bytes_hr(alloc)}. heap ({hd}): {bytes_hr(h)}\n", get_line_info())

            sleep(10u)
            let new_time = stat(fn).mtime
            if new_time != main_time
                info("restarting via source change {new_time}\n", get_line_info())
                telnet->restart()
                break
        unsafe
            delete telnet

        res = true

    return res
