options indenting = 4
options no_aot = true
options skip_lock_checks
options stack = 4194304 // 4 mb
options heap_size_hint = 8388608 // 8 mb
options string_heap_size_hint = 8388608 // 8 mb
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
require daslib/das_source_formatter
require json_tools
require lib
require dapLogger
require lsp_types
require rtti_tools
require completion
require ast_token
require config
require debugapi
require fs_tools
require dascript

// rebuild all files from defPaths - references

// default dascript implementation does not clone pointers
// def private builtin`clone ( var a:array<ast_token::AstToken? aka TT> explicit; b:array<ast_token::AstToken?> const# )
// 	__::builtin`resize(a,length(b))
// 	for aV,bV in a,b
// 		aV = bV

// requires - options multiple_contexts = true
// keep this clone right here, does not work from ast_token.das
def clone(var a : array<AstToken?>&; b : array<AstToken?> implicit)
    a |> resize(length(b))
    for idx, bV in iter_range(b), b
        if a[idx] == null
            a[idx] = new AstToken
        *(a[idx]) := *bV

var
    telnet : TelnetServer?


struct ValidateResult
    path : string
    ok : bool
    changeId : int
    completionData : Completions
    ast : AstTokens
    diag : table<string; array<Diagnostic>>
    program : smart_ptr<rtti::Program>
    fs : smart_ptr<rtti::FileAccess>
    [[do_not_delete]] mg : ModuleGroup?


typedef
    ReqCb = lambda<(data, error : JsonValue?) : void>
    ValidDataCb = lambda<(data : Dascript const?) : void>


struct Req
    id : double
    cb : ReqCb


struct ValidDataReq
    path : string
    cb : ValidDataCb

def fix_system_paths(path : string; workspaceFolders : array<WorkspaceFolder>)
    return path |> replace("$\{workspaceFolder\}", uri_to_path(workspaceFolders[0].uri))

def init_file_access(var access; config : PluginConfig; workspaceFolders : array<WorkspaceFolder>)
    assume roots = config.project.fileAccessRoots
    for rootName, rootPath in keys(roots), values(roots)
        var fixedPath = rootPath
        if !empty(workspaceFolders)
            fixedPath = fixedPath |> fix_system_paths(workspaceFolders)
        access |> add_file_access_root(rootName, fixedPath)


def compile(path, dataText : string; withContent : bool; changeId : int; config : PluginConfig; workspaceFolders : array<WorkspaceFolder>; var globalCompletion : Completions) : ValidateResult
    unsafe // struct with smart pointers
        var cloneData : ValidateResult
        cloneData.path = path
        cloneData.changeId = changeId

        let uri = path_to_uri(path)
        // info("> validate '{path}' project: '{config.project.file |> fix_system_paths(workspaceFolders)}'\n")

        var inscope access <- make_file_access(config.project.file |> fix_system_paths(workspaceFolders))
        if withContent && !empty(dataText)
            access |> set_file_source(path, dataText)
        access |> init_file_access(config, workspaceFolders)

        var mg = new [[ModuleGroup()]]
        var cp = CodeOfPolicies()
        cp.ignore_shared_modules = config.policies.ignoreSharedModules
        cp.no_global_variables = config.policies.noGlobalVariables
        cp.no_unused_block_arguments = config.policies.noUnusedBlockArguments
        cp.no_unused_function_arguments = config.policies.noUnusedFunctionArguments
        cp.fail_on_lack_of_aot_export = config.policies.failOnLackOfAotExport
        cp.export_all = true
        cp.aot_module = true
        cp.completion = true
        try
            compile_file(path, access, mg, cp) <| $(ok, program, error)
                // info("< compiled {path} {ok}\n'{error}'")

                if program != null
                    cloneData.program := program
                    cloneData.fs := access
                    cloneData.mg = mg
                    cloneData.ok = ok
                let compiled = ok && program != null

                if compiled
                    var visitor <- new AstData()
                    visitor.tokens = unsafe(addr(cloneData.ast))
                    var inscope adapter <- make_visitor(*visitor)
                    visit(program, adapter)
                    visitor.tokens = null
                    unsafe
                        delete visitor

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
                    let found = get(cloneData.diag, localUri) <| $(var list)
                        list |> emplace(diag)
                    if !found
                        cloneData.diag[localUri] <- [{ auto[] diag }]
        recover
            var diag <- [[Diagnostic _range = line_info_to_range(this_context().exceptionAt), severity = DiagnosticSeverity Error ]]
            if !empty(this_context().exception)
                diag.message = "failed to compile:{this_context().exception}"
            else
                diag.message = "failed to compile '{path}'"

            cloneData.diag[uri] <- [{ auto[] diag }]

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

    outChannel : jobque::Channel?

    // client capabilities
    hasConfigurationCapability : bool
    hasDiagnosticRelatedInformationCapability : bool

    completionData <- CompletionData() // global completion data

    workspaceFolders : array<WorkspaceFolder> // workspace data
    workspaceFiles : table<string; void?>
    workspaceFilesCursor : int = 0

    registry : table<string; Dascript> // path => Dascript
    // configurations
    configs : table<string; PluginConfig?> // path => configuration
    configReqs : table<string; double> // path => configuration request

    requestId : double = 1000lf // current request id
    requestCallbacks : array<Req> // active requests

    sendingError : bool = false

    validateQueue : table<string; tuple<first : bool; withContent : bool; changeId : int>>
    validatingFiles : table<string>
    validDataReqs : array<ValidDataReq>

    def TelnetServer()
        Server`Server(cast<Server> self)

    def override onError(msg : string; code : int)
        if sendingError
            print("server recursive error: {msg} - {code}\n")
            return
        sendingError = true
        info("server error:{code} - {msg}\n")
        sendingError = false
        done = true

    def override onConnect()
        info("connected port : {port}\n")

    def override onDisconnect()
        if length(current_string) > 0
            info(string(current_string))
            clear(current_string)
        info("disconnected port : {port}\n")

    def restart()
        if _server != null
            var inscope session : smart_ptr<NetworkServer>
            self->save(session)
            gc0_save_smart_ptr("telnet-session", session)

    def updateGlobalCompletion(force : bool = false)
        if completionData->updateGlobalList(force)
            for data in values(registry)
                completionData->appendToGlobalList(data.completionData)

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#responseMessage
    def sendRawData(var data : JsonValue?)
        var msg = write_json(data)
        var packStr = "Content-Length: {length(msg)}\r\n\r\n{msg}"
        // print("<------\n{packStr}\n-------\n")
        // print("<---- send data {length(packStr)}\n") // only print!
        self->send(unsafe(reinterpret<uint8?> packStr), length(packStr))
        unsafe
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

    def sendError(id : double; result : string)
        warning("send error {id}: {result}")
        self->sendRawData(JV({{ "jsonrpc" => JV("2.0"); "id" => JV(id); "result" => JV(result) }}))

    def sendErrorWithData(id : double; var data : ResponseError)
        warning("send error {id}: {data.code}: {data.message}")
        self->sendRawData(JV({{ "jsonrpc" => JV("2.0"); "id" => JV(id); "error" => JV(data) }}))
        delete data

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#notificationMessage
    def sendNotification(method : string; var data : JsonValue?)
        self->sendRawData(JV({{ "jsonrpc" => JV("2.0"); "method" => JV(method); "params" => data }}))

    def registerCapability(methods : array<string>)
        var registrations : array<Registration>
        for it in methods
            registrations |> emplace([[Registration method = it, id = it]])
        var inscope registrationParams <- [[RegistrationParams registrations <- registrations]]
        self->sendRequest("client/registerCapability", JV(registrationParams)) <| @(data, error : JsonValue?)
            if error != null
                info("error: registerCapability")
                info(error)

    def getConfig(path : string; var cb : lambda<(var config : PluginConfig?) : void>)
        var found = false
        configs |> get(path) <| $(config)
            found = true
            invoke(cb, unsafe(reinterpret<PluginConfig?> config))
            delete cb
        if found
            return

        var localCb <- @ <| [[ <- cb]](data, error : JsonValue ?)
            configReqs |> erase(path)
            if error == null && data != null && data.value is _array
                assume arr = data.value as _array
                if length(arr) > 0
                    var res <- new PluginConfig(arr[0])
                    invoke(cb, res)
                    delete cb
                    configs[path] <- res
                    return

            error("error: config for {path}\n")
            error(error)
            configs[path] = null
            var dummy : PluginConfig?
            invoke(cb, dummy)
            delete cb

        found = configReqs |> get(path) <| $(reqId)
            self->onRequest(reqId, localCb)
        if found
            return
        var item <- [[ConfigurationItem section = "dascript", scopeUri = path_to_uri(path) ]]
        var configParams <- [[ConfigurationParams items <- [{auto[] item }] ]]
        var reqId = self->sendRequest("workspace/configuration", JV(configParams), localCb)
        configReqs[path] = reqId

    def logMessage(msg : string; level : LogLevel)
        if !inited || done
            print(msg)
            return
        var res = [[LogMessageParams message = msg]]
        res._type = unsafe(reinterpret<MessageType> (level))
        self->sendNotification("window/logMessage", JV(res))
        delete res

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize
    def initialize(id : double; params : JsonValue?)
        var inscope ini <- InitializeParams(params)
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
            documentFormattingProvider = true,
            documentRangeFormattingProvider = false, // TODO:
            inlayHintProvider = true,
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
        info("> initialized! port: {port}\n")
        self->updateGlobalCompletion(true)

        var capabilities <- [{ string[] "workspace/didChangeWorkspaceFolders" }]
        if hasConfigurationCapability
            capabilities |> push("workspace/didChangeConfiguration")
        if capabilities |> length() > 0
            self->registerCapability(capabilities)
        delete capabilities

    def getValidData(path : string; var cb : ValidDataCb)
        if !key_exists(registry, path) && !key_exists(validateQueue, path)
            error("unknown file {path}, forcibly validate")
            self->validate(path, /*withContent*/false, /*first*/true)

        var found = false
        registry |> get(path) <| $(data)
            if data.validated
                found = true
                cb |> invoke(unsafe(addr(data)))
        if found
            return

        info("wait for valid data {path}")
        validDataReqs |> emplace([[ValidDataReq path = path, cb <- cb]])
        self->validate(path, /*withContent*/true, /*first*/true)

    def invalidateQueue()
        if !inited // wait initialization
            return
        if !outChannel.isEmpty
            for item in each_clone(outChannel, type<ValidateResult>)
                if true
                    var inscope cloneData : ValidateResult
                    cloneData.ok = item.ok
                    cloneData.changeId := item.changeId
                    cloneData.path = clone_string(item.path)
                    cloneData.ast := item.ast
                    cloneData.completionData := item.completionData
                    cloneData.diag := item.diag
                    var inscope rwItem := unsafe(reinterpret<ValidateResult- const&> item)
                    cloneData.program |> move <| rwItem.program
                    cloneData.fs |> move <| rwItem.fs
                    cloneData.mg <- rwItem.mg
                    self->validationDone(cloneData)
            // if outChannel.size == 0
            outChannel |> append(jobsNum - outChannel.size)

        if length(validatingFiles) >= jobsNum
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
                        info("> {workspaceFilesCursor + 1}/{length(workspaceFiles)} skip '{path}'. Already registered")
                    else
                        info("> {workspaceFilesCursor + 1}/{length(workspaceFiles)} enqueue '{path}'")
                        unsafe // struct with smart pointers
                            registry[path] <- [[Dascript uri = path_to_uri(path), path = path, version = -1lf]]
                        validateQueue[path] = [[auto false, false, 0]]
                        break
            workspaceFilesCursor += 1

            if validateQueue |> length == 0
                return

        var toValidate = ""
        for name, data in keys(validateQueue), values(validateQueue)
            if data.first && !key_exists(validatingFiles, name)
                toValidate = name
                break
        if toValidate |> empty
            for name in keys(validateQueue)
                if !key_exists(validatingFiles, name)
                    toValidate = name
                    break
        if toValidate |> empty
            return

        var data = unsafe(validateQueue?[toValidate])
        self->validate(toValidate, data.withContent, data.first)


    def validate(path : string; withContent : bool = true; first : bool = false)
        let data = unsafe(registry?[path])
        let changeId = data.changeId
        let valid = data != null && data.validated
        if valid
            info("> '{path}' is valid. skip validation")
            return
        if length(validatingFiles) >= jobsNum
            var found = false
            validateQueue |> get(path) <| $(var item)
                found = true
                item.changeId = changeId
                item.withContent ||= withContent
                item.first ||= first
            if !found
                validateQueue[path] = [[auto first, withContent, changeId]]
            return

        validatingFiles |> insert(path)
        if !validateQueue |> key_exists(path)
            validateQueue[path] = [[auto first, withContent, changeId]]

        self->getConfig(path) <| @(var config : PluginConfig?)
            if config == null
                validatingFiles |> erase(path)
                error("> validate error: '{path}'. Config is null")
            else
                self->validate_with_config(path, withContent, *config)

    def validate_with_config(path : string; withContent : bool; var config : PluginConfig)
        if !config.project.scanWorkspace
            var t0 = ref_time_ticks()
            var inscope cloneData : ValidateResult
            let found = registry |> get(path) <| $(var data)
                data.validated = true
                info("> validate '{path}' project: '{config.project.file}'\n")
                unsafe // struct with smart pointers
                    cloneData <- compile(path, data.text, withContent, data.changeId, config, workspaceFolders, completionData.globalCompletion)
            if found
                self->validationDone(cloneData)

            let dt = get_time_usec(t0)
            let sec = double(dt) / 1000000.0lf
            info("validation in {sec} s\n")
            validatingFiles |> erase(path)
            self->dispatchValidData(path)
            return

        var dataText = ""
        var changeId = 0
        if withContent
            var found = false
            registry |> get(path) <| $(var data)
                found = true
                changeId = data.changeId
                dataText = data.text
            if !found
                validatingFiles |> erase(path)
                error("validation failed: unregistered file '{path}'\n")
                return

        new_job <| @ [[ := config, := dataText]]
            unsafe // struct with smart pointers
                var inscope cloneData <- compile(path, dataText, withContent, changeId, config, workspaceFolders, completionData.globalCompletion)
                outChannel |> push_clone(cloneData)
                outChannel |> notify()
                cloneData.program := null
                cloneData.fs := null
                delete config

    def validationDone(var item : ValidateResult)
        var path = item.path
        validatingFiles |> erase(path)

        var isOpen = false
        var isActualData = true
        registry |> get(path) <| $(var data)
            isActualData = item.changeId == data.changeId
            info("validation done: {path}\nchangeId:{data.changeId} compiler changeId:{item.changeId}\n")
            isOpen = data.isOpen
            data.compiledOk = item.ok && item.program != null

            if data.compiledOk // otherwise keeps previous ast and completion data
                delete data.ast
                data.ast <- item.ast
                delete data.completionData
                data.completionData <- item.completionData
            else
                delete item.ast
                delete item.completionData

            unsafe
                delete data.mg
            delete data.program
            delete data.fs
            if isOpen
                data.program |> move <| item.program
                data.fs |> move <| item.fs
                data.mg <- item.mg
                item.mg = null

            data.validated = isActualData

            completionData->appendToGlobalList(data.completionData)

        if isOpen
            if isActualData
                for k, v in keys(item.diag), values(item.diag)
                    let localPath = uri_to_path(k)
                    registry |> get(localPath) <| $(fileData)
                        collect_diag(fileData.ast, localPath, v)
                    var params <- [[PublishDiagnosticsParams uri = k, diagnostics <- v ]]
                    self->sendNotification("textDocument/publishDiagnostics", JV(params))
                    delete params
            else
                for k in keys(item.diag)
                    var params <- [[PublishDiagnosticsParams uri = k ]]
                    self->sendNotification("textDocument/publishDiagnostics", JV(params))
                    delete params

        delete item.program
        delete item.fs
        unsafe
            delete item.mg
        delete item

        if isActualData
            self->updateGlobalCompletion(false)
            self->dispatchValidData(path)
        else
            info("re validate {path}")

    def dispatchValidData(path : string)
        if validateQueue |> key_exists(path)
            delete validateQueue[path]
            validateQueue |> erase(path)
        if length(validDataReqs) == 0
            return
        var data = unsafe(registry?[path])
        var idx = 0
        while idx < length(validDataReqs)
            assume req = validDataReqs[idx]
            if req.path == path
                req.cb |> invoke(data)
                delete validDataReqs[idx]
                validDataReqs |> erase(idx)
            else
                idx += 1

    [unused_argument(id)] def didOpen(id : double; params : JsonValue ?)
        var inscope ini = DidOpenTextDocumentParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        var found = false
        registry |> get(path) <| $(var data)
            found = true
            data.isOpen = true
            if data.validated
                data.validated = false
                data.changeId += 1
            data.text = ini.textDocument.text
            data.version = ini.textDocument.version
        if !found
            unsafe // struct with smart pointers
                registry[path] <- Dascript(ini.textDocument)
            info("> open file {path} len {length(ini.textDocument.text)}, total files {length(registry)}\n")
        self->validate(path, /*withContent*/true, /*first*/true)

    [unused_argument(id)] def didSave(id : double; params : JsonValue ?)
        var inscope doc = DidSaveTextDocumentParams(params)
        let path = uri_to_path(doc.textDocument.uri)
        info("> save file {path} {doc}. len {length(doc.text)}, total files {length(registry)}\n")
        let found = registry |> get(path) <| $(var data)
            if data.validated
                data.validated = false
                data.changeId += 1
        if !found
            unsafe // struct with smart pointers
                registry[path] <- Dascript(doc)
            info("> unknown? register again, total files {length(registry)}\n")
        self->validate(path, /*withContent*/true, /*first*/true)

    [unused_argument(id)] def didChange(id : double; params : JsonValue ?)
        var inscope doc <- DidChangeTextDocumentParams(params)
        if length(doc.contentChanges) == 0
            return

        let path = uri_to_path(doc.textDocument.uri)
        let found = registry |> get(path) <| $(var data)
            data.text <- doc.contentChanges[0].text
            data.version = doc.textDocument.version
            data.changeId += 1
        if !found
            info("> file changed {path} len {length(doc.contentChanges[0].text)}, unknown? register again, total files {length(registry)}\n")
            unsafe // struct with smart pointers
                registry[path] <- Dascript(doc)

        self->getConfig(path) <| @(var config : PluginConfig?)
            if !(config?.project?.scanWorkspace ?? false)
                return
            registry |> get(path) <| $(var data2)
                if data2.validated
                    data2.validated = false
                    self->validate(path, /*withContent*/true, /*first*/true)

    [unused_argument(id)] def didClose(id : double; params : JsonValue ?)
        var inscope doc = DidCloseTextDocumentParams(params)
        let path = uri_to_path(doc.textDocument.uri)
        registry |> get(path) <| $(var data)
            data.isOpen = false
            data.program := null
            data.fs := null
            data.validated = false
            unsafe
                delete data.mg
        info("> close file {path}, total files {length(registry)}\n")

        var res <- [[PublishDiagnosticsParams uri = path_to_uri(path) ]]
        self->sendNotification("textDocument/publishDiagnostics", JV(res))
        delete res

    def hover(id : double; params : JsonValue?)
        var ini = HoverParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        self->getConfig(path) <| @ [[ <- ini]] (var config : PluginConfig?)
            let verbose = config?.hovers?.verbose ?? false
            let experimental = config?.experimental ?? false
            self->hover_with_config(id, ini, verbose, experimental)

    def hover_with_config(id : double; var ini : HoverParams&; verbose, experimental : bool)
        let path = uri_to_path(ini.textDocument.uri)
        if !registry |> key_exists(path)
            unsafe // struct with smart pointers
                registry[path] <- Dascript(ini)
            warning("> {path} unknown? register again, total files {length(registry)}\n")
            self->validate(path, /*withContent*/true, /*first*/true)

        self->getValidData(path) <| @ [[ <- ini]] (data : Dascript const?)
            defer <|
                delete ini
            if data == null
                self->sendResponse(id, JVNull())
                return
            var found = false
            var hoversData : array<tuple<float; Range; string>>
            collect_token(data.ast, path, ini.position) <| $(it : AstToken?)
                if it.kind == AstTokenKind Block // ignore blocks
                    return
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
                warning("hover info not found @ {to_string(ini.position)}\n")
                self->sendResponse(id, JVNull())

    // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_completion
    def completion(id : double; params : JsonValue?)
        var ini <- CompletionParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        self->getConfig(path) <| @ [[ <- ini]] (var config : PluginConfig?)
            if config == null
                warning("unknowm file {path}. Send global completion {length(completionData.globalCompletion)}\n")
                self->sendResponse(id, JV(completionData.globalCompletion))
                return
            self->completion_with_config(id, ini, *config)

    def completion_with_config(id : double; var ini : CompletionParams&; var config : PluginConfig)
        let path = uri_to_path(ini.textDocument.uri)

        self->getValidData(path) <| @ [[ <- ini, := config]] (data : Dascript const?)
            defer <|
                delete ini
            if data == null
                warning("< send global completion {length(completionData.globalCompletion)}\n")
                self->sendResponse(id, JV(completionData.globalCompletion))
                return
            let t0 = ref_time_ticks()
            var list : array<JsonValue?>
            // info(data.text)
            let idx = pos_to_index(ini.position, data.text) - 1
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
                    while i < maxI && is_white_space(unsafe(character_uat(data.text, idx - i)))
                        i++
                    var insideO = 0
                    var insideD = 0
                    while i < maxI
                        let ch = unsafe(character_uat(data.text, idx - i))
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
                    // let search = reverse(string(word))
                    // info("@@@@ search '{search}' {pos}")
                    // info("> validate '{path}' project: '{config.project.file}'\n")
                    // info(pos)
                    var inscope prog : smart_ptr<rtti::Program>
                    var inscope access : smart_ptr<rtti::FileAccess>
                    var mg : ModuleGroup?
                    defer <|
                        unsafe
                            delete mg
                    if config.project.scanWorkspace
                        prog := data.program
                    else
                        access |> move_new <| make_file_access(config.project.file |> fix_system_paths(workspaceFolders))
                        access |> set_file_source(path, data.text)
                        access |> init_file_access(config, workspaceFolders)

                        mg = new [[ModuleGroup()]]
                        var cp = CodeOfPolicies()
                        cp.ignore_shared_modules = true
                        cp.completion = true
                        compile_file(path, access, mg, cp) <| $ [unused_argument(ok, error)] (ok, program, error)
                            prog := program

                    if prog != null
                        var visitor <- new AstCompletionData()
                        visitor.path = path
                        visitor.pos = pos
                        var inscope adapter <- make_visitor(*visitor)
                        visit(prog, adapter)

                        for it in visitor.res
                            var jitem = JV(it)
                            if it.kind == CompletionItemKind Method
                                assume obj = jitem.value as _object
                                var addEdit <- [[TextEdit
                                    _range = Range(ini.position.line, ini.position.character - beforeSpacesPos, ini.position.line, ini.position.character + 1),
                                    newText = " "
                                ]]
                                obj["additionalTextEdits"] = JV([{ auto[] JV(addEdit) }])
                                var edit <- [[TextEdit
                                    _range = Range(ini.position.line, ini.position.character, ini.position.line, ini.position.character + 2),
                                    newText = "->{it.name}("
                                ]]
                                obj["textEdit"] = JV(edit)
                            list |> emplace(jitem)

                        if visitor.found && length(visitor.types) > 0
                            for _type in keys(visitor.types)
                                completionData->findFuncCompletion(data.completionData, _type) <| $(val : Completion)
                                    var comp = JV(val)
                                    if val.kind != CompletionItemKind Field
                                        assume obj = comp.value as _object
                                        var addEdit <- [[TextEdit
                                            _range = Range(ini.position.line, ini.position.character - beforeSpacesPos, ini.position.line, ini.position.character),
                                            newText = " "
                                        ]]
                                        obj["additionalTextEdits"] = JV([{ auto[] JV(addEdit) }])
                                        var edit <- [[TextEdit
                                            _range = Range(ini.position.line, ini.position.character, ini.position.line, ini.position.character + 1),
                                            newText = is_operator(val.name) ? "{val.name} " : "|> {val.name}("
                                        ]]
                                        obj["textEdit"] = JV(edit)
                                    list |> emplace(comp)

                        unsafe
                            delete visitor

            if list |> length() == 0
                for it in values(data.completionData)
                    list |> push <| JV(it)
                for val in values(completionData.globalCompletion)
                    list |> push <| JV(val)
                info("< merge program completion {length(data.completionData)} with global {length(completionData.globalCompletion)} => {length(list)}\n")
                var tokensList : table<string; void?>
                collect_token(data.ast, path) <| $(token : AstToken?)
                    if (token.kind == AstTokenKind Constant // ignore constants (strings for example)
                        || data.completionData |> contains(token.name)
                        || completionData.globalCompletion |> contains(token.name)
                        || tokensList |> key_exists(token.name)
                    )
                        return
                    tokensList[token.name] = null
                    list |> push(JV({{ "label" => JV(token.name); "kind" => JV(CompletionItemKind Text) }}))
                delete tokensList

            var res <- JV({{ "isIncomplete" => JV(false); "items" => JV(list)}})
            self->sendResponse(id, res)
            let sec = double(get_time_usec(t0)) / 1000000.0lf
            info("< completion time {sec}s")
            delete list


    def resolveCompletion(id : double; var params : JsonValue?)
        self->sendResponse(id, clone_to_move(params))

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_definition
    def definition(id : double; var params : JsonValue?)
        var ini <- DefinitionParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        self->getValidData(path) <| @ [[ <- ini]] (data : Dascript const?)
            defer <|
                delete ini
            var token = find_token(data, path, ini.position) <| $(tok : AstToken?)
                return !empty(tok.defPath)
            if token == null
                self->sendResponse(id, JVNull())
                return
            var loc = [[Location uri = path_to_uri(token.defPath), _range = token.defRange]]
            self->sendResponse(id, JV(loc))
            delete loc

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_typeDefinition
    def typeDefinition(id : double; var params : JsonValue?)
        var ini <- TypeDefinitionParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        self->getValidData(path) <| @ [[ <- ini]] (data : Dascript const?)
            defer <|
                delete ini
            var token = find_token(data, path, ini.position) <| $(tok : AstToken?)
                return !empty(tok.typeDefPath)
            if token == null
                token = find_token(data, path, ini.position) <| $(tok : AstToken?)
                    return !empty(tok.defPath)
            if token == null
                self->sendResponse(id, JVNull())
                return
            let hasType = !empty(token.typeDefPath)
            var defPath = hasType ? token.typeDefPath : token.defPath
            let _range = hasType ? token.typeDefRange : token.defRange
            var loc = [[Location uri = path_to_uri(defPath), _range = _range ]]
            self->sendResponse(id, JV(loc))
            delete loc

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_documentSymbol
    def documentSymbol(id : double; var params : JsonValue?)
        var inscope ini <- DocumentSymbolParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        self->getValidData(path) <| @(data : Dascript const?)
            if data == null
                self->sendResponse(id, JVNull())
                return
            var res : array<JsonValue?>
            for it in data.ast
                it |> build_document_symbol(path, res)
            self->sendResponse(id, JV(res))

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#workspace_symbol
    def workspaceSymbol(id : double; var params : JsonValue?)
        var inscope ini <- WorkspaceSymbolParams(params)
        var res : array<JsonValue?>
        for data in values(registry)
            for it in data.ast
                it |> build_symbol_information(ini.query, 300, res)
        self->sendResponse(id, JV(res))

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#workspace_didChangeWorkspaceFolders
    [unused_argument(id)] def didChangeWorkspaceFolders(id : double; var params : JsonValue ?)
        var inscope ini <- DidChangeWorkspaceFoldersParams(params)
        for it in ini.event.removed
            let idx = find_index_if(workspaceFolders) <| $(fldr) => fldr.uri == it.uri
            if idx >= 0
                workspaceFolders |> erase(idx)
        for it in ini.event.added
            workspaceFolders |> push(it)
        delete workspaceFiles

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#workspace_didChangeConfiguration
    [unused_argument(id, params)] def didChangeConfiguration(id : double; var params : JsonValue ?)
        delete configs

    def getToken(data : Dascript const?; path : string; position : Position) : AstToken?
        var token = find_token(data, path, position) <| $(tok : AstToken?)
            return !empty(tok.defPath) && !is_zero(tok.defRange)
        if token == null
            token = find_token(data, path, position) <| $(tok : AstToken?)
                return tok != null
        return token

    def getRefs(token : AstToken?) : array<Location>
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
        self->getValidData(path) <| @ [[ <- ini]] (data : Dascript const?)
            defer <|
                delete ini
            if data == null
                self->sendResponse(id, JVNull())
                return
            var token = self->getToken(data, path, ini.position)
            var res <- self->getRefs(token)
            var jsRes <- [{ for it in res; JV(it) }]
            self->sendResponse(id, JV(jsRes))
            delete res

    def signatureHelp(id : double; var params : JsonValue?)
        var ini <- SignatureHelpParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        self->getConfig(path) <| @ [[ <- ini]] (var config : PluginConfig?)
            if config == null
                warning("unknowm file {path}. Send global completion {length(completionData.globalCompletion)}\n")
                self->sendResponse(id, JV(completionData.globalCompletion))
                return
            self->signatureHelp_with_config(id, ini, config)

    // TODO: config???
    [unused_argument(config)] def signatureHelp_with_config(id : double; var ini : SignatureHelpParams; config : PluginConfig ?)
        let path = uri_to_path(ini.textDocument.uri)
        self->getValidData(path) <| @ [[ <- ini]] (data : Dascript const?)
            defer <|
                delete ini
            if data == null
                self->sendResponse(id, JVNull())
                return
            var found = false
            let idx = pos_to_index(ini.position, data.text) - 1 // starts before closed )
            if idx > 0
                var list : array<SignatureInformation>
                var searchToken = false
                var i = 0
                let maxI = min(idx, ini.position.character)
                while i < maxI
                    var insideO = 0
                    while i < maxI
                        let ch = unsafe(character_uat(data.text, idx - i))
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
                    while i < maxI && is_white_space(unsafe(character_uat(data.text, idx - i)))
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
                self->sendResponse(id, JVNull())

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_documentColor
    def documentColor(id : double; var params : JsonValue?)
        var inscope ini <- DocumentColorParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        self->getConfig(path) <| @ [[ <- ini]] (var config : PluginConfig?)
            if config == null
                warning("unknowm file {path}.\n")
                self->sendResponse(id, JVNull())
                return
            self->documentColor_with_config(id, ini, config)

    def documentColor_with_config(id : double; var ini : DocumentColorParams; config : PluginConfig ?)
        let path = uri_to_path(ini.textDocument.uri)
        self->getValidData(path) <| @(data : Dascript const?)
            if data == null
                self->sendResponse(id, JVNull())
                return
            var res : array<ColorInformation>
            // defer_delete(res) // BUG:
            var line = 0
            var charPos = 0
            var step = -1
            var currentNum : array<int>
            // defer_delete(currentNum)
            for i, ch in iter_range(data.text), data.text
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
                            var _range = Range(line, charPos - len - 2, line, charPos)
                            if config.colorPreviewFormat == ColorPreviewFormat RGBA
                                res |> emplace([[ColorInformation
                                    _range <- _range,
                                    color = [[Color alpha=color[3], red = color[0], green = color[1], blue = color[2]]]
                                ]])
                            else
                                res |> emplace([[ColorInformation
                                    _range <- _range,
                                    color = [[Color alpha=color[0], red = color[1], green = color[2], blue = color[3]]]
                                ]])
                            delete color
                        step = -1
                        clear(currentNum)
                charPos++
                if ch == '\n'
                    line++
                    charPos = 0
                    step = -1
                    clear(currentNum)
                    continue
            if length(res) > 0
                var arr <- [{ for it in res; JV(it)}]
                self->sendResponse(id, JV(arr))
            else
                self->sendResponse(id, JVNull())
            delete currentNum
            delete res

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_colorPresentation
    def colorPresentation(id : double; var params : JsonValue?)
        var inscope ini <- ColorPresentationParams(params)
        let len = length(ini._range)
        let color = ini.color |> to_string(len)
        var res <- [[ColorPresentation _label = color]]
        self->sendResponse(id, JV([{ auto[] JV(res) }]))
        delete res

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_rename
    [unused_argument(id)] def prepareRename(id : double; var params : JsonValue ?)
        var ini <- PrepareRenameParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        self->getValidData(path) <| @ [[ <- ini]] (data : Dascript const?)
            var token = self->getToken(data, path, ini.position)
            if token == null || !token._range |> range_oneline()
                self->sendResponse(id, JVNull())
            else
                self->sendResponse(id, JV(token._range))
        finally
            delete ini

    // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_formatting
    [unused_argument(id)] def formatting(id : double; var params : JsonValue ?)
        var ini <- DocumentFormattingParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        self->getValidData(path) <| @ [[ <- ini]] (data : Dascript const?)
            defer <|
                delete ini
            if data == null
                self->sendResponse(id, JVNull())
                return
            let formatted = format_source_string(data.text)
            var res <- [{TextEdit[]
                [[TextEdit
                    _range = Range(0, 0, INT_MAX, 0),
                    newText = formatted
                ]]
            }]
            self->sendResponse(id, JV(res))
            delete res

    // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_inlayHint
    [unused_argument(id)] def inlayHint(id : double; var params : JsonValue ?)
        var ini <- InlayHintParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        self->getValidData(path) <| @ [[ <- ini]] (data : Dascript const?)
            defer <|
                delete ini
            if data == null || !data.compiledOk
                self->sendResponse(id, JVNull())
                return
            var usedIndexes : table<int; string>
            var res : array<InlayHint>
            data.ast |> collect_token(path, ini._range) <| $(tok)
                if (tok.kind == AstTokenKind Function || tok.kind == AstTokenKind Block) && length(tok.resultType) > 0 && !is_zero(tok.funcHeaderEndPos)
                    let fromIdx = pos_to_index(tok.funcHeaderEndPos, data.text)
                    let toIdx = pos_to_index(tok.bodyStartPos, data.text)
                    var bracketsCount = 0
                    var totalBrackets = 0
                    var lastBracket = fromIdx
                    var foundIdx = -1
                    for idx in range(fromIdx, toIdx)
                        let ch = unsafe(character_uat(data.text, idx))
                        if ch == '('
                            bracketsCount += 1
                            totalBrackets += 1
                        elif ch == ')'
                            bracketsCount -= 1
                            totalBrackets += 1
                            // arg : Type<(..(..)..)> ) == bracketsCount == -1
                            if bracketsCount == 0
                                lastBracket = idx + 1
                            if bracketsCount == -1
                                foundIdx = idx + 1
                                break

                    // def foo == totalBrackets == 0
                    // () == totalBrackets == 2 && bracketsCount == 0
                    if foundIdx == -1 && (totalBrackets == 0 || (totalBrackets == 2 && bracketsCount == 0))
                        foundIdx = lastBracket

                    for idx in range(foundIdx, toIdx)
                        let ch = unsafe(character_uat(data.text, idx))
                        if ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
                            continue
                        if ch == ':' // type declaration? reset searching
                            foundIdx = -1
                        break

                    if foundIdx >= 0 && usedIndexes?[foundIdx] ?? "" != tok.resultType
                        usedIndexes[foundIdx] = tok.resultType
                        res |> emplace([[InlayHint
                                position=index_to_pos(foundIdx, data.text),
                                _label=tok.resultType,
                                paddingLeft=true,
                                // tooltip="{tok.kind}:{tok._range}",
                                kind=InlayHintKind Type]])
                elif tok.kind == AstTokenKind Constant && length(tok.callArgument) > 0
                    let idx = pos_to_index(tok._range.start, data.text)
                    if usedIndexes?[idx] ?? "" != tok.callArgument
                        usedIndexes[idx] = tok.callArgument
                        res |> emplace([[InlayHint
                            position=tok._range.start,
                            _label=tok.callArgument,
                            paddingRight=true,
                            // tooltip="{tok.kind}:{tok._range}",
                            kind=InlayHintKind Parameter]])
                elif tok.kind == AstTokenKind Variable && tok._range |> in_range(tok.defRange)
                    let textLen = length(data.text)
                    var endIdx = pos_to_index(tok._range.end, data.text)
                    let akaLen = length(tok._aka)

                    if akaLen > 0
                        var akaIdx = 0
                        var i = endIdx
                        while i < textLen - 3
                            let ch1 = unsafe(character_uat(data.text, i + 0))
                            let ch2 = unsafe(character_uat(data.text, i + 1))
                            let ch3 = unsafe(character_uat(data.text, i + 2))
                            if ch1 == 'a' && ch2 == 'k' && ch3 == 'a'
                                akaIdx = i + 3
                                break
                            i += 1
                        if akaIdx > 0
                            i = akaIdx + 1
                            while i < textLen - 3
                                let ch = unsafe(character_uat(data.text, i))
                                if ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
                                    i += 1
                                    continue
                                if slice(data.text, i, i + akaLen) == tok._aka
                                    akaIdx = i + akaLen
                                else
                                    akaIdx = -1
                                break
                        if akaIdx > 0
                            endIdx = akaIdx

                    if usedIndexes?[endIdx] ?? "" == tok._type
                        return
                    var insertHint = false
                    var idx = endIdx
                    while idx < textLen
                        let ch = unsafe(character_uat(data.text, idx))
                        if ch == ' '
                            idx += 1
                            continue
                        if ch != ':'
                            insertHint = true
                        break
                    if insertHint
                        usedIndexes[endIdx] = tok._type
                        res |> emplace([[InlayHint
                            position=index_to_pos(endIdx, data.text),
                            _label=tok._type,
                            paddingLeft=true,
                            // tooltip="{tok.kind}:{tok._range}",
                            kind=InlayHintKind Type]])
            self->sendResponse(id, JV(res))
            delete usedIndexes
            delete res

    // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_rangeFormatting
    [unused_argument(id)] def rangeFormatting(id : double; var params : JsonValue ?)
        var ini <- DocumentRangeFormattingParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        self->getValidData(path) <| @ [[ <- ini]] (data : Dascript const?)
            defer_delete(ini)
            if data == null
                self->sendResponse(id, JVNull())
                return
            let startIdx = pos_to_index(ini._range.start, data.text)
            let endIdx = pos_to_index(ini._range.end, data.text)
            let sliceStr = slice(data.text, startIdx, endIdx)
            info(startIdx)
            info(endIdx)
            info(sliceStr)
            let formatted = format_source_string(sliceStr)
            info(formatted)
            var res <- [{TextEdit[]
                [[TextEdit
                    _range = Range(ini._range.start, ini._range.end),
                    newText = formatted
                ]]
            }]
            self->sendResponse(id, JV(res))
            delete res

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_prepareRename
    [unused_argument(id)] def rename(id : double; var params : JsonValue ?)
        var ini <- RenameParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        self->getValidData(path) <| @ [[ <- ini]] (data : Dascript const?)
            defer <|
                delete ini
            if data == null
                self->sendResponse(id, JVNull())
                return
            var token = self->getToken(data, path, ini.position)
            if token == null || !token._range |> range_oneline()
                self->sendResponse(id, JVNull())
                return
            // split module::name
            // split Struct : Parent
            var res <- self->getRefs(token)
            if length(res) == 0
                delete res
                self->sendResponse(id, JVNull())
                return
            let tokenLength = length(token._range)
            var edit : WorkspaceEdit
            for it in res
                if length(it._range) != tokenLength
                    continue
                var found = false
                edit.changes |> get(it.uri) <| $(edits)
                    found = true
                    edits |> emplace <| [[TextEdit _range = it._range, newText = ini.newName]]
                if !found
                    edit.changes[it.uri] <- [{auto[] [[TextEdit _range = it._range, newText = ini.newName]] }]
            self->sendResponse(id, JV(edit))
            delete res


    def add_semantic_token(idx : int; ast : AstToken?; var line, ch : int&; var res : SemanticTokens)
        if !range_oneline(ast._range) || length(ast._range) == 0
            return // TODO: multiline tokens
        var newLine = ast._range.start.line
        var newChar = ast._range.start.character
        if newLine == line
            newLine = 0
            newChar -= ch
            // if newChar < 0
            //     error("remove token. wrong char")
            //     error(ast)
            //     return
            ch += newChar
        else
            newLine -= line
            // if newLine < 0
            //     error("remove token. wrong line")
            //     error(ast)
            //     return
            line += newLine
            ch = newChar
        if newLine == 0 && newChar == 0
            // error("same token {newLine} {newChar} {length(ast._range)} {idx}")
            // error(ast)
            return // multiple tokens at one pos
        res.data |> push(newLine)
        res.data |> push(newChar)
        res.data |> push(length(ast._range))
        res.data |> push(idx)
        res.data |> push(0) // no tokens modifiers
        // info("{newLine} {newChar} {length(ast._range)} {idx}")
        // info(ast)

    def collect_semantic_tokens(ast : AstToken?; var line, ch : int&; var res : SemanticTokens)
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


    [unused_argument(id)] def semanticTokensFull(id : double; var params : JsonValue ?)
        var ini <- SemanticTokensParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        self->getValidData(path) <| @ [[ <- ini]] (data : Dascript const?)
            defer <|
                delete ini
            if data == null
                self->sendResponse(id, JVNull())
                return
            var tokens : array<AstToken?>
            data.ast |> collect_token(path) <| $(tok)
                tokens |> push(tok)
            var res : SemanticTokens
            var line, ch : int
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


    def override onData(message : uint8?; size : int)
        for i in range(size)
            var uch = unsafe(message[i])
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
                    var lenStr = unsafe(chop(str, 16, length(current_string) - length(prefix) - 2)) // \r\n length
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
                error("Json read error: {error}\n")
                return
            if msg.value is _object
                assume obj = msg.value as _object
                if obj |> key_exists("result")
                    let id = jon(msg, "id")
                    // info("> result {id} callbacks num: {length(requestCallbacks)}\n")
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
                    // info("> result after: cb num: {length(requestCallbacks)}\n")
                    if !found
                        warning("\n@@@@@@@@@@@@@@@@@@\nTODO: unhandled result: {id} \n@@@@@@@@@@@@@@@@@@\n")
                        warning(write_json(msg))

                elif obj |> key_exists("method")
                    let method = jos(msg, "method")
                    info("> method {method} id: {int64(jon(msg, "id"))}\n")
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

                    elif method == "textDocument/formatting"
                        self->formatting(jon(msg, "id"), joj(msg, "params"))

                    elif method == "textDocument/inlayHint"
                        self->inlayHint(jon(msg, "id"), joj(msg, "params"))

                    // elif method == "textDocument/rangeFormatting"
                    //     self->rangeFormatting(jon(msg, "id"), joj(msg, "params"))

                    elif method == "exit"
                        done = true

                    elif method == "shutdown"
                        done = true

                    elif method == "$/cancelRequest"
                        pass // TODO: process ignored requiests

                    elif method == "$/setTraceNotification" || method == "$/setTrace"
                        info(write_json(msg))
                        pass // ignore

                    else
                        warning("\n@@@@@@@@@@@@@@@@@@\nTODO: support: {method}\n@@@@@@@@@@@@@@@@@@\n")
                        warning(write_json(msg))
            else
                error(write_json(msg))
                error("\n@@@@@@@@@@@@@@@@@@\nError: unknown content\n@@@@@@@@@@@@@@@@@@\n")
            unsafe
                delete msg

// [export] // for dasContDev.exe
// def main(fn : string)
//     return mainLoop(fn)

[export]
def main()
    mainLoop("")

def mainLoop(fn : string)
    let args <- get_command_line_arguments()
    debug(args)
    let DEFAULT_PORT = 9000
    if get_string_arg(args, "--port", "") == ""
        error("--port argument is required, default port is {DEFAULT_PORT}")
    let port = get_int_arg(args, "--port", DEFAULT_PORT)
    let main_time = stat(fn).mtime
    info("server start: port: {port} file:{fn} ts:{main_time}\n")

    telnet = new TelnetServer()
    telnet.port = port
    telnet->make_server_adapter()

    var res = false
    with_job_que <|
        with_channel(telnet.jobsNum) <| $(outChannel)

            telnet.outChannel = outChannel

            var inscope session : smart_ptr<NetworkServer> <- gc0_restore_smart_ptr("telnet-session")
            if session != null
                info("restoring server after restart\n")
                telnet->restore(session)
                telnet->initialized()
            elif telnet->init(port)
                info("server at port {port}\n")
            else
                info("server failed to initialize\n")
                return

            let initialStrLimit = uint64(2 * 1024 * 1024)
            var strLimit = initialStrLimit
            var step = 1u
            while !telnet.done
                telnet->tick()

                telnet->invalidateQueue()

                var alloc = string_heap_bytes_allocated()
                if alloc > strLimit
                    var strReport = "STRING HEAP COLLECT: {bytes_hr(alloc)} -> "
                    var heapReport = "HEAP COLLECT: {bytes_hr(heap_bytes_allocated())} -> "
                    unsafe(heap_collect(true))
                    alloc = string_heap_bytes_allocated()
                    info("{strReport}{bytes_hr(alloc)} limit : {bytes_hr(strLimit)}\n{heapReport}{bytes_hr(heap_bytes_allocated())}")
                    if alloc > strLimit
                        strLimit *= uint64(2)
                        warning("UP string heap limit: {bytes_hr(strLimit)}\n")
                    elif strLimit > initialStrLimit && alloc < strLimit / uint64(2)
                        strLimit = strLimit / uint64(2)
                        warning("DOWN string heap limit: {bytes_hr(strLimit)}\n")

                step --
                if step == 0u
                    let h = heap_bytes_allocated()
                    let hd = heap_depth()
                    var shd = string_heap_depth()

                    step = 2000u

                    info("string heap ({shd}): {bytes_hr(alloc)}. heap ({hd}): {bytes_hr(h)}\n")

                sleep(10u)
                let new_time = stat(fn).mtime
                if new_time != main_time
                    info("restarting via source change {new_time}\n")
                    telnet->restart()
                    break
            unsafe
                delete telnet

            res = true

    return res
