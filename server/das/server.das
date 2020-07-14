// options log = true
// options log_nodes = true
// options log_require = true
// options heap_page = 0
// options string_heap_page = 0
// options optimize = false
// options debugger=true // required for GC
// options string_heap = 8000000
// options string_heap_page = 16384
// options heap = 8000000
// options heap_page = 16384
options persistent_heap = true

require math
require ast
require daslib/ast_boost
require strings
require daslib/strings_boost
require rtti
require network
require fio
require uriparser
require daslib/json
require json_tools
require lib
require logger
require lsp_types
require rtti_tools
require completion
require ast_token
require config

var
    telnet : TelnetServer?

struct Dascript
    uri : DocumentUri
    path : string
    text : string
    version : double
    completionData : Completions
    ast : AstTokens
    valid : bool

def Dascript(txt : TextDocumentItem)
    return <- [[Dascript uri = txt.uri, path = uri_to_path(txt.uri), version = txt.version, text = move(txt.text)]]

def Dascript(doc : DidChangeTextDocumentParams)
    return <- [[Dascript uri = doc.textDocument.uri, path = uri_to_path(doc.textDocument.uri), version = doc.textDocument.version, text = doc.contentChanges[0].text]]

def Dascript(doc : DidSaveTextDocumentParams)
    return <- [[Dascript uri = doc.textDocument.uri, path = uri_to_path(doc.textDocument.uri), version = -1lf, text = move(doc.text)]]

def Dascript(doc : HoverParams)
    return <- [[Dascript uri = doc.textDocument.uri, path = uri_to_path(doc.textDocument.uri), version = -1lf, text = ""]]

typedef
    ReqCb = lambda<(data, error : JsonValue?):void>

struct Req
    id : double
    cb : ReqCb

class TelnetServer : Server
    port : int // port number, for debug
    done : bool = false // server is done
    current_string : array<uint8> // raw input
    toRead : int = 0 // message length
    inited : bool = false // is LSP inited

    // client capabilities
    hasConfigurationCapability : bool
    hasDiagnosticRelatedInformationCapability : bool

    completionData <- CompletionData() // global completion data

    workspaceFolders : array<WorkspaceFolder> // workspace data
    registry : table<string; Dascript> // path => Dascript
    // configurations
    configs : table<string; PluginConfig?> // path => configuration
    configReqs : table<string; double> // path => configuration request

    requestId : double = 1000lf // current request id
    requestCallbacks : array<Req> // active requests

    def TelnetServer()
        Server`Server(cast<Server> self)

    def override onError(msg:string; code:int)
        info("server error:{code} - {msg}\n", get_line_info())
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

    def updateGlobalCompletion(force : bool)
        completionData->updateGlobalList(force)

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#responseMessage
    def sendRawData(var data : JsonValue?)
        var msg = write_json(data)
        var packStr = "Content-Length: {length(msg)}\r\n\r\n{msg}"
        // print("<------\n{packStr}\n-------\n")
        // print("<---- send data {length(packStr)}\n") // only print!
        unsafe
            self->send(reinterpret<uint8?> packStr, length(packStr))
    finally
        delete data

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#responseMessage
    def sendResponse(id : double; var data : JsonValue?)
        self->sendRawData(js({{ "jsonrpc" => JV("2.0"); "id" => js(id); "result" => data }}))

    def sendRequest(method : string; var data : JsonValue?; var cb : ReqCb) : double
        let reqId = requestId
        self->onRequest(reqId, cb)
        self->sendRawData(js({{ "jsonrpc" => JV("2.0"); "id" => js(reqId); "method" => js(method); "params" => data }}))
        requestId = requestId + 1lf
        return reqId

    def onRequest(id : double; var cb : ReqCb)
        requestCallbacks |> emplace([[Req id = id, cb <- cb]])

    def sendError(id : double; var data : ResponseError)
        self->sendRawData(js({{ "jsonrpc" => js("2.0"); "id" => js(id); "error" => js(data) }}))
        delete data

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#notificationMessage
    def sendNotification(method : string; var data : JsonValue?)
        self->sendRawData(js({{ "jsonrpc" => JV("2.0"); "method" => JV(method); "params" => data }}))

    def registerCapability(methods : array<string>)
        var registrations : array<Registration>
        for it in methods
            registrations |> emplace([[Registration method = it, id = it]])
        var registrationParams <- [[RegistrationParams registrations <- registrations]]
        self->sendRequest("client/registerCapability", js(registrationParams)) <| @(data, error : JsonValue?)
            if error != null
                info("error: registerCapability\n", get_line_info())
                info(error, get_line_info())
    finally
        delete registrationParams

    def getConfig(path : string; var cb : lambda<(config : PluginConfig?):void>)
        var found = configs |> find_if_exists(path) <| $(config)
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
            invoke(cb, null)
            delete cb

        found = configReqs |> find_if_exists(path) <| $(reqId)
            self->onRequest(*reqId, localCb)
        if found
            return
        var item <- [[ConfigurationItem section = "dascript", scopeUri = path_to_uri(path) ]]
        var configParams <- [[ConfigurationParams items <- [{auto[] item }] ]]
        var reqId = self->sendRequest("workspace/configuration", js(configParams), localCb)
        configReqs[path] = reqId

    def log(msg : string; level : LogLevel)
        if !inited || done
            return
        var res = [[LogMessageParams message = msg]]
        unsafe
            res._type = reinterpret<MessageType>(level)
        self->sendNotification("window/logMessage", js(res))
    finally
        delete res

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize
    def initialize(id : double; params : JsonValue?)
        // info(write_json(params))
        var ini <- InitializeParams(params)
        workspaceFolders := ini.workspaceFolders
        hasConfigurationCapability = ini.capabilities.workspace.configuration
        hasDiagnosticRelatedInformationCapability = ini.capabilities.textDocument.publishDiagnostics.relatedInformation
        // info(ini)
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
            signatureHelpProvider <- [[SignatureHelpOptions triggerCharacters <- [{ auto[] "(" }] ]],
            workspace = [[ServerCapabilitiesWorkspace workspaceFolders <- workspace]]
        ]]

        var res <- [[InitializeResult capabilities <- cap, serverInfo = [[ServerInfo name = "das_server"]] ]]
        self->sendResponse(id, js(res))
    finally
        delete ini
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

    def validate(path : string; withContent : bool = false)
        info("> validate? '{path}'\n", get_line_info())
        var valid = false
        registry |> find_for_edit_if_exists(path) <| $(var data)
            valid = data.valid
            data.valid = true
        if valid
            info("> validate? '{path}' file is valid. skip", get_line_info())
            return
        self->getConfig(path) <| @[[<-ini]](config : PluginConfig?)
            if config == null
                info("> validate? '{path}' config is null", get_line_info())
            else
                self->validate_with_config(path, withContent, *config)

    def validate_with_config(path : string; withContent : bool; config : PluginConfig)
        registry |> find_for_edit_if_exists(path) <| $(var data)
            info("> validate '{path}' project: '{config.project.file}'\n", get_line_info())
            let uri = path_to_uri(path)
            var access <- make_file_access(config.project.file)
            if withContent
                set_file_source(access, path, data.text)

            compile_file(path, access) <| $(ok, program, error)
                info("< compiled {path} {ok}", get_line_info())
                // info(error, get_line_info())

                if program != null
                    for it in data.ast
                        delete it
                    clear(data.ast)
                    // info("< visit ast {path}", get_line_info())
                    // if !ok
                    //     info(program, get_line_info())
                    unsafe
                        var visitor <- AstData()
                        visitor.tokens = addr(data.ast)
                        var adapter <- make_visitor(visitor)
                        visit(program, adapter)
                        visitor.tokens = null
                        delete adapter
                        delete visitor

                    // info("info collect tokens {path}", get_line_info())
                    collect_token(data.ast) <| $(var tok : AstToken?)
                        tok.path = resolve_path(tok.path, config)
                        if !empty(tok.defPath)
                            tok.defPath = resolve_path(tok.defPath, config)
                        if !empty(tok.typeDefPath)
                            tok.typeDefPath = resolve_path(tok.typeDefPath, config)

                if ok
                    var diags : array<Diagnostic>
                    if program != null
                        for it in values(data.completionData)
                            delete it
                        clear(data.completionData)
                        completionData->programCompletion(program, data.completionData)

                        collect_diag(data.ast, path, diags)

                    var params <- [[PublishDiagnosticsParams uri = uri, diagnostics <- diags ]]
                    self->sendNotification("textDocument/publishDiagnostics", js(params))
                    delete params
                    return

                if program == null
                    var diags : array<Diagnostic>
                    diags |> emplace([[Diagnostic message = "{error}", severity = DiagnosticSeverity Error, _range = [[Range]] ]])
                    var params <- [[PublishDiagnosticsParams uri = uri, diagnostics <- diags ]]
                    self->sendNotification("textDocument/publishDiagnostics", js(params))
                    delete params
                    return
                // info("{error}\n")
                // info(program.errors)
                var map : table<string; array<Diagnostic>>
                for err in program.errors
                    // if err.at.fileInfo == null
                        // info(err)
                        // info("< error without file info\n")
                    let localUri = resolve_path(err.at.fileInfo != null ? path_to_uri(string(err.at.fileInfo.name)) : uri, config)
                    let at = line_info_to_range(err.at)
                    var diag <- [[Diagnostic message = "{err.what}", _range = at, severity = DiagnosticSeverity Error, code = double(int(err.cerr)) ]]
                    if hasDiagnosticRelatedInformationCapability
                        if !empty(err.extra) || !empty(err.fixme)
                            var extra = "{err.extra}"
                            if !empty(err.fixme)
                                extra += "\n{err.fixme}"
                            var info = [[DiagnosticRelatedInformation location = [[Location _range = at, uri = localUri ]], message = extra ]]
                            diag.relatedInformation <- [{ auto[] info }]
                    unsafe
                        var list = find_for_edit(map, localUri)
                        if list != null
                            *list |> emplace(diag)
                        else
                            map[localUri] <- [{ auto[] diag }]

                for k, v in keys(map), values(map)
                    let localPath = uri_to_path(k)
                    if ok || localPath != path
                        registry |> find_if_exists(localPath) <| $(fileData)
                            collect_diag(fileData.ast, localPath, v)
                    var params <- [[PublishDiagnosticsParams uri = k, diagnostics <- v ]]
                    self->sendNotification("textDocument/publishDiagnostics", js(params))
                delete map
        finally
            unsafe
                delete access

    def didOpen(id : double; params : JsonValue?)
        var ini = DidOpenTextDocumentParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        registry[path] <- Dascript(ini.textDocument)
        info("> open file {path} len {length(ini.textDocument.text)}, total files {length(registry)}\n", get_line_info())
        self->validate(path, false)
    finally
        delete ini

    def didSave(id : double; params : JsonValue?)
        var doc = DidSaveTextDocumentParams(params)
        let path = uri_to_path(doc.textDocument.uri)
        info("> save file {path} len {length(doc.text)}, total files {length(registry)}\n", get_line_info())
        var version = -1lf
        unsafe
            var data = find_for_edit(registry, path)
            if data != null
                version = data.version
                if length(doc.text) > 0
                    // delete_string(data.text) // TODO:
                    data.text = doc.text
            else
                registry[path] <- Dascript(doc)
                info("> unknown? register again, total files {length(registry)}\n", get_line_info())
            self->validate(path, false)
    finally
        delete doc

    def didChange(id : double; params : JsonValue?)
        var doc <- DidChangeTextDocumentParams(params)
        let path = uri_to_path(doc.textDocument.uri)
        // info("> file changed? {path} changes {length(doc.contentChanges)}\n", get_line_info())
        if length(doc.contentChanges) > 0
            unsafe
                var data = find_for_edit(registry, path)
                if data != null
                    data.valid = false
                    // delete_string(data.text) // TODO:
                    data.text <- doc.contentChanges[0].text
                    if doc.textDocument.version >= 0lf
                        data.version = doc.textDocument.version
                    // info("> file changed {path} len {length(data.text)}, total files {length(registry)}\n", get_line_info())
                else
                    info("> file changed {path} len {length(doc.contentChanges[0].text)}, unknown? register again, total files {length(registry)}\n", get_line_info())
                    registry[path] <- Dascript(doc)
                // self->validate(path, true) // TODO: validate every N sec
    finally
        delete doc

    def didClose(id : double; params : JsonValue?)
        var doc = DidCloseTextDocumentParams(params)
        let path = uri_to_path(doc.textDocument.uri)
        find_for_edit_if_exists(registry, path) <| $(var data)
            // delete_string(data.text) // TODO:
            delete *data
        erase(registry, path)
        info("> close file {path}, total files {length(registry)}\n", get_line_info())

        var res <- [[PublishDiagnosticsParams uri = path_to_uri(path) ]]
        self->sendNotification("textDocument/publishDiagnostics", js(res))
        delete res
    finally
        delete doc

    def hover(id : double; params : JsonValue?)
        var ini = HoverParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        self->getConfig(path) <| @[[<-ini]](config : PluginConfig?)
            let verbose = config?.hovers?.verbose ?? false
            // if config != null
            //     registry |> find_if_exists(path) <| $(data)
            //         if !data.valid
            //             self->validate_with_config(path, true, *config)
            self->hover_with_config(id, move(ini), verbose)

    def hover_with_config(id : double; var ini : HoverParams; verbose : bool)
        let path = uri_to_path(ini.textDocument.uri)
        if !registry |> key_exists(path)
            registry[path] <- Dascript(ini)
            warning("> {path} unknown? register again, total files {length(registry)}\n", get_line_info())
            self->validate(path, false)
        var found = false
        find_if_exists(registry, path) <| $(var data)
            var hoversData : array<tuple<float; string>>
            var _range : Range
            var first = true
            collect_token(data.ast, path, ini.position) <| $(it : AstToken?)
                if first || it._range |> in_range(_range)
                    _range = it._range
                first = false
                let ratio = float(range_ratio(it._range)) + (empty(it.defPath) ? 100f : 0f) + (it.topLevel ? 0. : 10.)
                hoversData |> emplace([[auto ratio, to_hover(it, verbose)]])
                completionData->findCompletion(data.completionData, it._type) <| $(val : Completion)
                    hoversData |> emplace([[auto ratio + 0.5, "```dascript\n{join(val.impls, "\n")}\n```"]])
                // TODO: filter huge tokens?
            if hoversData |> length() > 0
                hoversData |> sort() <| $(a; b)
                    return a._0 < b._0
                var hovers <- [{ for it in hoversData; it._1 }]
                var h <- [[ Hover _range = _range, contents <- hovers ]]
                self->sendResponse(id, js(h))
                found = true
                delete h
        if !found
            warning("< hover info not found @ {to_string(ini.position)}\n", get_line_info())
            self->sendResponse(id, js())
        delete ini

    def completion(id : double; params : JsonValue?)
        var ini <- CompletionParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        self->getConfig(path) <| @[[<-ini]](config : PluginConfig?)
            if config == null
                warning("unknowm file {path}. Send global completion {length(completionData.globalCompletion)}\n", get_line_info())
                self->sendResponse(id, js(completionData.globalCompletion))
                delete ini
                return
            // registry |> find_if_exists(path) <| $(data)
            //     if !data.valid
            //         self->validate_with_config(path, true, *config)
            self->completion_with_config(id, move(ini), *config)

    def completion_with_config(id : double; var ini : CompletionParams; config : PluginConfig)
        let path = uri_to_path(ini.textDocument.uri)
        var found = find_if_exists(registry, path) <| $(data)
            let startTime = get_clock()
            var list : array<JsonValue?>
            let idx = pos_to_index(ini.position, data.text)
            // info("@@@@@ pos index {idx}", get_line_info())
            if idx > 0
                // var word : array<uint8>
                var searchToken = false
                var i = 0
                let maxI = min(idx, ini.position.character)
                while i < maxI && is_white_space(character_at(data.text, idx - i))
                    i++
                while i < maxI
                    let ch = character_at(data.text, idx - i)
                    // word |> push(uint8(ch))
                    if ch == '.' || ch == ':' || (ch == '>' && idx - i >= 1 && character_at(data.text, idx - i - 1) == '-')
                        searchToken = true
                        i += ch == '.' && (idx - i < 1 || character_at(data.text, idx - i - 1) != '?') ? 1 : 2
                        break
                    if (ch == 's' && idx - i >= 1 && (character_at(data.text, idx - i - 1) == 'a' || character_at(data.text, idx - i - 1) == 'i')
                        && character_at(data.text, idx - i - 2) == ' ' || (character_at(data.text, idx - i - 2) == '?' && idx - i >= 3 && character_at(data.text, idx - i - 3) == ' ')
                    )
                        searchToken = true
                        i += (character_at(data.text, idx - i - 2) == ' ' ? 3 : 4)
                        break
                    if ch == '[' || ch == '-'
                        break
                    i++
                while i < maxI && is_white_space(character_at(data.text, idx - i))
                    i++
                if searchToken
                    var insideO = 0
                    var insideD = 0
                    while i < maxI
                        let ch = character_at(data.text, idx - i)
                        if ch == ')'
                            insideO ++
                        elif ch == ']'
                            insideD ++
                        elif ch == '('
                            if insideO > 0
                                insideO --
                            else
                                i ++
                                break
                        elif ch == '['
                            if insideD > 0
                                insideD --
                            else
                                i ++
                                break

                        if insideO > 0 || insideD > 0
                            i ++
                            continue
                        i ++
                        break
                    var pos = Position(ini.position.line, ini.position.character - i)
                    // let search = reverse(string(word))
                    // info("@@@@ search '{search}' {pos}", get_line_info())
                    // info("> validate '{path}' project: '{config.project.file}'\n", get_line_info())
                    let uri = path_to_uri(path)
                    var access <- make_file_access(config.project.file)
                    set_file_source(access, path, data.text)

                    compile_file(path, access) <| $(ok, program, error)
                        if program != null
                            unsafe
                                var visitor <- AstCompletionData()
                                visitor.path = path
                                visitor.pos = pos
                                var adapter <- make_visitor(visitor)
                                visit(program, adapter)

                                for it in visitor.res
                                    list |> emplace(js(it))

                                delete adapter
                                delete visitor
                    delete pos
                    unsafe
                        delete access

            if list |> length() == 0
                for it in values(data.completionData)
                    list |> emplace(js(it))
                for val in values(completionData.globalCompletion)
                    list |> emplace(js(val))
                info("< merge program completion {length(data.completionData)} with global {length(completionData.globalCompletion)} => {length(list)}\n", get_line_info())
                var tokensList : table<string; void?>
                collect_token(data.ast, path) <| $(token : AstToken?)
                    if (token.kind == AstTokenKind Constant
                        || data.completionData |> key_exists(token.name)
                        || completionData.globalCompletion |> key_exists(token.name)
                        || tokensList |> key_exists(token.name)
                    )
                        return
                    tokensList[token.name] = null
                    list |> emplace(js({{ "label" => js(token.name); "kind" => js(int(CompletionItemKind Text)) }}))
                delete tokensList

            var res <- js({{ "isIncomplete" => js(false); "items" => js(list)}})
            self->sendResponse(id, res)
            info("< completion time { get_clock() - startTime }", get_line_info())

        if !found
            warning("< send global completion {length(completionData.globalCompletion)}\n", get_line_info())
            self->sendResponse(id, js(completionData.globalCompletion))
    finally
        delete ini

    def resolveCompletion(id : double; var params : JsonValue?)
        var err : string
        var copyData = read_json(write_json(params), err)
        self->sendResponse(id, copyData)

    def find_token(path : string; pos : Position; valid : block<(tok : AstToken?) : bool>) : AstToken?
        var token : AstToken?
        find_if_exists(registry, path) <| $(data)
            var minRatio = FLT_MAX
            collect_token(data.ast, path, pos) <| $(tok : AstToken?)
                if !invoke(valid, tok)
                    return
                let ratio = float(range_ratio(tok._range)) + (empty(tok.defPath) ? 100f : 0f) + (tok.topLevel ? 0. : 10.)
                if token == null || ratio < minRatio
                    minRatio = ratio
                    token = tok
        return token

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_definition
    def definition(id : double; var params : JsonValue?)
        var ini <- DefinitionParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        // registry |> find_if_exists(path) <| $(data)
        //     if !data.valid
        //         self->validate(path, true)
        var token = self->find_token(path, ini.position) <| $(tok : AstToken?)
            return !empty(tok.defPath)
        if token == null
            self->sendResponse(id, js())
            return
        var loc = [[Location uri = path_to_uri(token.defPath), _range = token.defRange]]
        self->sendResponse(id, js(loc))
        delete loc
    finally
        delete ini

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_typeDefinition
    def typeDefinition(id : double; var params : JsonValue?)
        var ini <- TypeDefinitionParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        // registry |> find_if_exists(path) <| $(data)
        //     if !data.valid
        //         self->validate(path, true)
        var token = self->find_token(path, ini.position) <| $(tok : AstToken?)
            return !empty(tok.typeDefPath)
        if token == null
            token = self->find_token(path, ini.position) <| $(tok : AstToken?)
                return !empty(tok.defPath)
        if token == null
            self->sendResponse(id, js())
            return
        let hasType = !empty(token.typeDefPath)
        var defPath = hasType ? token.typeDefPath : token.defPath
        let _range & = hasType ? token.typeDefRange : token.defRange
        var loc = [[Location uri = path_to_uri(defPath), _range = _range ]]
        self->sendResponse(id, js(loc))
        delete loc
    finally
        delete ini

    def buildDocumentSymbol(ast : AstToken?; path : string; var res : array<JsonValue?>) : void
        if !ast.topLevel || ast.path != path
            return
        var data <- {{
            "name" => js(ast.name |> strip_left("`"));
            "detail" => js(ast._type);
            "kind" => js(int(ast.kind));
            "range" => js(ast._range);
            "selectionRange" => js(ast._range)
        }}
        if ast.children |> length() > 0
            var children : array<JsonValue?>
            for it in ast.children
                self->buildDocumentSymbol(it, path, children)
            if children |> length() > 0
                data["children"] <- js(children)
        res |> emplace(js(data))

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_documentSymbol
    def documentSymbol(id : double; var params : JsonValue?)
        var ini <- DocumentSymbolParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        // registry |> find_if_exists(path) <| $(data)
        //     if !data.valid
        //         self->validate(path, true)
        self->getConfig(path) <| @[[<-ini]](config : PluginConfig?) // wait first validation
            let data = find(registry, path)
            if data == null
                self->sendResponse(id, js())
            else
                var res : array<JsonValue?>
                for it in data.ast
                    self->buildDocumentSymbol(it, path, res)
                self->sendResponse(id, js(res))
            delete ini

    def buildSymbolInformation(ast : AstToken?; query : string; limit : int; var res : array<JsonValue?>) : void
        if !ast.topLevel || length(res) >= limit || ast.name |> starts_with("builtin`")
            return
        if !empty(ast.path) && (empty(query) || find(ast.name, query) != -1)
            var data <- {{
                "name" => js(ast.name);
                "containerName" => js(ast.desc);
                "kind" => js(int(ast.kind));
                "location" => js({{ "range" => js(ast._range); "uri" => js(path_to_uri(ast.path)) }})
            }}
            res |> emplace(js(data))
        for it in ast.children
            self->buildSymbolInformation(it, query, limit, res)

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#workspace_symbol
    def workspaceSymbol(id : double; var params : JsonValue?)
        var ini <- WorkspaceSymbolParams(params)
        var res : array<JsonValue?>
        for data in values(registry)
            for it in data.ast
                self->buildSymbolInformation(it, ini.query, 300, res)
        self->sendResponse(id, js(res))
        delete ini

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#workspace_didChangeWorkspaceFolders
    def didChangeWorkspaceFolders(id : double; var params : JsonValue?)
        var ini <- DidChangeWorkspaceFoldersParams(params)
        for it in ini.event.removed
            for i in range(0, length(workspaceFolders))
                if workspaceFolders[i].uri == it.uri
                    workspaceFolders |> erase(i)
                    break
        for it in ini.event.added
            workspaceFolders |> push(it)
        delete ini

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#workspace_didChangeConfiguration
    def didChangeConfiguration(id : double; var params : JsonValue?)
        for it in values(configs)
            delete it
        clear(configs)

    def addUniqueReference(var res : array<Location>; _range : Range; uri : string) : void
        for it in res
            if it.uri == uri && it._range == _range
                return
        res |> emplace([[Location _range = _range, uri = uri ]])

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_references
    def references(id : double; var params : JsonValue?)
        var ini <- ReferenceParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        // registry |> find_if_exists(path) <| $(data)
        //     if !data.valid
        //         self->validate(path, true)
        var token = self->find_token(path, ini.position) <| $(tok : AstToken?)
            return !empty(tok.defPath) && !is_zero(tok.defRange)
        if token == null
            token = self->find_token(path, ini.position) <| $(tok : AstToken?)
                return tok != null
        if token == null
            self->sendResponse(id, js())
            return
        var res : array<Location>
        for data in values(registry)
            var addDef = false
            if !empty(token.defPath)
                // same references
                collect_ref_token(data.ast, token.defPath, token.defRange) <| $(tok : AstToken?)
                    addDef = true
                    self->addUniqueReference(res, tok._range, path_to_uri(tok.path))
                if addDef
                    self->addUniqueReference(res, token.defRange, path_to_uri(token.defPath))
            // refs to this token
            collect_ref_token(data.ast, token.path, token._range) <| $(tok : AstToken?)
                self->addUniqueReference(res, tok._range, path_to_uri(tok.path))
            if !addDef
                self->addUniqueReference(res, token._range, path_to_uri(token.path))
        var jsRes : array<json::JsonValue?>
        for it in res
            jsRes |> emplace(js(it))
        self->sendResponse(id, js(jsRes))
        delete res
    finally
        delete ini

    def signatureHelp(id : double; var params : JsonValue?)
        var ini <- SignatureHelpParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        self->getConfig(path) <| @[[<-ini]](config : PluginConfig?)
            if config == null
                warning("unknowm file {path}. Send global completion {length(completionData.globalCompletion)}\n", get_line_info())
                self->sendResponse(id, js(completionData.globalCompletion))
                delete ini
                return
            // registry |> find_if_exists(path) <| $(data)
            //     if !data.valid
            //         self->validate_with_config(path, true, *config)
            self->signatureHelp_with_config(id, move(ini), config)

    def signatureHelp_with_config(id : double; var ini : SignatureHelpParams; config : PluginConfig?)
        let path = uri_to_path(ini.textDocument.uri)
        var found = false
        find_if_exists(registry, path) <| $(data)
            let idx = pos_to_index(ini.position, data.text)
            // info("@@@@@ pos index {idx}", get_line_info())
            if idx > 0
                var list : array<SignatureInformation>
                // var word : array<uint8>
                var searchToken = false
                var i = 0
                let maxI = min(idx, ini.position.character)
                while i < maxI
                    var insideO = 0
                    while i < maxI
                        let ch = character_at(data.text, idx - i)
                        // word |> push(uint8(ch))
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
                    while i < maxI && is_white_space(character_at(data.text, idx - i))
                        i++
                    if searchToken
                        var pos = Position(ini.position.line, ini.position.character - i)
                        var token = self->find_token(path, pos) <| $(tok : AstToken?)
                            return tok != null
                        if token != null && (token.kind == AstTokenKind Function || token.kind == AstTokenKind Method || token.kind == AstTokenKind Operator)
                            completionData->findCompletion(data.completionData, token._type) <| $(val : Completion)
                                for it in val.impls
                                    list |> emplace([[SignatureInformation _label = it]])
                            if list |> length() == 0
                                list |> emplace([[SignatureInformation _label = token.desc]])
                        if list |> length() > 0
                            self->sendResponse(id, js([[SignatureHelp signatures <- list ]]))
                            found = true
                            break
        if !found
            self->sendResponse(id, js())
    finally
        delete ini

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_documentColor
    def documentColor(id : double; var params : JsonValue?)
        var ini <- DocumentColorParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        var found = false
        find_if_exists(registry, path) <| $(data)
            var res : array<ColorInformation>
            var line = 0
	        var charPos = 0
            var i = 0
            var step = -1
            var currentNum : array<int>
            for i in range(0, length(data.text))
                let ch = character_at(data.text, i)
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
                var arr <- [{ for it in res; js(it)}]
                self->sendResponse(id, js(arr))
            delete res
        if !found
            self->sendResponse(id, js())
    finally
        delete ini

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_colorPresentation
    def colorPresentation(id : double; var params : JsonValue?)
        var ini <- ColorPresentationParams(params)
        let c & = ini.color
        let len = length(ini._range)
        let color = build_string() <| $(var str)
            write(str, "0x")
            if len > 8
                format(str, "%02X", int(c.alpha * 255lf))
            format(str, "%02X", int(c.red * 255lf))
            format(str, "%02X", int(c.green * 255lf))
            format(str, "%02X", int(c.blue * 255lf))

        var res <- [[ColorPresentation _label = color]]
        self->sendResponse(id, js([{ auto[] js(res) }]))
    finally
        delete ini

    def override onData(message:uint8?; size:int)
        for i in range(0,size)
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
            if inited
                self->updateGlobalCompletion(false)

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

                    elif method == "exit"
                        done = true

                    elif method == "shutdown"
                        done = true

                    elif method == "$/cancelRequest"
                        pass // ignore???

                    elif method == "$/setTraceNotification"
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

[export,init]
def init()
    mainLoop("")

def mainLoop(fn : string)
    let args <- get_command_line_arguments()
    debug(args)
    let port = get_int_arg(args, "--port", 9000)
    let main_time = stat(fn).mtime
    info("server start: port: {port} file:{fn} ts:{main_time}\n", get_line_info())
    telnet = new TelnetServer()
    telnet.port = port

    var session : smart_ptr<NetworkServer> <- gc0_restore_smart_ptr("telnet-session")
    if session != null
        info("restoring server after restart\n", get_line_info())
        telnet->restore(session)
        telnet->initialized()
    elif telnet->init(port)
        info("server at port {port}\n", get_line_info())
    else
        info("server failed to initialize\n", get_line_info())
        return false

    logger::onLog <- @ <| (msg : string; level : LogLevel)
        if telnet != null
            telnet->log(msg, level)

    let initialstrLimit = uint64(2 * 1024 * 1024)
    var strLimit = initialstrLimit
    var step = 1u
    while !telnet.done
        telnet->tick()

        var alloc = string_heap_bytes_allocated()
        if alloc > strLimit
            // string_heap_report()
            var report = "STRING HEAP COLLECT: {bytes_hr(alloc)} -> "
            unsafe
                string_heap_collect()
            alloc = string_heap_bytes_allocated()
            info("{report}{bytes_hr(alloc)} limit : {bytes_hr(strLimit)}\n", get_line_info())
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

            step = 500u
            // if h > 15000u
            // info("------------------\n")
            // heap_report()
            // info("------------------\n")
            // string_heap_report()
            // info("------------------\n")
            // step = 100u

            info("string heap ({shd}): {bytes_hr(alloc)}. heap ({hd}): {bytes_hr(h)}\n", get_line_info())

        sleep(0u)
        let new_time = stat(fn).mtime
        if new_time != main_time
            info("restarting via source change {new_time}\n", get_line_info())
            telnet->restart()
            break
    unsafe
        delete telnet

    return true
