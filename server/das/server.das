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
    isContDev = false
    telnet : TelnetServer?

struct Dascript
    uri : DocumentUri
    path : string
    text : string
    version : double
    completionData : Completions
    ast : AstTokens

def Dascript(var txt : TextDocumentItem)
    return <- [[Dascript uri = txt.uri, path = uri_to_path(txt.uri), version = txt.version, text = move(txt.text)]]

def Dascript(var doc : DidChangeTextDocumentParams)
    return <- [[Dascript uri = doc.textDocument.uri, path = uri_to_path(doc.textDocument.uri), version = doc.textDocument.version, text <- doc.contentChanges[0].text]]

def Dascript(var doc : DidSaveTextDocumentParams)
    return <- [[Dascript uri = doc.textDocument.uri, path = uri_to_path(doc.textDocument.uri), version = -1lf, text = move(doc.text)]]

def Dascript(var doc : HoverParams)
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
        info("server error:{code} - {msg}\n")
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
            var session : smart_ptr<NetworkServer>
            self->save(session)
            gc0_save_smart_ptr("telnet-session", session)

    def updateGlobalCompletion(force : bool)
        completionData->updateGlobalList(force)

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#responseMessage
    def sendRawData(var data : JsonValue?)
        var in scope msg = write_json(data)
        var in scope packStr = "Content-Length: {length(msg)}\r\n\r\n{msg}"
        // print("<------\n{packStr}\n-------\n")
        print("<---- send data {length(packStr)}\n") // only print!
        unsafe
            self->send(reinterpret<uint8?> packStr, length(packStr))
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
        var in scope registrationParams <- [[RegistrationParams registrations <- registrations]]
        self->sendRequest("client/registerCapability", js(registrationParams)) <| @(data, error : JsonValue?)
            if error != null
                info("error: registerCapability\n")
                info(error)
            else
                info("reg capabilities\n")
                info(data)

    def getConfig(path : string; var cb : lambda<(config : PluginConfig?):void>)
        var found = false
        configs |> find_if_exists(path) <| $(config)
            found = true
            invoke(cb, *config)
            delete cb
        if found
            return

        var localCb <- @ <| (data, error : JsonValue?)
            configReqs |> erase(path)
            if error == null & data != null & data.value is _array
                let arr & = data.value as _array
                if length(arr) > 0
                    var res <- new PluginConfig(arr[0])
                    // info("> got config {path}")
                    // info(res)
                    invoke(cb, res)
                    delete cb
                    configs[path] <- res
                    return

            error("error: config for {path}\n")
            error(error)
            configs[path] = null
            invoke(cb, null)
            delete cb

        configReqs |> find_if_exists(path) <| $(reqId)
            found = true
            self->onRequest(*reqId, localCb)
        if found
            return
        var item <- [[ConfigurationItem section = "dascript", scopeUri = path_to_uri(path) ]]
        var configParams <- [[ConfigurationParams items <- [{auto[] item }] ]]
        var reqId = self->sendRequest("workspace/configuration", js(configParams), localCb)
        configReqs[path] = reqId

    def log(msg : string; level : LogLevel)
        if !inited | done
            return
        var res = [[LogMessageParams message = msg]]
        unsafe
            res._type = reinterpret<MessageType>(level)
        self->sendNotification("window/logMessage", js(res))

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize
    def initialize(id : double; params : JsonValue?)
        // info(write_json(params))
        var in scope ini <- InitializeParams(params)
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
            documentSymbolProvider = true,
            workspaceSymbolProvider = true,
            workspace = [[ServerCapabilitiesWorkspace workspaceFolders <- workspace]]
        ]]

        var res <- [[InitializeResult capabilities <- cap, serverInfo = [[ServerInfo name = "das_server"]] ]]
        self->sendResponse(id, js(res))
        delete res

    def initialized()
        inited = true
        info("> initialized! port: {port}\n")
        self->updateGlobalCompletion(true)

        var in scope capabilities <- [{ string[] "workspace/didChangeWorkspaceFolders" }]
        if hasConfigurationCapability
            capabilities |> push("workspace/didChangeConfiguration")
        if capabilities |> length() > 0
            self->registerCapability(capabilities)

    def validate(path : string; withContent : bool = false)
        info("> validate? '{path}'\n")

        registry |> find_for_edit_if_exists(path) <| $(var data)
            info("> validate '{path}'\n")
            let uri = path_to_uri(path)
            var in scope access <- make_file_access("")
            if withContent
                set_file_source(access, path, data.text)

            compile_file(path, access) <| $(ok, program, error)
                // info(ok)
                // info(program)
                // info(error)

                if program != null
                    for it in data.ast
                        delete it
                    clear(data.ast)
                    unsafe
                        var visitor = AstData()
                        visitor.tokens = addr(data.ast)
                        var in scope adapter <- make_visitor(visitor)
                        visit(program, adapter)
                        visitor.tokens = null

                if ok
                    var diags : array<Diagnostic>
                    if program != null
                        for it in values(data.completionData)
                            delete it
                        clear(data.completionData)
                        completionData->programCompletion(program, data.completionData)

                        collect_diag(data.ast, path, diags)

                    var in scope params <- [[PublishDiagnosticsParams uri = uri, diagnostics <- diags ]]
                    self->sendNotification("textDocument/publishDiagnostics", js(params))
                    return

                if program == null
                    var diags : array<Diagnostic>
                    diags |> emplace([[Diagnostic message = "{error}", severity = DiagnosticSeverity Error, _range = [[Range]] ]])
                    var in scope params <- [[PublishDiagnosticsParams uri = uri, diagnostics <- diags ]]
                    self->sendNotification("textDocument/publishDiagnostics", js(params))
                    return
                // info("{error}\n")
                // info(program.errors)
                var map : table<string; array<Diagnostic>>
                for err in program.errors
                    // if err.at.fileInfo == null
                        // info(err)
                        // info("< error without file info\n")
                    let localUri = err.at.fileInfo != null ? path_to_uri(string(err.at.fileInfo.name)) : uri
                    let at = line_info_to_range(err.at)
                    var diag <- [[Diagnostic message = "{err.what}", _range = at, severity = DiagnosticSeverity Error, code = double(int(err.cerr)) ]]
                    if hasDiagnosticRelatedInformationCapability
                        var extra = "{err.extra}"
                        let fixme = "{err.fixme}"
                        if extra != "" | fixme != ""
                            if fixme != ""
                                extra += "\n{fixme}"
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
                    if ok | localPath != path
                        registry |> find_if_exists(localPath) <| $(fileData)
                            collect_diag(fileData.ast, localPath, v)
                    var params <- [[PublishDiagnosticsParams uri = k, diagnostics <- v ]]
                    self->sendNotification("textDocument/publishDiagnostics", js(params))

    def didOpen(id : double; params : JsonValue?)
        var in scope doc = DidOpenTextDocumentParams(params)
        let path = uri_to_path(doc.textDocument.uri)
        registry[path] <- Dascript(doc.textDocument)
        info("> open file {path} len {length(doc.textDocument.text)}, total files {length(registry)}\n")
        self->validate(path, false)

        // self->getConfig(path) <| @(config : PluginConfig?)
        //     info("didOpen: got config!!!!\n")
        //     info(config)

        // var item <- [[ConfigurationItem section = "dascript", scopeUri=doc.textDocument.uri ]]
        // var configParams <- [[ConfigurationParams items <- [{auto[] item }] ]]
        // self->sendRequest("workspace/configuration", js(configParams), true) <| @(data , error: JsonValue?)
        //     info("configuration handler")
        //     info(data)

    def didSave(id : double; params : JsonValue?)
        var in scope doc = DidSaveTextDocumentParams(params)
        let path = uri_to_path(doc.textDocument.uri)
        info("> save file {path} len {length(doc.text)}, total files {length(registry)}\n")
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
                info("> unknown? register again, total files {length(registry)}\n")
            self->validate(path, false)

    def didChange(id : double; params : JsonValue?)
        var in scope doc <- DidChangeTextDocumentParams(params)
        let path = uri_to_path(doc.textDocument.uri)
        info("> file changed? {path} changes {length(doc.contentChanges)}\n")
        if length(doc.contentChanges) > 0
            unsafe
                var data = find_for_edit(registry, path)
                if data != null
                    // delete_string(data.text) // TODO:
                    data.text <- doc.contentChanges[0].text
                    if doc.textDocument.version >= 0lf
                        data.version = doc.textDocument.version
                    info("> file changed {path} len {length(data.text)}, total files {length(registry)}\n")
                else
                    info("> file changed {path} len {length(doc.contentChanges[0].text)}, unknown? register again, total files {length(registry)}\n")
                    registry[path] <- Dascript(doc)
                // self->validate(path, true) // TODO: validate every N sec

    def didClose(id : double; params : JsonValue?)
        var doc = DidCloseTextDocumentParams(params)
        let path = uri_to_path(doc.textDocument.uri)
        find_for_edit_if_exists(registry, path) <| $(var data)
            // delete_string(data.text) // TODO:
            delete *data
        erase(registry, path)
        info("> close file {path}, total files {length(registry)}\n")

        var res <- [[PublishDiagnosticsParams uri = path_to_uri(path) ]]
        self->sendNotification("textDocument/publishDiagnostics", js(res))
        delete doc

    def hover(id : double; params : JsonValue?)
        var in scope req = HoverParams(params)
        var found = false
        let path = uri_to_path(req.textDocument.uri)
        if !registry |> key_exists(path)
            registry[path] <- Dascript(req)
            info("> [WARN] {path} unknown? register again, total files {length(registry)}\n")
            self->validate(path, false)
        find_if_exists(registry, path) <| $(var data)
            var hoversData : array<tuple<float; string>>
            var _range : Range
            var first = true
            collect_token(data.ast, path, req.position) <| $(it : AstToken?)
                if first | it._range |> in_range(_range)
                    _range = it._range
                first = false
                let ratio = float(range_ratio(it._range)) + (it.defPath == "" ? 100f : 0f)
                hoversData |> emplace([[auto ratio, to_hover(it)]])
                completionData->findCompletion(data.completionData, it._type, it.kind != AstTokenKind Function) <| $(val : Completion)
                    hoversData |> emplace([[auto ratio + 0.5, "```dascript\n" + join(val.impls, "\n") + "\n```"]])
                // TODO: filter huge tokens?
            if hoversData |> length() > 0
                hoversData |> sort() <| $(a; b)
                    return a._0 < b._0
                var hovers <- [{ for it in hoversData; it._1 }]
                var in scope h <- [[ Hover _range = _range, contents <- hovers ]]
                self->sendResponse(id, js(h))
                found = true
        if !found
            info("< [WARN] hover info not found @ {to_string(req.position)}\n")
            self->sendResponse(id, js())

    def completion(id : double; params : JsonValue?)
        var in scope ini <- CompletionParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        let data = find(registry, path)
        if data != null
            var list <- [{ for it in values(data.completionData); js(it) }]
            for val in values(completionData.globalCompletion)
                list |> emplace(js(val))
            info("< merge program completion {length(data.completionData)} with global {length(completionData.globalCompletion)} => {length(list)}\n")
            var in scope tokensList : table<string; void?>
            collect_token(data.ast, path) <| $(token : AstToken?)
                if (token.kind == AstTokenKind Constant
                  | data.completionData |> key_exists(token.name)
                  | completionData.globalCompletion |> key_exists(token.name)
                  | tokensList |> key_exists(token.name)
                )
                    return
                tokensList[token.name] = null
                list |> emplace(js({{ "label" => js(token.name); "kind" => js(double(int(CompletionItemKind Text))) }}))

            var res = js({{ "isIncomplete" => js(false); "items" => js(list)}})
            self->sendResponse(id, res)
        else
            info("< [WARN] send global completion {length(completionData.globalCompletion)}\n")
            self->sendResponse(id, js(completionData.globalCompletion))

    def resolveCompletion(id : double; var params : JsonValue?)
        var copyData := params
        self->sendResponse(id, copyData)

    def find_token(path : string; pos : Position; valid : block<(tok : AstToken?) : bool>) : AstToken?
        var token : AstToken?
        find_if_exists(registry, path) <| $(data)
            collect_token(data.ast, path, pos) <| $(tok : AstToken?)
                if (token == null | tok._range |> in_range(token._range)) & invoke(valid, tok)
                    token = tok
        return token

    def resolvePath(path : string; config : PluginConfig) : string
        let st = stat(path)
        if st.is_valid
            return path
        for it in config.project.roots
            let newPath = join_path(it, path)
            let newSt = stat(newPath)
            if newSt.is_valid
                return newPath
        return path

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_definition
    def definition(id : double; var params : JsonValue?)
        var ini <- DefinitionParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        var token = self->find_token(path, ini.position) <| $(tok : AstToken?)
            return tok.defPath != ""
        if token == null
            self->sendResponse(id, js())
            delete ini
            return
        self->getConfig(path) <| @(config : PluginConfig?)
            info("definition: got config for def!!!!\n")
            info(config)
            if config == null
                self->sendResponse(id, js())
                return
            var fixedPath = self->resolvePath(token.defPath, *config)
            info("goto {path_to_uri(fixedPath)}\n")
            var loc = [[Location uri = path_to_uri(fixedPath), _range = token.defRange]]
            self->sendResponse(id, js(loc))
            delete loc
        finally
            token = null
            delete ini

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_typeDefinition
    def typeDefinition(id : double; var params : JsonValue?)
        var in scope ini <- TypeDefinitionParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        var token = self->find_token(path, ini.position) <| $(tok : AstToken?)
            return tok.typeDefPath != ""
        if token == null
            token = self->find_token(path, ini.position) <| $(tok : AstToken?)
                return tok.defPath != ""
        if token != null
            let hasType = token.typeDefPath != ""
            var in scope loc = [[Location
                uri = path_to_uri(hasType ? token.typeDefPath : token.defPath),
                _range = hasType ? token.typeDefRange : token.defRange
            ]]
            self->sendResponse(id, js(loc))
            return
        self->sendResponse(id, js())

    def buildDocumentSymbol(ast : AstToken?; path : string; var res : array<JsonValue?>) : void
        if !ast.topLevel | ast.path != path
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
        var in scope ini <- DocumentSymbolParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        let data = find(registry, path)
        if data != null
            var res : array<JsonValue?>
            for it in data.ast
                self->buildDocumentSymbol(it, path, res)
            self->sendResponse(id, js(res))
            return
        self->sendResponse(id, js())

    def buildSymbolInformation(ast : AstToken?; query : string; limit : int; var res : array<JsonValue?>) : void
        if !ast.topLevel | length(res) >= limit | ast.name |> starts_with("builtin`")
            return
        if ast.path != "" & (query == "" | find(ast.name, query) != -1)
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
        var in scope ini <- WorkspaceSymbolParams(params)
        var res : array<JsonValue?>
        for data in values(registry)
            for it in data.ast
                self->buildSymbolInformation(it, ini.query, 300, res)
        self->sendResponse(id, js(res))

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#workspace_didChangeWorkspaceFolders
    def didChangeWorkspaceFolders(id : double; var params : JsonValue?)
        var in scope ini <- DidChangeWorkspaceFoldersParams(params)
        for it in ini.event.removed
            for i in range(0, length(workspaceFolders))
                if workspaceFolders[i].uri == it.uri
                    workspaceFolders |> erase(i)
                    break
        for it in ini.event.added
            workspaceFolders |> push(it)

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
                        // info("> content len {lenStr} as int {toRead}\n")
                        clear(current_string)
                continue

            if ch == '\n' | ch == '\r'
                continue

            push(current_string, uch)
            if length(current_string) < toRead
                continue

            toRead = 0
            var in scope error : string
            var msg = read_json(current_string, error)
            clear(current_string)
            if inited
                self->updateGlobalCompletion(false)

            if msg == null
                error("Json read error: {error}\n")
                return
            if msg.value is _object
                var obj & = msg.value as _object
                if obj |> key_exists("result")
                    let id = jon(msg, "id")
                    info("> result {id} callbacks num: {length(requestCallbacks)}\n")
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
                    info("> result after: cb num: {length(requestCallbacks)}\n")
                    if !found
                        warning("\n@@@@@@@@@@@@@@@@@@\nTODO: unhandled result: {id} \n@@@@@@@@@@@@@@@@@@\n")
                        warning(write_json(msg))

                elif obj |> key_exists("method")
                    let method = jos(msg, "method")
                    info("> method {method}\n")
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

                    elif method == "exit"
                        done = true

                    elif method == "shutdown"
                        done = true

                    elif method == "$/cancelRequest"
                        pass // ignore???

                    elif method == "$/setTraceNotification"
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

[export]
def main(fn : string)
    isContDev = true
    return mainLoop(fn)

[export]
def init()
    mainLoop("")

def mainLoop(fn : string)
    let args <- get_command_line_arguments()
    debug(args)
    let port = get_int_arg(args, "--port", 9000)
    let main_time = stat(fn).mtime
    info("server start: port: {port} file:{fn} ts:{main_time}\n")
    telnet = new TelnetServer()
    telnet.port = port

    var session : smart_ptr<NetworkServer> <- gc0_restore_smart_ptr("telnet-session")
    if session != null
        info("restoring server after restart\n")
        telnet->restore(session)
        telnet->initialized()
    elif telnet->init(port)
        info("server at port {port}\n")
    else
        info("server failed to initialize\n")
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
            info("{report}{bytes_hr(alloc)} limit : {bytes_hr(strLimit)}\n")
            if alloc > strLimit
                strLimit *= uint64(2)
                warning("UP string heap limit: {bytes_hr(strLimit)}\n")
            elif strLimit > initialstrLimit & alloc < strLimit / uint64(2)
                strLimit = strLimit / uint64(2)
                warning("DOWN string heap limit: {bytes_hr(strLimit)}\n")

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

            info("string heap ({shd}): {bytes_hr(alloc)}. heap ({hd}): {bytes_hr(h)}\n")

        sleep(0u)
        let new_time = stat(fn).mtime
        if new_time != main_time
            info("restarting via source change {new_time}\n")
            telnet->restart()
            break
    unsafe
        delete telnet

    return true
