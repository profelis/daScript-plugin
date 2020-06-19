// options log = true
// options log_nodes = true
// options log_require = true
// options heap_page = 0
// options string_heap_page = 0
// options optimize = false
// options debugger=true // required for GC
options string_heap = 8000000
options string_heap_page = 16384
options heap = 8000000
options heap_page = 16384

require ast
require daslib/ast_boost
require daslib/strings
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

var
    isContDev = false
    telnet : TelnetServer?

enum AstTokenKind
    File = 1
    Module = 2
    Namespace = 3
    Package = 4
    Class = 5
    Method = 6
    Property = 7
    Field = 8
    Constructor = 9
    Enum = 10
    Interface = 11
    Function = 12
    Variable = 13
    Constant = 14
    String = 15
    Number = 16
    Boolean = 17
    Array = 18
    Object = 19
    Key = 20
    Null = 21
    EnumMember = 22
    Struct = 23
    Event = 24
    Operator = 25
    TypeParameter = 26


bitfield AstTokenWarn
    unused
    deprecated

struct AstToken
    topLevel : bool
    _range : Range
    path : string

    name : string
    desc : string
    kind : AstTokenKind
    _type : string
    warn : AstTokenWarn
    children : array<AstToken?>

    defRange : Range
    defPath : string

    typeDefRange : Range
    typeDefPath : string

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

class TelnetServer : Server
    port : int // port number, for debug
    done : bool = false // server is done
    current_string : array<uint8> // raw input
    toRead : int = 0

    inited : bool = false // is LSP inited

    completionData <- CompletionData()
    registry : table<string; Dascript> // path => Dascript

    hasConfigurationCapability : bool
    hasDiagnosticRelatedInformationCapability : bool

    def TelnetServer
        Server`Server(cast<Server> self)

    def override onError(msg:string; code:int)
        info("server error:{code} - {msg}\n")
        done = true

    def override onConnect
        info("connected\n")

    def override onDisconnect
        if length(current_string) > 0
            info(string(current_string))
            clear(current_string)
        info("disconnected\n")

    def restart
        if _server != null
            var session : smart_ptr<NetworkServer>
            self->save(session)
            gc0_save_smart_ptr("telnet-session", session)

    def updateGlobalCompletion(force : bool)
        completionData->updateGlobalList(force)

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#responseMessage
    def sendRawResponse(var data : JsonValue?; free = true)
        var in scope msg = write_json(data)
        var in scope packStr = "Content-Length: {length(msg)}\r\n\r\n{msg}"
        // print("<------\n{packStr}\n-------\n")
        print("<---- send data {length(packStr)}\n") // only print!
        unsafe
            self->send(reinterpret<uint8?> packStr, length(packStr))
        if free
            delete data

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#responseMessage
    def sendResponse(id : double; var data : JsonValue?; free = true)
        self->sendRawResponse(js({{ "jsonrpc" => JV("2.0"); "id" => js(id); "result" => data }}), free)

    def sendError(id : double; var data : ResponseError; free = true)
        self->sendRawResponse(js({{ "jsonrpc" => js("2.0"); "id" => js(id); "error" => js(data) }}), free)
        if free
            delete data

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#notificationMessage
    def sendNotification(method : string; var data : JsonValue?; free = true)
        self->sendRawResponse(js({{ "jsonrpc" => JV("2.0"); "method" => JV(method); "params" => data }}), free)

    def log(msg : string; level : LogLevel)
        if inited & !done
            unsafe
                var res = [[LogMessageParams message = msg, _type = reinterpret<MessageType>(level)]]
                self->sendNotification("window/logMessage", js(res), true)

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize
    def initialize(id : double; params : JsonValue?)
        // info(write_json(params))
        var in scope ini <- InitializeParams(params)
        hasConfigurationCapability = ini.capabilities.workspace.configuration
        hasDiagnosticRelatedInformationCapability = ini.capabilities.textDocument.publishDiagnostics.relatedInformation
        // info(ini)
        var cmpl <- [[CompletionOptions resolveProvider = true, triggerCharacters <- [{ auto[] "." }] ]]
        var cap <- [[ServerCapabilities
            textDocumentSync = TextDocumentSyncKind Full,
            completionProvider <- cmpl,
            hoverProvider = true,
            definitionProvider = true,
            typeDefinitionProvider = true,
            documentSymbolProvider = true,
            workspaceSymbolProvider = true
        ]]

        var res <- [[InitializeResult capabilities <- cap, serverInfo = [[ServerInfo name = "das_server"]] ]]
        self->sendResponse(id, js(res), true)
        delete res

    def initialized()
        inited = true
        info("> initialized! port: {port}\n")
        self->updateGlobalCompletion(true)

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
                    var in scope visitor = AstData()
                    unsafe
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
                    self->sendNotification("textDocument/publishDiagnostics", js(params), true)
                    return

                if program == null
                    var diags : array<Diagnostic>
                    diags |> emplace([[Diagnostic message = "{error}", severity = DiagnosticSeverity Error, _range = [[Range]] ]])
                    var in scope params <- [[PublishDiagnosticsParams uri = uri, diagnostics <- diags ]]
                    self->sendNotification("textDocument/publishDiagnostics", js(params), true)
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
                        registry |> find_if_exists(localPath) <| $(data)
                            collect_diag(data.ast, localPath, v)
                    var params <- [[PublishDiagnosticsParams uri = k, diagnostics <- v ]]
                    self->sendNotification("textDocument/publishDiagnostics", js(params), true)

    def didOpen(id : double; params : JsonValue?)
        var in scope doc = DidOpenTextDocumentParams(params)
        let path = uri_to_path(doc.textDocument.uri)
        registry[path] <- Dascript(doc.textDocument)
        info("> open file {path} len {length(doc.textDocument.text)}, total files {length(registry)}\n")
        self->validate(path, false)

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
        var in scope doc = DidCloseTextDocumentParams(params)
        let path = uri_to_path(doc.textDocument.uri)
        find_for_edit_if_exists(registry, path) <| $(var data)
            // delete_string(data.text) // TODO:
            delete *data
        erase(registry, path)
        info("> close file {path}, total files {length(registry)}\n")

        var params <- [[PublishDiagnosticsParams uri = path_to_uri(path) ]]
        self->sendNotification("textDocument/publishDiagnostics", js(params), true)

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
                if first
                    _range = it._range
                    first = false
                elif it._range |> in_range(_range)
                    _range = it._range
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
                self->sendResponse(id, js(h), true)
                found = true
        if !found
            info("< [WARN] hover info not found @ {to_string(req.position)}\n")
            self->sendResponse(id, js(), true)

    def completion(id : double; params : JsonValue?)
        var in scope params <- CompletionParams(params)
        info(params)
        let path = uri_to_path(params.textDocument.uri)
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
            self->sendResponse(id, res, true)
        else
            info("< [WARN] send global completion {length(completionData.globalCompletion)}\n")
            self->sendResponse(id, js(completionData.globalCompletion), true)

    def resolveCompletion(id : double; var params : JsonValue?)
        self->sendResponse(id, params, false)

    def find_token(path : string; pos : Position; valid : block<(tok : AstToken?) : bool>) : AstToken?
        let data = find(registry, path)
        if data != null
            var token : AstToken?
            collect_token(data.ast, path, pos) <| $(tok : AstToken?)
                if invoke(valid, tok) & (token == null | tok._range |> in_range(token._range))
                    token = tok
            return token
        return null

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_definition
    def definition(id : double; var params : JsonValue?)
        var in scope params <- DefinitionParams(params)
        let path = uri_to_path(params.textDocument.uri)
        var token = self->find_token(path, params.position) <| $(tok : AstToken?)
            return tok.defPath != ""
        if token != null
            info(token)
            var in scope loc = [[Location uri = path_to_uri(token.defPath), _range = token.defRange]]
            info(loc)
            self->sendResponse(id, js(loc), true)
            return
        self->sendResponse(id, JV(null), true)

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_typeDefinition
    def typeDefinition(id : double; var params : JsonValue?)
        var in scope params <- TypeDefinitionParams(params)
        let path = uri_to_path(params.textDocument.uri)
        var token = self->find_token(path, params.position) <| $(tok : AstToken?)
            return tok.typeDefPath != ""
        if token == null
            token = self->find_token(path, params.position) <| $(tok : AstToken?)
                return tok.defPath != ""
        if token != null
            info(token)
            let hasType = token.typeDefPath != ""
            var in scope loc = [[Location
                uri = path_to_uri(hasType ? token.typeDefPath : token.defPath),
                _range = hasType ? token.typeDefRange : token.defRange
            ]]
            info(loc)
            self->sendResponse(id, js(loc), true)
            return
        self->sendResponse(id, JV(null), true)

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
        let ini <- DocumentSymbolParams(params)
        let path = uri_to_path(ini.textDocument.uri)
        let data = find(registry, path)
        if data != null
            var res : array<JsonValue?>
            for it in data.ast
                self->buildDocumentSymbol(it, path, res)
            self->sendResponse(id, js(res), true)
            return
        self->sendResponse(id, JV(null), true)

    def buildSymbolInformation(ast : AstToken?; query : string; var res : array<JsonValue?>) : void
        if !ast.topLevel
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
            self->buildSymbolInformation(it, query, res)

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#workspace_symbol
    def workspaceSymbol(id : double; var params : JsonValue?)
        let ini <- WorkspaceSymbolParams(params)
        var res : array<JsonValue?>
        for data in values(registry)
            for it in data.ast
                self->buildSymbolInformation(it, ini.query, res)
        self->sendResponse(id, js(res), true)

    def override onData(msg:uint8?; size:int)
        for i in range(0,size)
            unsafe
                let uch = msg[i]
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
                            info("> content len {lenStr} as int {toRead}\n")
                            clear(current_string)
                    continue

                if ch == '\n' | ch == '\r'
                    continue

                push(current_string, uch)
                if toRead == length(current_string)
                    toRead = 0
                    // info(">------\n{string(current_string)}\n------\n")
                    var in scope error : string
                    var in scope msg = read_json(current_string, error)
                    // info(msg)
                    if msg == null
                        info("Json read error: {error}\n")
                    else
                        if msg.value is _object
                            let method = jos(msg, "method")
                            info("method {method}\n")
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
                                info("\n@@@@@@@@@@@@@@@@@@\nTODO: support: {method}\n@@@@@@@@@@@@@@@@@@\n")
                                info(write_json(msg))
                        else
                            info(write_json(msg))
                            info("\n@@@@@@@@@@@@@@@@@@\nError: unknown content\n@@@@@@@@@@@@@@@@@@\n")
                    clear(current_string)
                    if inited
                        self->updateGlobalCompletion(false)
            // info(current_string)


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

    logger::onLog = @ <| (msg : string; level : LogLevel)
        if telnet != null
            telnet->log(msg, level)

    var strLimit = 2u * 1024u * 1024u
    var step = 1u
    while !telnet.done
        telnet->tick()

        var alloc = string_heap_bytes_allocated()
        if alloc > strLimit
            // string_heap_report()
            info("STRING HEAP FREE: {bytes_hr(alloc)}\n")
            unsafe
                string_heap_collect()
            alloc = string_heap_bytes_allocated()
            if alloc > strLimit
                strLimit += strLimit
                warning("UPDATE STRING HEAP LIMIT TO {bytes_hr(strLimit)}\n")
            info("STRING HEAP FREE after: {bytes_hr(alloc)}\n")

        step --
        if step == 0u
            let h = heap_bytes_allocated()
            let hd = heap_depth()
            var shd = string_heap_depth()
            info("str heap: {bytes_hr(alloc)} str heap depth : {shd} heap : {bytes_hr(h)} heapDepth : {hd}\n")

            step = 500u
            // if h > 15000u
            // info("------------------\n")
            // heap_report()
            // info("------------------\n")
            // string_heap_report()
            // info("------------------\n")
            // step = 100u

        sleep(0u)
        let new_time = stat(fn).mtime
        if new_time != main_time
            info("restarting via source change {new_time}\n")
            telnet->restart()
            break
    unsafe
        delete telnet

    return true

def to_hover(val : AstToken?)
    var res = "```dascript\n{val.desc}\nname: {val.name}\ntype: {val._type}\nkind: {val.kind}\n{val.path} @ {to_string(val._range)}\n{val.defPath} @ {to_string(val.defRange)}\n{val.typeDefPath} @ {to_string(val.typeDefRange)}\n```"
    return <- res

typedef
    AstTokens = array<AstToken?>

def collect_diag(val : AstToken?; path : string; var diag : array<Diagnostic>)
    if val.path == path & val.warn != bitfield(0)
        var d <- [[Diagnostic _range = val._range ]]
        if val.warn.unused
            d.message = "[Unused] {val.desc}"
            d.severity = DiagnosticSeverity Information
        else
            d.message = "[Deprecated] {val.desc}"
            d.severity = DiagnosticSeverity Warning
        diag |> emplace(d)
    for it in val.children
        collect_diag(it, path, diag)

def collect_diag(val : AstTokens; path : string; var diag : array<Diagnostic>)
    for it in val
        collect_diag(it, path, diag)

def collect_token(val : AstToken?; path : string; blk : block<(token:AstToken?):void>)
    if val.path == path
        invoke(blk, val)
    for it in val.children
        collect_token(it, path, blk)

def collect_token(val : AstTokens; path : string; blk : block<(token:AstToken?):void>)
    for it in val
        collect_token(it, path, blk)

def collect_token(val : AstToken?; path : string; pos : Position; blk : block<(token:AstToken?):void>)
    if val.path == path & pos |> in_range(val._range)
        invoke(blk, val)
    for it in val.children
        collect_token(it, path, pos, blk)

def collect_token(val : AstTokens; path : string; pos : Position; blk : block<(token:AstToken?):void>)
    for it in val
        collect_token(it, path, pos, blk)

def module_name(mod)
    if mod == null
        return ""
    let name = "{mod.name}"
    return length(name) == 0 ? "" : "{name}::"

def set_token_path(var token : AstToken?; at)
    token._range = line_info_to_range(at)
    token.path = at.fileInfo != null ? "{at.fileInfo.name}" : ""

def set_token_def(var token : AstToken?; at)
    token.defRange = line_info_to_range(at)
    token.defPath = at.fileInfo != null ? "{at.fileInfo.name}" : ""

def set_token_type_def(var token : AstToken?; typ)
    if typ != null
        token.typeDefRange = line_info_to_range(typ.at)
        token.typeDefPath = typ.at.fileInfo != null ? "{typ.at.fileInfo.name}" : ""

def var_to_token(arg; blk : block<(var token:AstToken?):void>)
    if arg == null | arg.flags.generated
        return false
    let typ = arg._type != null ? "{module_name(arg._module)}{describe([decl=arg._type,extra=true])}" : ""
    var desc = "{arg.name} : {typ}"
    if !arg._type.flags.constant
        desc = "var " + desc
    if arg.init != null
        desc += " = {describe(arg.init)}"
    var token <- new [[AstToken
        name = "{arg.name}",
        desc = desc,
        kind = AstTokenKind Variable,
        _type = typ,
        warn = arg.isAccessUnused ? AstTokenWarn unused : bitfield(0)
    ]]
    set_token_path(token, arg.at)
    set_token_def(token, arg.at)
    if arg._type != null & arg._type.structType != null
        set_token_type_def(token, arg._type.structType)
    invoke(blk, token)
    return true


def field_to_token(expr; blk : block<(var token:AstToken?):void>)
    if expr == null
        return false
    let typ = expr._type != null ? "{describe([decl=expr._type,extra=true])}" : ""
    var name = "{expr.name}"
    var desc = "{name} : {typ}"
    if expr.field != null & expr.field.init != null
        desc = "{desc} = {describe(expr.field.init)}"
    var res <- new [[AstToken
        name = name,
        desc = desc,
        kind = AstTokenKind Field,
        _type = typ
    ]]
    set_token_path(res, expr.atField)
    if expr.field != null
        set_token_def(res, expr.field.at)
        if expr.field._type != null & expr.field._type.structType != null
            set_token_type_def(res, expr.field._type.structType)
    if range_oneline(res._range) & typ |> starts_with("function<")
        res.kind = AstTokenKind Method
        let selfLen = 4
        res._range.start.character = res._range.start.character > selfLen ? res._range.start.character - selfLen : res._range.start.character
        res._range.end.character += length(res.name)
    invoke(blk, res)
    return true


def like_call_to_token(expr; blk : block<(var token:AstToken?):void>)
    if expr == null
        return false
    // info(expr._type)
    var typ = "def {expr.name}"
    var firstArg : string
    if expr.arguments.length > 0
        typ += "("
        var idx = 1
        for arg in expr.arguments
            if idx == 1
                firstArg = describe(arg)
            else
                typ += ", "
            // if arg is ExprMakeBlock
                // var blk & = reinterpret<smart_ptr<ExprMakeBlock>>(arg)
                // info(blk)
                // typ += "__blk__"
                // continue
            if arg._type != null & !arg._type.flags.constant
                typ += "var "
            typ += "arg{idx} : {arg._type != null ? describe([decl=arg._type,extra=true]) : "" }"
            idx++
        typ += ")"
    // if firstArg |> starts_with("self.")
    //     return false
    if expr._type != null
        typ += " : {describe([decl=expr._type,extra=true])}"
    var res <- new [[AstToken
        name = "{expr.name}",
        desc = typ,
        kind = AstTokenKind Function,
        _type = typ
    ]]
    set_token_path(res, expr.at)
    if range_oneline(res._range) & firstArg |> starts_with("self.")
        let selfLen = 4
        res._range.start.character = res._range.start.character > selfLen ? res._range.start.character - selfLen : res._range.start.character
        res._range.end.character += length(firstArg) - selfLen - 1
    invoke(blk, res)
    return true

def call_to_token(expr; blk : block<(var token:AstToken?):void>)
    if expr == null
        return false
    // info(expr._type)
    var typ = "def {expr.name}"
    var firstArg : string
    if expr.arguments.length > 0
        typ += "("
        var idx = 1
        for arg in expr.arguments
            if idx == 1
                firstArg = describe(arg)
            else
                typ += ", "
            // if arg is ExprMakeBlock
                // var blk & = reinterpret<smart_ptr<ExprMakeBlock>>(arg)
                // info(blk)
                // typ += "__blk__"
                // continue
            if arg._type != null & !arg._type.flags.constant
                typ += "var "
            typ += "arg{idx} : {arg._type != null ? describe([decl=arg._type,extra=true]) : "" }"
            idx++
        typ += ")"
    // if firstArg |> starts_with("self.")
    //     return false
    if expr._type != null
        typ += " : {describe([decl=expr._type,extra=true])}"
    var res <- new [[AstToken
        name = "{expr.name}",
        desc = typ,
        kind = AstTokenKind Function,
        _type = typ
    ]]
    set_token_path(res, expr.at)
    if expr.func != null
        set_token_def(res, expr.func.atDecl)
    if range_oneline(res._range) & firstArg |> starts_with("self.")
        let selfLen = 4
        res._range.start.character = res._range.start.character > selfLen ? res._range.start.character - selfLen : res._range.start.character
        res._range.end.character += length(firstArg) - selfLen - 1
    invoke(blk, res)
    return true

def type_to_token(typ; blk : block<(var token:AstToken?):void>)
    if typ == null
        return false
    let typeName = describe([decl=typ,extra=true])
    var tok <- new [[AstToken
        name = typeName,
        desc = typeName,
        kind = AstTokenKind Struct,
        _type = typeName
    ]]
    set_token_path(tok, typ.at)
    if typ.structType != null
        set_token_def(tok, typ.structType.at)
    invoke(blk, tok)
    return true

class AstData : AstVisitor
    tokens : AstTokens?
    skipExprs : bool = false
// program
    // def override preVisitProgram(prog:ProgramPtr) : void
    // def override visitProgram(porg:ProgramPtr) : void
    // def override preVisitProgramBody(prog:ProgramPtr) : void
// type
    def override preVisitTypeDecl(typ:TypeDeclPtr) : void
        type_to_token(typ) <| $(var token : AstToken?)
            *tokens |> emplace(token)
    // def override visitTypeDecl(typ:TypeDeclPtr) : TypeDeclPtr
// type alias
    def override preVisitAlias(typ:TypeDeclPtr;name:das_string) : void
        type_to_token(typ) <| $(var token : AstToken?)
            *tokens |> emplace(token)
    // def override visitAlias(typ:TypeDeclPtr;name:das_string) : TypeDeclPtr
// enumeration
    def override preVisitEnumeration(enu:EnumerationPtr) : void
        if enu == null
            return
        var name = "{module_name(enu._module)}{enu.name}"
        var token <- new [[AstToken
            topLevel = true,
            name = name,
            desc = "enum {name} : {get_das_type_name(enu.baseType)}",
            kind = AstTokenKind Enum,
            _type = name
        ]]
        set_token_path(token, enu.at)
        for it in enu.list
            var typ = it.value._type != null ? describe(it.value._type) : ""
            var desc = "{enu.name} {it.name}"
            if it.value != null
                desc += " = {describe(it.value)}"
            var tok <- new [[AstToken
                topLevel = true,
                name = "{it.name}",
                desc = desc,
                kind = AstTokenKind EnumMember,
                _type = typ
            ]]
            set_token_path(tok, it.at)
            token.children |> emplace(tok)
        *tokens |> emplace(token)

    // def override preVisitEnumerationValue(enu:EnumerationPtr;name:das_string;value:ExpressionPtr;last:bool) : void
    // def override visitEnumerationValue(enu:EnumerationPtr;name:das_string;value:ExpressionPtr;last:bool) : ExpressionPtr
    // def override visitEnumeration(enu:EnumerationPtr) : EnumerationPtr
// // structure
    def override preVisitStructure(str:StructurePtr) : void
        if str.flags.generated
            return
        var name = "{module_name(str._module)}{str.name}"
        var desc = "struct {name}"
        if str.parent != null
            desc += " : {module_name(str.parent._module)}{str.parent.name}"
        var token <- new [[AstToken
            topLevel = true,
            name = name,
            desc = desc,
            kind = str.flags.isClass ? AstTokenKind Class : AstTokenKind Struct,
            _type = name
        ]]
        set_token_path(token, str.at)
        for decl in str.fields
            if decl.flags.generated
                continue
            let typ = decl._type != null ? "{describe([decl=decl._type,extra=true])}" : ""
            var fieldName = "{decl.name}"
            var fieldDesc = "{fieldName} : {typ}"
            if decl.init != null
                fieldDesc += " = {describe(decl.init)}"
            var tok <- new [[AstToken
                topLevel = true,
                name = fieldName,
                desc = fieldDesc,
                kind = AstTokenKind Field,
                _type = typ
            ]]
            set_token_path(tok, decl.at)
            token.children |> emplace(tok)
        *tokens |> emplace(token)
    // def override preVisitStructureField(str:StructurePtr;decl:FieldDeclaration;last:bool) : void
    // def override visitStructureField(str:StructurePtr;decl:FieldDeclaration;last:bool) : void
    // def override visitStructure(str:StructurePtr) : StructurePtr
// function
    def override preVisitFunction(fun:FunctionPtr) : void
        if fun.flags.generated & !fun.flags._generator
            skipExprs = true
            return
        var desc = "def {module_name(fun._module)}{fun.name}"
        if fun.arguments.length >= 0
            desc += "("
            var first = true
            for arg in fun.arguments
                if !first
                    desc += "; "
                first = false
                if !arg._type.flags.constant
                    desc += "var "
                desc += "{arg.name} : {describe([decl=arg._type,extra=true])}"
                if arg.init != null
                    desc += " = {describe(arg.init)}"
            desc += ")"
        if fun.result != null
            desc += " : {describe([decl=fun.result,extra=true])}"
        var token <- new [[AstToken
            // topLevel = fun.body != null,
            name = "{fun.name}",
            desc = desc,
            kind = fun.flags.isClassMethod ? AstTokenKind Method : AstTokenKind Function,
            _type = desc
        ]]
        set_token_path(token, fun.at)
        set_token_def(token, fun.atDecl)
        token.topLevel = !fun.flags.isClassMethod //!fun.flags.generated & token._range |> in_range(token.defRange)
        for it in fun.arguments
            var_to_token(it) <| $(var arg : AstToken?)
                token.children |> emplace(arg)
        *tokens |> emplace(token)
    def override visitFunction(fun:FunctionPtr) : FunctionPtr
        skipExprs = false
        return fun
    // def override preVisitFunctionArgument(fun:FunctionPtr;arg:VariablePtr;lastArg:bool) : void
    // def override visitFunctionArgument(fun:FunctionPtr;arg:VariablePtr;lastArg:bool) : VariablePtr
    // def override preVisitFunctionArgumentInit(fun:FunctionPtr;arg:VariablePtr;value:ExpressionPtr) : void
    // def override visitFunctionArgumentInit(fun:FunctionPtr;arg:VariablePtr;value:ExpressionPtr) : ExpressionPtr
    // def override preVisitFunctionBody(fun:FunctionPtr) : void
    // def override visitFunctionBody(fun:FunctionPtr) : void
// any expression there is
    // def override preVisitExpression(expr:ExpressionPtr) : void
    // def override visitExpression(expr:ExpressionPtr) : ExpressionPtr
// block
    // def override preVisitExprBlock(fun:smart_ptr<ExprBlock>) : void
    // def override visitExprBlock(blk:smart_ptr<ExprBlock>) : ExpressionPtr
    def override preVisitExprBlockArgument(blk:smart_ptr<ExprBlock>;arg:VariablePtr;lastArg:bool): void
        if skipExprs
            return
        var_to_token(arg) <| $(var token : AstToken?)
            *tokens |> emplace(token)
    // def override visitExprBlockArgument(blk:smart_ptr<ExprBlock>;arg:VariablePtr;lastArg:bool) : VariablePtr
    // def override preVisitExprBlockArgumentInit(blk:smart_ptr<ExprBlock>;arg:VariablePtr;expr:ExpressionPtr): void
    // def override visitExprBlockArgumentInit(blk:smart_ptr<ExprBlock>;arg:VariablePtr;expr:ExpressionPtr) : ExpressionPtr
    // def override preVisitExprBlockExpression(blk:smart_ptr<ExprBlock>;expr:ExpressionPtr): void
    // def override visitExprBlockExpression(blk:smart_ptr<ExprBlock>;expr:ExpressionPtr) : ExpressionPtr
    // def override preVisitExprBlockFinal(blk:smart_ptr<ExprBlock>): void
    // def override visitExprBlockFinal(blk:smart_ptr<ExprBlock>): void
    // def override preVisitExprBlockFinalExpression(blk:smart_ptr<ExprBlock>;expr:ExpressionPtr): void
    // def override visitExprBlockFinalExpression(blk:smart_ptr<ExprBlock>;expr:ExpressionPtr) : ExpressionPtr
// let
    def override preVisitExprLet(expr:smart_ptr<ExprLet>) : void
        if skipExprs
            return
        for it in expr.variables
            var_to_token(it) <| $(var token : AstToken?)
                *tokens |> emplace(token)
    // def override visitExprLet(expr:smart_ptr<ExprLet>) : ExpressionPtr
    // def override preVisitExprLetVariable(expr:smart_ptr<ExprLet>;arg:VariablePtr;lastArg:bool) : void
    // def override visitExprLetVariable(expr:smart_ptr<ExprLet>;arg:VariablePtr;lastArg:bool) : VariablePtr
    // def override preVisitExprLetVariableInit(blk:smart_ptr<ExprLet>;arg:VariablePtr;expr:ExpressionPtr): void
    // def override visitExprLetVariableInit(blk:smart_ptr<ExprLet>;arg:VariablePtr;expr:ExpressionPtr) : ExpressionPtr
// global let
    // def override preVisitGlobalLet(prog:ProgramPtr) : void
    // def override visitGlobalLet(prog:ProgramPtr) : void
    def override preVisitGlobalLetVariable(arg:VariablePtr;lastArg:bool) : void
        if skipExprs
            return
        var_to_token(arg) <| $(var token : AstToken?)
            token.topLevel = true
            *tokens |> emplace(token)
    // def override visitGlobalLetVariable(arg:VariablePtr;lastArg:bool) : VariablePtr
    // def override preVisitGlobalLetVariableInit(arg:VariablePtr;expr:ExpressionPtr): void
    // def override visitGlobalLetVariableInit(arg:VariablePtr;expr:ExpressionPtr) : ExpressionPtr
// string builder
    // def override preVisitExprStringBuilder(expr:smart_ptr<ExprStringBuilder>): void
    // def override visitExprStringBuilder(expr:smart_ptr<ExprStringBuilder>) : ExpressionPtr
    // def override preVisitExprStringBuilderElement(expr:smart_ptr<ExprStringBuilder>;elem:ExpressionPtr;last:bool): void
    // def override visitExprStringBuilderElement(expr:smart_ptr<ExprStringBuilder>;elem:ExpressionPtr;last:bool) : ExpressionPtr
// new
    // def override preVisitExprNew(expr:smart_ptr<ExprNew>): void
    // def override visitExprNew(expr:smart_ptr<ExprNew>) : ExpressionPtr
    // def override preVisitExprNewArgument(expr:smart_ptr<ExprNew>;arg:ExpressionPtr;last:bool): void
    // def override visitExprNewArgument(expr:smart_ptr<ExprNew>;arg:ExpressionPtr;last:bool) : ExpressionPtr
// named call (this one only exists before or during the infer)
    // def override preVisitExprNamedCall(expr:smart_ptr<ExprNamedCall>): void
    // def override visitExprNamedCall(expr:smart_ptr<ExprNamedCall>) : ExpressionPtr
    // def override preVisitExprNamedCallArgument(expr:smart_ptr<ExprNamedCall>;arg:MakeFieldDeclPtr;last:bool): void
    // def override visitExprNamedCallArgument(expr:smart_ptr<ExprNamedCall>;arg:MakeFieldDeclPtr;last:bool) : MakeFieldDeclPtr
// looks like call (debug,assert,verify,erase,find,key_exists,keys,values,invoke,memzero etc)
    def override preVisitExprLooksLikeCall(expr:smart_ptr<ExprLooksLikeCall>): void
        if skipExprs
            return
        like_call_to_token(expr) <| $(var token : AstToken?)
            *tokens |> emplace(token)
    // def override visitExprLooksLikeCall(expr:smart_ptr<ExprLooksLikeCall>) : ExpressionPtr
    // def override preVisitExprLooksLikeCallArgument(expr:smart_ptr<ExprLooksLikeCall>;arg:ExpressionPtr;last:bool): void
    // def override visitExprLooksLikeCallArgument(expr:smart_ptr<ExprLooksLikeCall>;arg:ExpressionPtr;last:bool) : ExpressionPtr
// call
    def override preVisitExprCall(expr:smart_ptr<ExprCall>): void
        if skipExprs
            return
        call_to_token(expr) <| $(var token : AstToken?)
            *tokens |> emplace(token)
    // def override visitExprCall(expr:smart_ptr<ExprCall>) : ExpressionPtr
    // def override preVisitExprCallArgument(expr:smart_ptr<ExprCall>;arg:ExpressionPtr;last:bool): void
    // def override visitExprCallArgument(expr:smart_ptr<ExprCall>;arg:ExpressionPtr;last:bool) : ExpressionPtr
// null coaelescing
    // def override preVisitExprNullCoalescing(expr:smart_ptr<ExprNullCoalescing>): void
    // def override visitExprNullCoalescing(expr:smart_ptr<ExprNullCoalescing>) : ExpressionPtr
    // def override preVisitExprNullCoalescingDefault(expr:smart_ptr<ExprNullCoalescing>;defval:ExpressionPtr): void
// at
    // def override preVisitExprAt(expr:smart_ptr<ExprAt>): void
    // def override visitExprAt(expr:smart_ptr<ExprAt>) : ExpressionPtr
    // def override preVisitExprAtIndex(expr:smart_ptr<ExprAt>;index:ExpressionPtr): void
// // safe at
    // def override preVisitExprSafeAt(expr:smart_ptr<ExprSafeAt>): void
    // def override visitExprSafeAt(expr:smart_ptr<ExprSafeAt>) : ExpressionPtr
    // def override preVisitExprSafeAtIndex(expr:smart_ptr<ExprAt>;index:ExpressionPtr): void
// is
    // def override preVisitExprIs(expr:smart_ptr<ExprIs>): void
    // def override visitExprIs(expr:smart_ptr<ExprIs>) : ExpressionPtr
    // def override preVisitExprIsType(expr:smart_ptr<ExprAt>;typeDecl:TypeDeclPtr): void
// op2
    // def override preVisitExprOp2(expr:smart_ptr<ExprOp2>): void
    // def override visitExprOp2(expr:smart_ptr<ExprOp2>) : ExpressionPtr
    // def override preVisitExprOp2Right(expr:smart_ptr<ExprOp2>;right:ExpressionPtr): void
// op3
    // def override preVisitExprOp3(expr:smart_ptr<ExprOp3>): void
    // def override visitExprOp3(expr:smart_ptr<ExprOp3>) : ExpressionPtr
    // def override preVisitExprOp3Left(expr:smart_ptr<ExprOp3>;left:ExpressionPtr): void
    // def override preVisitExprOp3Right(expr:smart_ptr<ExprOp3>;right:ExpressionPtr): void
// copy
    // def override preVisitExprCopy(expr:smart_ptr<ExprCopy>): void
    // def override visitExprCopy(expr:smart_ptr<ExprCopy>) : ExpressionPtr
    // def override preVisitExprCopyRight(expr:smart_ptr<ExprCopy>;right:ExpressionPtr): void
// move
    // def override preVisitExprMove(expr:smart_ptr<ExprMove>): void
    // def override visitExprMove(expr:smart_ptr<ExprMove>) : ExpressionPtr
    // def override preVisitExprMoveRight(expr:smart_ptr<ExprMove>;right:ExpressionPtr) : void
// clone
    // def override preVisitExprClone(expr:smart_ptr<ExprClone>) : void
    // def override visitExprClone(expr:smart_ptr<ExprClone>) : ExpressionPtr
    // def override preVisitExprCloneRight(expr:smart_ptr<ExprClone>;right:ExpressionPtr) : void
// with
    // def override preVisitExprWith(expr:smart_ptr<ExprWith>) : void
    // def override visitExprWith(expr:smart_ptr<ExprWith>) : ExpressionPtr
    // def override preVisitExprWithBody(expr:smart_ptr<ExprWith>;right:ExpressionPtr) : void
// while
    // def override preVisitExprWhile(expr:smart_ptr<ExprWhile>) : void
    // def override visitExprWhile(expr:smart_ptr<ExprWhile>) : ExpressionPtr
    // def override preVisitExprWhileBody(expr:smart_ptr<ExprWhile>;right:ExpressionPtr) : void
// try-catch
    // def override preVisitExprTryCatch(expr:smart_ptr<ExprTryCatch>) : void
    // def override visitExprTryCatch(expr:smart_ptr<ExprTryCatch>) : ExpressionPtr
    // def override preVisitExprTryCatchCatch(expr:smart_ptr<ExprTryCatch>;right:ExpressionPtr) : void
// if-then-else
    // def override preVisitExprIfThenElse(expr:smart_ptr<ExprIfThenElse>) : void
    // def override visitExprIfThenElse(expr:smart_ptr<ExprIfThenElse>) : ExpressionPtr
    // def override preVisitExprIfThenElseIfBlock(expr:smart_ptr<ExprIfThenElse>;ifBlock:ExpressionPtr) : void
    // def override preVisitExprIfThenElseElseBlock(expr:smart_ptr<ExprIfThenElse>;elseBlock:ExpressionPtr) : void
// for
    // def override preVisitExprFor(expr:smart_ptr<ExprFor>) : void
    // def override visitExprFor(expr:smart_ptr<ExprFor>) : ExpressionPtr
    def override preVisitExprForVariable(expr:smart_ptr<ExprFor>;svar:VariablePtr;last:bool) : void
        if skipExprs
            return
        var_to_token(svar) <| $(var token : AstToken?)
            *tokens |> emplace(token)
    // def override visitExprForVariable(expr:smart_ptr<ExprFor>;svar:VariablePtr;last:bool) : VariablePtr
    // def override preVisitExprForSource(expr:smart_ptr<ExprFor>;source:ExpressionPtr;last:bool) : void
    // def override visitExprForSource(expr:smart_ptr<ExprFor>;source:ExpressionPtr;last:bool) : ExpressionPtr
    // def override preVisitExprForStack(expr:smart_ptr<ExprFor>) : void
    // def override preVisitExprForBody(expr:smart_ptr<ExprFor>) : void
// make variant
    // def override preVisitExprMakeVariant(expr:smart_ptr<ExprMakeVariant>) : void
    // def override visitExprMakeVariant(expr:smart_ptr<ExprMakeVariant>) : ExpressionPtr
    // def override preVisitExprMakeVariantField(expr:smart_ptr<ExprMakeVariant>;index:int;decl:MakeFieldDeclPtr;last:bool) : void
    // def override visitExprMakeVariantField(expr:smart_ptr<ExprMakeVariant>;index:int;decl:MakeFieldDeclPtr;last:bool) : MakeFieldDeclPtr
// make structure
    // def override preVisitExprMakeStruct(expr:smart_ptr<ExprMakeStruct>) : void
    // def override visitExprMakeStruct(expr:smart_ptr<ExprMakeStruct>) : ExpressionPtr
    // def override preVisitExprMakeStructIndex(expr:smart_ptr<ExprMakeStruct>;index:int;last:bool) : void
    // def override visitExprMakeStructIndex(expr:smart_ptr<ExprMakeStruct>;index:int;last:bool) : void
    def override preVisitExprMakeStructField(expr:smart_ptr<ExprMakeStruct>;index:int;decl:MakeFieldDeclPtr;last:bool) : void
        if skipExprs
            return
        var typ = decl.value != null & decl.value._type != null ? describe(decl.value._type) : ""
        var name = "{decl.name}"
        if expr._type != null
            name = "{describe(expr._type)}.{name}"
        var token <- new [[AstToken
            name = name,
            desc = "{name} : {typ}",
            kind = AstTokenKind Field,
            _type = typ
        ]]
        set_token_path(token, decl.at)
        if expr._type != null
            if expr._type.structType != null
                set_token_def(token, expr._type.structType.at)
        *tokens |> emplace(token)
    // def override visitExprMakeStructField(expr:smart_ptr<ExprMakeStruct>;index:int;decl:MakeFieldDeclPtr;last:bool) : MakeFieldDeclPtr
// make array
    // def override preVisitExprMakeArray(expr:smart_ptr<ExprMakeArray>) : void
    // def override visitExprMakeArray(expr:smart_ptr<ExprMakeArray>) : ExpressionPtr
    // def override preVisitExprMakeArrayIndex(expr:smart_ptr<ExprMakeArray>;index:int;init:ExpressionPtr;last:bool) : void
    // def override visitExprMakeArrayIndex(expr:smart_ptr<ExprMakeArray>;index:int;init:ExpressionPtr;last:bool) : ExpressionPtr
// make tuple
    // def override preVisitExprMakeTuple(expr:smart_ptr<ExprMakeTuple>) : void
    // def override visitExprMakeTuple(expr:smart_ptr<ExprMakeTuple>) : ExpressionPtr
    // def override preVisitExprMakeTupleIndex(expr:smart_ptr<ExprMakeTuple>;index:int;init:ExpressionPtr;last:bool) : void
    // def override visitExprMakeTupleIndex(expr:smart_ptr<ExprMakeTuple>;index:int;init:ExpressionPtr;last:bool) : ExpressionPtr
// array comprehension
    // def override preVisitExprArrayComprehension(expr:smart_ptr<ExprArrayComprehension>) : void
    // def override visitExprArrayComprehension(expr:smart_ptr<ExprArrayComprehension>) : ExpressionPtr
    // def override preVisitExprArrayComprehensionSubexpr(expr:smart_ptr<ExprArrayComprehension>;subexrp:ExpressionPtr) : void
    // def override preVisitExprArrayComprehensionWhere(expr:smart_ptr<ExprArrayComprehension>;filter:ExpressionPtr) : void
// type info
    // def override preVisitExprTypeInfo(expr:smart_ptr<ExprTypeInfo>) : void
    // def override visitExprTypeInfo(expr:smart_ptr<ExprTypeInfo>) : ExpressionPtr
// ptr to ref
    // def override preVisitExprPtr2Ref(expr:smart_ptr<ExprPtr2Ref>) : void
    // def override visitExprPtr2Ref(expr:smart_ptr<ExprPtr2Ref>) : ExpressionPtr
// label
    // def override preVisitExprLabel(expr:smart_ptr<ExprLabel>) : void
    // def override visitExprLabel(expr:smart_ptr<ExprLabel>) : ExpressionPtr
// goto
    // def override preVisitExprGoto(expr:smart_ptr<ExprGoto>) : void
    // def override visitExprGoto(expr:smart_ptr<ExprGoto>) : ExpressionPtr
// ref to value
    // def override preVisitExprRef2Value(expr:smart_ptr<ExprRef2Value>) : void
    // def override visitExprRef2Value(expr:smart_ptr<ExprRef2Value>) : ExpressionPtr
// ref to ptr
    // def override preVisitExprRef2Ptr(expr:smart_ptr<ExprRef2Ptr>) : void
    // def override visitExprRef2Ptr(expr:smart_ptr<ExprRef2Ptr>) : ExpressionPtr
// @@
    // def override preVisitExprAddr(expr:smart_ptr<ExprAddr>) : void
    // def override visitExprAddr(expr:smart_ptr<ExprAddr>) : ExpressionPtr
// assert / verify
    // def override preVisitExprAssert(expr:smart_ptr<ExprAssert>) : void
    // def override visitExprAssert(expr:smart_ptr<ExprAssert>) : ExpressionPtr
// static_assert
    // def override preVisitExprStaticAssert(expr:smart_ptr<ExprStaticAssert>) : void
    // def override visitExprStaticAssert(expr:smart_ptr<ExprStaticAssert>) : ExpressionPtr
// debug
    // def override preVisitExprDebug(expr:smart_ptr<ExprDebug>) : void
    // def override visitExprDebug(expr:smart_ptr<ExprDebug>) : ExpressionPtr
// invoke
    // def override preVisitExprInvoke(expr:smart_ptr<ExprInvoke>) : void
    // def override visitExprInvoke(expr:smart_ptr<ExprInvoke>) : ExpressionPtr
// erase
    // def override preVisitExprErase(expr:smart_ptr<ExprErase>) : void
    // def override visitExprErase(expr:smart_ptr<ExprErase>) : ExpressionPtr
// find
    // def override preVisitExprFind(expr:smart_ptr<ExprFind>) : void
    // def override visitExprFind(expr:smart_ptr<ExprFind>) : ExpressionPtr
// key exists
    // def override preVisitExprKeyExists(expr:smart_ptr<ExprKeyExists>) : void
    // def override visitExprKeyExists(expr:smart_ptr<ExprKeyExists>) : ExpressionPtr
// ascend
    // def override preVisitExprAscend(expr:smart_ptr<ExprAscend>) : void
    // def override visitExprAscend(expr:smart_ptr<ExprAscend>) : ExpressionPtr
// cast
    // def override preVisitExprCast(expr:smart_ptr<ExprCast>) : void
    // def override visitExprCast(expr:smart_ptr<ExprCast>) : ExpressionPtr
// delete
    // def override preVisitExprDelete(expr:smart_ptr<ExprDelete>) : void
    // def override visitExprDelete(expr:smart_ptr<ExprDelete>) : ExpressionPtr
// var
    def override preVisitExprVar(expr:smart_ptr<ExprVar>) : void
        if skipExprs
            return
        var_to_token(expr.variable) <| $(var token)
            token.defPath = token.path
            token.defRange = token._range
            set_token_path(token, expr.at)
            *tokens |> emplace(token)
    // def override visitExprVar(expr:smart_ptr<ExprVar>) : ExpressionPtr
// field
    def override preVisitExprField(expr:smart_ptr<ExprField>) : void
        if skipExprs
            return
        field_to_token(expr) <| $(var token)
            *tokens |> emplace(token)
    // def override visitExprField(expr:smart_ptr<ExprField>) : ExpressionPtr
// safe field
    def override preVisitExprSafeField(expr:smart_ptr<ExprSafeField>) : void
        if skipExprs
            return
        field_to_token(expr) <| $(var token)
            *tokens |> emplace(token)
    // def override visitExprSafeField(expr:smart_ptr<ExprSafeField>) : ExpressionPtr
// swizzle
    def override preVisitExprSwizzle(expr:smart_ptr<ExprSwizzle>) : void
        if skipExprs
            return
        var swizzle = ""
        let f2name = [[string "x";"y";"z";"w"]]
        for fch in expr.fields
            let f = int(fch)
            swizzle += "{f>=0 & f<=3 ? f2name[f] : "?"}"
        let typ = "{describe([decl=expr.value._type,extra=true])}.{swizzle}"
        let name = "{describe(expr.value)}.{swizzle}"
        var token <- new [[AstToken
            name = name,
            desc = "{name} : {typ}",
            kind = AstTokenKind Variable,
            _type = "{typ}"
        ]]
        set_token_path(token, expr.value != null ? expr.value.at : expr.at)
        token._range.start.character = token._range.end.character + 1
        token._range.end.character += length(swizzle) + 1
        *tokens |> emplace(token)
    // def override visitExprSwizzle(expr:smart_ptr<ExprSwizzle>) : ExpressionPtr
// is variant
    // def override preVisitExprIsVariant(expr:smart_ptr<ExprIsVariant>) : void
    // def override visitExprIsVariant(expr:smart_ptr<ExprIsVariant>) : ExpressionPtr
// as variant
    // def override preVisitExprAsVariant(expr:smart_ptr<ExprAsVariant>) : void
    // def override visitExprAsVariant(expr:smart_ptr<ExprAsVariant>) : ExpressionPtr
// safe as variant
    // def override preVisitExprSafeAsVariant(expr:smart_ptr<ExprSafeAsVariant>) : void
    // def override visitExprSafeAsVariant(expr:smart_ptr<ExprSafeAsVariant>) : ExpressionPtr
// op1
    // def override preVisitExprOp1(expr:smart_ptr<ExprOp1>) : void
    // def override visitExprOp1(expr:smart_ptr<ExprOp1>) : ExpressionPtr
// return
    // def override preVisitExprReturn(expr:smart_ptr<ExprReturn>) : void
    // def override visitExprReturn(expr:smart_ptr<ExprReturn>) : ExpressionPtr
// yield
    // def override preVisitExprYield(expr:smart_ptr<ExprYield>) : void
    // def override visitExprYield(expr:smart_ptr<ExprYield>) : ExpressionPtr
// break
    // def override preVisitExprBreak(expr:smart_ptr<ExprBreak>) : void
    // def override visitExprBreak(expr:smart_ptr<ExprBreak>) : ExpressionPtr
// continue
    // def override preVisitExprContinue(expr:smart_ptr<ExprContinue>) : void
    // def override visitExprContinue(expr:smart_ptr<ExprContinue>) : ExpressionPtr
// make block
    // def override preVisitExprMakeBlock(expr:smart_ptr<ExprMakeBlock>) : void
    // def override visitExprMakeBlock(expr:smart_ptr<ExprMakeBlock>) : ExpressionPtr
// make generator
    // def override preVisitExprMakeGenerator(expr:smart_ptr<ExprMakeGenerator>) : void
    // def override visitExprMakeGenerator(expr:smart_ptr<ExprMakeGenerator>) : ExpressionPtr
// memzero
    // def override preVisitExprMemZero(expr:smart_ptr<ExprMemZero>) : void
    // def override visitExprMemZero(expr:smart_ptr<ExprMemZero>) : ExpressionPtr
// const
    def override preVisitExprConst(expr:smart_ptr<ExprConst>) : void
        if skipExprs
            return
        if expr.at.line == 0u & expr.at.column == 0u
            return
        let typ = expr._type != null ? describe([decl=expr._type,extra=true]) : ""
        var name = describe(ExpressionPtr(expr))
        var token <- new [[AstToken
            name = name,
            desc = "constant : {typ} = {name}",
            kind = AstTokenKind Constant,
            _type = "{typ}"
        ]]
        set_token_path(token, expr.at)
        *tokens |> emplace(token)
    // def override visitExprConst(expr:smart_ptr<ExprConst>) : ExpressionPtr
// const ptr
    // def override preVisitExprConstPtr(expr:smart_ptr<ExprConstPtr>) : void
    // def override visitExprConstPtr(expr:smart_ptr<ExprConstPtr>) : ExpressionPtr
// const enumeration
    def override preVisitExprConstEnumeration(expr:smart_ptr<ExprConstEnumeration>) : void
        if skipExprs
            return
        let typ = expr._type != null ? describe(expr._type, false, false, false) : ""
        var name = "{expr.value}"
        var token <- new [[AstToken
            name = name,
            desc = "{typ} {name}",
            kind = AstTokenKind EnumMember,
            _type = "{typ}"
        ]]
        set_token_path(token, expr.at)
        *tokens |> emplace(token)

    // def override visitExprConstEnumeration(expr:smart_ptr<ExprConstEnumeration>) : ExpressionPtr
// cont bitfield
    // def override preVisitExprConstBitfield(expr:smart_ptr<ExprConstBitfield>) : void
    // def override visitExprConstBitfield(expr:smart_ptr<ExprConstBitfield>) : ExpressionPtr
// const int 8
    // def override preVisitExprConstInt8(expr:smart_ptr<ExprConstInt8>) : void
    // def override visitExprConstInt8(expr:smart_ptr<ExprConstInt8>) : ExpressionPtr
// const int 16
    // def override preVisitExprConstInt16(expr:smart_ptr<ExprConstInt16>) : void
    // def override visitExprConstInt16(expr:smart_ptr<ExprConstInt16>) : ExpressionPtr
// const int 64
    // def override preVisitExprConstInt64(expr:smart_ptr<ExprConstInt64>) : void
    // def override visitExprConstInt64(expr:smart_ptr<ExprConstInt64>) : ExpressionPtr
// const int
    // def override preVisitExprConstInt(expr:smart_ptr<ExprConstInt>) : void
    // def override visitExprConstInt(expr:smart_ptr<ExprConstInt>) : ExpressionPtr
// const int2
    // def override preVisitExprConstInt2(expr:smart_ptr<ExprConstInt2>) : void
    // def override visitExprConstInt2(expr:smart_ptr<ExprConstInt2>) : ExpressionPtr
// const int3
    // def override preVisitExprConstInt3(expr:smart_ptr<ExprConstInt3>) : void
    // def override visitExprConstInt3(expr:smart_ptr<ExprConstInt3>) : ExpressionPtr
// const int4
    // def override preVisitExprConstInt4(expr:smart_ptr<ExprConstInt4>) : void
    // def override visitExprConstInt4(expr:smart_ptr<ExprConstInt4>) : ExpressionPtr
// const uint 8
    // def override preVisitExprConstUInt8(expr:smart_ptr<ExprConstUInt8>) : void
    // def override visitExprConstUInt8(expr:smart_ptr<ExprConstUInt8>) : ExpressionPtr
// const uint 16
    // def override preVisitExprConstUInt16(expr:smart_ptr<ExprConstUInt16>) : void
    // def override visitExprConstUInt16(expr:smart_ptr<ExprConstUInt16>) : ExpressionPtr
// const uint 64
    // def override preVisitExprConstUInt64(expr:smart_ptr<ExprConstUInt64>) : void
    // def override visitExprConstUInt64(expr:smart_ptr<ExprConstUInt64>) : ExpressionPtr
// const uint
    // def override preVisitExprConstUInt(expr:smart_ptr<ExprConstUInt>) : void
    // def override visitExprConstUInt(expr:smart_ptr<ExprConstUInt>) : ExpressionPtr
// const uint2
    // def override preVisitExprConstUInt2(expr:smart_ptr<ExprConstUInt2>) : void
    // def override visitExprConstUInt2(expr:smart_ptr<ExprConstUInt2>) : ExpressionPtr
// const uint3
    // def override preVisitExprConstUInt3(expr:smart_ptr<ExprConstUInt3>) : void
    // def override visitExprConstUInt3(expr:smart_ptr<ExprConstUInt3>) : ExpressionPtr
// const uint4
    // def override preVisitExprConstUInt4(expr:smart_ptr<ExprConstUInt4>) : void
    // def override visitExprConstUInt4(expr:smart_ptr<ExprConstUInt4>) : ExpressionPtr
// const range
    // def override preVisitExprConstRange(expr:smart_ptr<ExprConstRange>) : void
    // def override visitExprConstRange(expr:smart_ptr<ExprConstRange>) : ExpressionPtr
// const urnage
    // def override preVisitExprConstURange(expr:smart_ptr<ExprConstURange>) : void
    // def override visitExprConstURange(expr:smart_ptr<ExprConstURange>) : ExpressionPtr
// const bool
    // def override preVisitExprConstBool(expr:smart_ptr<ExprConstBool>) : void
    // def override visitExprConstBool(expr:smart_ptr<ExprConstBool>) : ExpressionPtr
// const float
    // def override preVisitExprConstFloat(expr:smart_ptr<ExprConstFloat>) : void
    // def override visitExprConstFloat(expr:smart_ptr<ExprConstFloat>) : ExpressionPtr
// const float2
    // def override preVisitExprConstFloat2(expr:smart_ptr<ExprConstFloat2>) : void
    // def override visitExprConstFloat2(expr:smart_ptr<ExprConstFloat2>) : ExpressionPtr
// const float3
    // def override preVisitExprConstFloat3(expr:smart_ptr<ExprConstFloat3>) : void
    // def override visitExprConstFloat3(expr:smart_ptr<ExprConstFloat3>) : ExpressionPtr
// const float4
    // def override preVisitExprConstFloat4(expr:smart_ptr<ExprConstFloat4>) : void
    // def override visitExprConstFloat4(expr:smart_ptr<ExprConstFloat4>) : ExpressionPtr
// const string
    // def override preVisitExprConstString(expr:smart_ptr<ExprConstString>) : void
    // def override visitExprConstString(expr:smart_ptr<ExprConstString>) : ExpressionPtr
// const double
    // def override preVisitExprConstDouble(expr:smart_ptr<ExprConstDouble>) : void
    // def override visitExprConstDouble(expr:smart_ptr<ExprConstDouble>) : ExpressionPtr
// fake context
    // def override preVisitExprFakeContext(expr:smart_ptr<ExprFakeContext>) : void
    // def override visitExprFakeContext(expr:smart_ptr<ExprFakeContext>) : ExpressionPtr
// fake line info
    // def override preVisitExprFakeLineInfo(expr:smart_ptr<ExprFakeLineInfo>) : void
    // def override visitExprFakeLineInfo(expr:smart_ptr<ExprFakeLineInfo>) : ExpressionPtr
