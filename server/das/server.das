// options log = true
// options log_nodes = true
// options heap_page = 0
// options string_heap_page = 0
options debugger=true

require fio
require rtti
require network
require fio
require uriparser
require daslib/json
require json_tools
require lsp_types
require rtti_tools
require completion

var
    isContDev = false

struct Dascript
    uri : DocumentUri
    path : string
    text : string
    version : double
    completionData : Completions

def Dascript(var txt : TextDocumentItem)
    return <- [[Dascript uri = txt.uri, path = uriToPath(txt.uri), version = txt.version, text = move(txt.text)]]

def Dascript(var doc : DidChangeTextDocumentParams)
    return <- [[Dascript uri = doc.textDocument.uri, path = uriToPath(doc.textDocument.uri), version = doc.textDocument.version, text <- doc.contentChanges[0].text]]

def Dascript(var doc : DidSaveTextDocumentParams)
    return <- [[Dascript uri = doc.textDocument.uri, path = uriToPath(doc.textDocument.uri), version = -1lf, text = move(doc.text)]]

class TelnetServer : Server
    done : bool = false // server is done
    current_string : array<uint8> // raw input
    toRead : int = 0

    inited : bool = false // is LSP inited

    completionData <- CompletionData()
    registry : table<DocumentUri; Dascript>

    def TelnetServer
        Server`Server(cast<Server> self)

    def override onError(msg:string; code:int)
        print("server error:{code} - {msg}\n")
        done = true

    def override onConnect
        print("connected\n")

    def override onDisconnect
        if length(current_string) > 0
            print(string(current_string))
            clear(current_string)
        print("disconnected\n")

    [unsafe] def restart
        if _server != null
            var session : smart_ptr<NetworkServer>
            self->save(session)
            gc0_save_smart_ptr("telnet-session", session)

    def updateGlobalCompletion(force : bool)
        completionData->updateGlobalList(force)

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#responseMessage
    [unsafe] def sendRawResponse(var data : JsonValue?; free = true)
        var in scope msg = write_json(data)
        var in scope packStr = "Content-Length: {length(msg)}\r\n\r\n{msg}"
        // print("<------\n{packStr}\n-------\n")
        print("<---- send data {length(packStr)}\n")
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

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize
    def initialize(id : double; params : JsonValue?)
        var cmpl <- [[CompletionOptions resolveProvider = true]]
        var cap <- [[ServerCapabilities
            textDocumentSync = TextDocumentSyncKind Full,
            completionProvider <- cmpl,
            hoverProvider = false
        ]]
        var res <- [[InitializeResult capabilities <- cap, serverInfo = [[ServerInfo name = "das_server"]] ]]
        self->sendResponse(id, js(res), true)
        delete res

    def initialized()
        inited = true
        print("> initialized!\n")
        self->updateGlobalCompletion(true)

    [unsafe] def validate(path : string; withContent : bool = false)
        print("> validate? '{path}'\n")

        registry |> find_for_edit_if_exists(path) <| $(var data)
            print("> validate '{path}'\n")
            let uri = pathToUri(path)
            var in scope access <- make_file_access("")
            if withContent
                set_file_source(access, path, data.text)

            compile_file(path, access) <| $(ok, program, error)
                // debug(ok)
                // debug(program)
                // debug(error)

                if ok
                    if program != null
                        registry |> find_for_edit_if_exists(path) <| $(var data)
                            clear(data.completionData)
                            completionData->programCompletion(program, data.completionData)
                    var in scope params <- [[PublishDiagnosticsParams uri = uri ]]
                    self->sendNotification("textDocument/publishDiagnostics", js(params), true)
                    return

                if program == null
                    var diag <- [[Diagnostic message = "{error}", severity = DiagnosticSeverity Error, _range = [[Range]] ]]
                    var in scope params <- [[PublishDiagnosticsParams uri = uri, diagnostics <- [{ auto[] diag }] ]]
                    self->sendNotification("textDocument/publishDiagnostics", js(params), true)
                    return

                // print("{error}\n")
                // debug(program.errors)
                var map : table<string; array<Diagnostic>>

                for err in program.errors
                    let localUri = pathToUri(string(err.at.fileInfo.name))
                    let at = lineInfoToRange(err.at)
                    var diag <- [[Diagnostic message = "{err.what}", _range = at, severity = DiagnosticSeverity Error, code = double(int(err.cerr)) ]]
                    var extra = "{err.extra}"
                    let fixme = "{err.fixme}"
                    if extra != "" | fixme != ""
                        if fixme != ""
                            extra += "\n{fixme}"
                        var info = [[DiagnosticRelatedInformation location = [[Location _range = at, uri = localUri ]], message = extra ]]
                        diag.relatedInformation <- [{ auto[] info }]
                    var list = find_for_edit(map, localUri)
                    if list != null
                        *list |> emplace(diag)
                    else
                        map[localUri] <- [{ auto[] diag }]
                    debug(err)

                for k, v in keys(map), values(map)
                    var params <- [[PublishDiagnosticsParams uri = k, diagnostics <- v ]]
                    self->sendNotification("textDocument/publishDiagnostics", js(params), true)


    def didOpen(id : double; params : JsonValue?)
        var in scope doc = DidOpenTextDocumentParams(params)
        let path = uriToPath(doc.textDocument.uri)
        registry[path] <- Dascript(doc.textDocument)
        print("> open file {path} len {length(doc.textDocument.text)}, total files {length(registry)}\n")
        self->validate(path, false)

    [unsafe] def didSave(id : double; params : JsonValue?)
        var in scope doc = DidSaveTextDocumentParams(params)
        let path = uriToPath(doc.textDocument.uri)
        print("> save file {path} len {length(doc.text)}, total files {length(registry)}\n")
        var version = -1lf
        var data = find_for_edit(registry, path)
        if data != null
            version = data.version
            if length(doc.text) > 0
                // delete_string(data.text) // TODO:
                data.text = doc.text
        else
            registry[path] <- Dascript(doc)
            print("> unknown? register again, total files {length(registry)}\n")
        self->validate(path, false)

    [unsafe] def didChange(id : double; params : JsonValue?)
        var in scope doc <- DidChangeTextDocumentParams(params)
        let path = uriToPath(doc.textDocument.uri)
        print("> file changed? {path} changes {length(doc.contentChanges)}\n")
        if length(doc.contentChanges) > 0
            var data = find_for_edit(registry, path)
            if data != null
                // delete_string(data.text) // TODO:
                data.text <- doc.contentChanges[0].text
                if doc.textDocument.version >= 0lf
                    data.version = doc.textDocument.version
                print("> file changed {path} len {length(data.text)}, total files {length(registry)}\n")
            else
                print("> file changed {path} len {length(doc.contentChanges[0].text)}, unknown? register again, total files {length(registry)}\n")
                registry[path] <- Dascript(doc)
            self->validate(path, true)

    [unsafe] def didClose(id : double; params : JsonValue?)
        var in scope doc = DidCloseTextDocumentParams(params)
        let path = uriToPath(doc.textDocument.uri)
        find_for_edit_if_exists(registry, path) <| $(var data)
            // delete_string(data.text) // TODO:
            delete *data
        erase(registry, path)
        print("> close file {path}, total files {length(registry)}\n")

        var params <- [[PublishDiagnosticsParams uri = pathToUri(path) ]]
        self->sendNotification("textDocument/publishDiagnostics", js(params), true)

    def hover(id : double; params : JsonValue?)
        debug("TODO: hover")

    [unsafe] def completion(id : double; params : JsonValue?)
        var in scope params <- CompletionParams(params)
        debug(params)
        let data = find(registry, uriToPath(params.textDocument.uri))
        if data != null
            print("< [INFO] send program completion {length(data.completionData)}\n")
            self->sendResponse(id, completionData->getCompletion(data.completionData), true)
        else
            print("< [WARN] send global completion {length(completionData.globalCompletion)}\n")
            self->sendResponse(id, js(completionData.globalCompletion), true)

    def resolveCompletion(id : double; var params : JsonValue?)
        self->sendResponse(id, params, false)

    [unsafe] def override onData(msg:uint8?; size:int)
        for i in range(0,size)
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
                    var lenStr = str |> chop(16, length(current_string) - length(prefix) - 2) // \r\n length
                    toRead = to_int(lenStr)
                    print("> content len {lenStr} as int {toRead}\n")
                    clear(current_string)
                continue

            if ch == '\n' | ch == '\r'
                continue

            push(current_string, uch)
            if toRead == length(current_string)
                toRead = 0
                // print(">------\n{string(current_string)}\n------\n")
                var in scope error : string
                var in scope msg = read_json(current_string, error)
                // debug(msg)
                if msg == null
                    print("Json read error: {error}\n")
                else
                    if msg.value is _object
                        let method = jos(msg, "method")
                        print("method {method}\n")
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

                        elif method == "shutdown"
                            done = true

                        elif method == "$/cancelRequest"
                            pass // ignore???

                        elif method == "$/setTraceNotification"
                            print(write_json(msg))
                            pass // ignore

                        else
                            print("\n@@@@@@@@@@@@@@@@@@\nTODO: support: {method}\n@@@@@@@@@@@@@@@@@@\n")
                            print(write_json(msg))
                    else
                        print(write_json(msg))
                        print("\n@@@@@@@@@@@@@@@@@@\nError: unknown content\n@@@@@@@@@@@@@@@@@@\n")
                clear(current_string)
                if inited
                    self->updateGlobalCompletion(false)
        // print(current_string)

[export]
def main(fn : string)
    isContDev = true
    return mainLoop(fn)

[export]
def init()
    mainLoop("")

[unsafe]
def mainLoop(fn : string)
    let main_time = stat(fn).mtime
    let port = 9000
    print("server start: port: {port} file:{fn} ts:{main_time}\n")
    var telnet = new TelnetServer()

    var session : smart_ptr<NetworkServer> <- gc0_restore_smart_ptr("telnet-session")
    if session != null
        print("restoring server after restart\n")
        telnet->restore(session)
        telnet->initialized()
    elif telnet->init(port)
        print("server at port {port}\n")
    else
        print("server failed to initialize\n")
        return false

    var strLimit = 2u * 1024u * 1024u
    var step = 1u
    while !telnet.done
        telnet->tick()

        var alloc = string_heap_bytes_allocated()
        if alloc > strLimit
            // string_heap_report()
            print("STRING HEAP FREE: {alloc} {int(alloc)}\n")
            string_heap_collect()
            alloc = string_heap_bytes_allocated()
            if alloc > strLimit
                strLimit += strLimit
                print("UPDATE STRING HEAP LIMIT TO {strLimit}\n")
            print("STRING HEAP FREE after: {alloc} {int(alloc)}\n")

        step --
        if step == 0u
            let h = heap_bytes_allocated()
            print("str heap: {int(alloc)} heap : {int(h)}\n")

            step = 100u
            // if h > 15000u
            //     print("------------------\n")
            //     heap_report()
            //     print("------------------\n")
            //     string_heap_report()
            //     print("------------------\n")
            //     step = 100u


        sleep(0u)
        let new_time = stat(fn).mtime
        if new_time != main_time
            print("restarting via source change {new_time}\n")
            telnet->restart()
            break
    delete telnet

    return true
