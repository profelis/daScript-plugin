// options log = true

require rtti
require network
require fio
require daslib/json
require json_tools
require lsp_types


var
    isContDev = false
    registry : table<DocumentUri; TextDocumentItem>

class TelnetServer : Server
    done : bool = false
    current_string : string
    initialized : bool = false
    toRead : int = 0

    def TelnetServer
        Server`Server(cast<Server> self)

    def override onError(msg:string; code:int)
        print("server error:{code} - {msg}\n")
        done = true

    def override onConnect
        print("connected\n")

    def override onDisconnect
        if !empty(current_string)
            print(current_string)
            current_string = ""
        print("disconnected\n")

    [unsafe] def restart
        if _server != null
            var session : smart_ptr<NetworkServer>
            self->save(session)
            gc0_save_smart_ptr("telnet-session", session)

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#responseMessage
    [unsafe] def sendRawResponse(data : JsonValue?)
        let msg = write_json(data)
        let packStr = "Content-Length: {length(msg)}\r\n\r\n{msg}"
        print("<------\n{packStr}\n-------\n\n")
        self->send(reinterpret<uint8?> packStr, length(packStr))

    def sendResponse(id : double; data : JsonValue?)
        let res = JV({{ "jsonrpc" => JV("2.0"); "id" => JV(id); "result" => data }})
        self->sendRawResponse(res)

    def sendError(id : double; data : ResponseError)
        let res = JV({{ "jsonrpc" => JV("2.0"); "id" => JV(id); "error" => toJson(data) }})
        self->sendRawResponse(res)

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#notificationMessage
    def sendNotification(method : string; data : JsonValue?)
        self->sendRawResponse(JV({{ "jsonrpc" => JV("2.0"); "method" => JV(method); "params" => data }}))

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize
    def sendInit(id : double; params : JsonValue?)
        let cap = [[ServerCapabilities textDocumentSync = TextDocumentSyncKind Full, hoverProvider = true]]
        let res = [[InitializeResult capabilities = cap, serverInfo = new [[ServerInfo name = "das_server"]] ]]
        self->sendResponse(id, toJson(res))

    def validate(uri : string; content : string = "")
        print("validate!!!!\n")
        compile_file(uriToPath(uri)) <| $(ok, program, error)
            debug(ok)
            debug(program)
            debug(error)
            if !ok
                let pos = [[Position line = 2, character = 2]]
                let diag = [[Diagnostic message = "{error}", _range = [[Range start = pos, end = pos ]] ]]
                let params <- [[PublishDiagnosticsParams uri = uri, diagnostics <- [{ auto[] diag }] ]]
                self->sendNotification("textDocument/publishDiagnostics", toJson(params))
            else
                let params <- [[PublishDiagnosticsParams uri = uri ]]
                self->sendNotification("textDocument/publishDiagnostics", toJson(params))

    def didOpen(id : double; params : JsonValue?)
        let doc = DidOpenTextDocumentParams(params)
        let uri = doc.textDocument.uri
        registry[uri] = doc.textDocument
        print("< open file {uri} len {length(doc.textDocument.text)}, total files {length(registry)}\n")
        self->validate(uri, doc.textDocument.text)

    def didSave(id : double; params : JsonValue?)
        let doc = DidSaveTextDocumentParams(params)
        let uri = doc.textDocument.uri
        print("< save file {uri} len {length(doc.text)}, total files {length(registry)}\n")
        if length(doc.text) > 0
            find_for_edit(registry, uri) <| $(var data)
                data.text = doc.text
        find(registry, uri) <| $(data)
            if data != null
                self->validate(uri, data.text)

    [unsafe] def didChange(id : double; params : JsonValue?)
        let doc <- DidChangeTextDocumentParams(params)
        let uri = doc.textDocument.uri
        print("< file changed? {uri} changes {length(doc.contentChanges)}\n")
        if length(doc.contentChanges) > 0
            find_for_edit(registry, uri) <| $(var data)
                if data != null
                    data.text = doc.contentChanges[0].text
                    data.version = doc.textDocument.version ?? data.version
                    print("< file changed {uri} len {length(data.text)}, total files {length(registry)}\n")

    def didClose(id : double; params : JsonValue?)
        let doc = DidCloseTextDocumentParams(params)
        erase(registry, doc.textDocument.uri)
        print("< close file {doc.textDocument.uri}, total files {length(registry)}\n")

    def hover(id : double; params : JsonValue?)
        debug("TODO: hover")

    [unsafe] def override onData(msg:uint8?; size:int)
        for i in range(0,size)
            let ch = int(msg[i])

            if toRead == 0
                current_string = append(current_string,ch)
                let prefix = "Content-Length: "
                if ch == '\n' & length(current_string) > length(prefix) & current_string |> starts_with(prefix)
                    let lenStr = current_string |> chop(16, length(current_string) - length(prefix) - 2) // \r\n length
                    toRead = to_int(lenStr)
                    print("> content len {lenStr} as int {toRead}\n")
                    delete current_string
                continue

            if ch == '\n' | ch == '\r'
                continue

            current_string = append(current_string,ch)
            if toRead == length(current_string)
                toRead = 0
                print(">------\n{current_string}\n------\n")
                var error : string
                let msg = read_json(current_string, error)
                if msg == null
                    print("Json read error: {error}\n")
                else
                    if msg.value is _object
                        let method = jos(msg, "method")
                        print("method {method}\n")
                        if method == "initialize"
                            self->sendInit(jon(msg, "id"), joj(msg, "params"))

                        elif method == "initialized"
                            initialized = true
                            print("> initialized response. We did it!\n")

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

                        elif method == "shutdown"
                            done = true

                        elif method == "$/cancelRequest"
                            pass // ignore???

                        else
                            print("\n@@@@@@@@@@@@@@@@@@\nTODO: support: {method}\n@@@@@@@@@@@@@@@@@@\n")
                    else
                        print(write_json(msg))
                delete current_string
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
    elif telnet->init(port)
        print("server at port {port}\n")
    else
        print("server failed to initialize\n")
        return false

    while !telnet.done
        telnet->tick()
        sleep(0u)
        let new_time = stat(fn).mtime
        if new_time != main_time
            print("restarting via source change {new_time}\n")
            telnet->restart()
            break
    delete telnet                       // don't forget this, or we will leak the Server
    return true
