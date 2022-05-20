options indenting = 4
options no_aot = true

require uriparser
require rtti

require lsp_types
require completion
require ast_token
require fs_tools
require strings


struct Dascript
    uri : DocumentUri
    path : string
    text : string
    version : double

    isOpen : bool
    validated : bool

    completionData : Completions
    ast : AstTokens
    program : smart_ptr<rtti::Program>
    fs : smart_ptr<rtti::FileAccess>
    [[do_not_delete]] mg : ModuleGroup?



def Dascript(txt : TextDocumentItem)
    return <- [[Dascript uri = txt.uri, path = uri_to_path(txt.uri), version = txt.version, text = txt.text, isOpen = true]]

def Dascript(doc : DidChangeTextDocumentParams)
    return <- [[Dascript uri = doc.textDocument.uri, path = uri_to_path(doc.textDocument.uri), version = doc.textDocument.version, text = doc.contentChanges[0].text, isOpen = true]]

def Dascript(doc : DidSaveTextDocumentParams)
    return <- [[Dascript uri = doc.textDocument.uri, path = uri_to_path(doc.textDocument.uri), version = -1lf, text = doc.text, isOpen = true]]

def Dascript(doc : HoverParams)
    return <- [[Dascript uri = doc.textDocument.uri, path = uri_to_path(doc.textDocument.uri), version = -1lf, text = "", isOpen = true]]


def find_token(data : Dascript const?; path : string; pos : Position; valid : block<(tok : AstToken?) : bool>) : AstToken?
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
