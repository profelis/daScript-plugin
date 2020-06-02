require daslib/json
require json_tools

def urlDecode(str : string) : string
	return join(split(str, "%3A"), ":")

[unsafe]
def uriToPath(str : string)
	let path = urlDecode(str)
	return path |> character_at(9) == ':' ? chop(path, 8, length(path) - 8) : chop(path, 7, length(path) - 7)

typedef
    DocumentUri = string

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize
struct ServerInfo
	name : string
	version : string

def toJson(val : ServerInfo)
	return JV({{ "name" => JV(val.name); "version" => JV(val.version) }})

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize
struct ServerCapabilities
	textDocumentSync : TextDocumentSyncKind
	hoverProvider : bool
	// TODO:

enum TextDocumentSyncKind
	None = 0
	Full = 1
	Incremental = 2

def toJson(val : ServerCapabilities)
	return JV({{ "textDocumentSync" => JV(double(int(val.textDocumentSync))); "hoverProvider" => JV(val.hoverProvider) }})

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize
struct InitializeResult
	capabilities : ServerCapabilities
	serverInfo : ServerInfo?

def toJson(val : InitializeResult)
	var res <- {{ "capabilities" => toJson(val.capabilities) }}
	if val.serverInfo != null
		res["serverInfo"] = toJson(*(val.serverInfo))
	return JV(res)


// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#responseMessage
struct ResponseError
    code : double
    message : string
    data : JsonValue? // string | number | boolean | array | object | null

def toJson(val : ResponseError)
	return JV({{ "code" => JV(val.code); "message" => JV(val.message); "data" => val.data }})

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#position
struct Position
    line : int = 1
    character : int = 1

def Position(val : JsonValue?)
	if val == null | val.value is _null
		return [[Position]]
	return [[Position line = int(jon(val, "line")), character = int(jon(val, "character")) ]]

def toJson(val : Position)
	return JV({{ "line" => JV(double(val.line)); "character" => JV(double(val.character)) }})

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#range
struct Range
    start : Position
    end : Position

def Range(val : JsonValue?)
	if val == null | val.value is _null
		return [[Range start = [[Position]], end = [[Position]] ]]
	return [[Range start = Position(joj(val, "start")), end = Position(joj(val, "end")) ]]

def toJson(val : Range)
	return JV({{ "start" => toJson(val.start); "end" => toJson(val.end) }})

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#location
struct Location
    uri : DocumentUri
    _range : Range

def toJson(val : Location)
	return JV({{ "uri" => JV(val.uri); "range" => toJson(val._range)}})

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#locationLink
struct LocaltionLink
    originSelectionRange : Range?
    targetUri : DocumentUri
    targetRange : Range
    targetSelectionRange : Range

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocumentItem
struct TextDocumentItem
    uri : DocumentUri
    languageId : string
    version : double
    text : string

def TextDocumentItem(val : JsonValue?)
    return [[TextDocumentItem uri = jos(val, "uri"), languageId = jos(val, "languageId"), version = jon(val, "version"), text = jos(val, "text") ]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocumentIdentifier
struct TextDocumentIdentifier
    uri : DocumentUri

def TextDocumentIdentifier(val : JsonValue?)
	return [[TextDocumentIdentifier uri = jos(val, "uri")]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_synchronization
struct DidOpenTextDocumentParams
    textDocument : TextDocumentItem

def DidOpenTextDocumentParams(val : JsonValue?)
    return [[DidOpenTextDocumentParams textDocument = TextDocumentItem(joj(val, "textDocument")) ]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_didClose
struct DidCloseTextDocumentParams
	textDocument : TextDocumentIdentifier

def DidCloseTextDocumentParams(val : JsonValue?)
	return [[DidCloseTextDocumentParams textDocument = TextDocumentIdentifier(joj(val, "textDocument")) ]]

struct VersionedTextDocumentIdentifier : TextDocumentIdentifier
	version : double?

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_didChange
def VersionedTextDocumentIdentifier(val : JsonValue?)
	return [[VersionedTextDocumentIdentifier uri = jos(val, "uri"), version = jonn(val, "version") ]]

struct TextDocumentContentChangeEvent
	_range : Range
	rangeLength : double?
	text : string

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_didChange
def TextDocumentContentChangeEvent(val : JsonValue?)
	return [[TextDocumentContentChangeEvent _range = Range(joj(val, "range")), rangeLength = jonn(val, "rangeLength"), text = jos(val, "text") ]]

struct DidChangeTextDocumentParams
	textDocument : VersionedTextDocumentIdentifier
	contentChanges : array<TextDocumentContentChangeEvent>

[unsafe]
def DidChangeTextDocumentParams(val : JsonValue?)
	var contentChanges : array<TextDocumentContentChangeEvent>
	let changesVal = jojn(val, "contentChanges")
	if changesVal != null
		for it in (*changesVal).value as _array
			contentChanges |> push(TextDocumentContentChangeEvent(it))
	return <- [[DidChangeTextDocumentParams textDocument = VersionedTextDocumentIdentifier(joj(val, "textDocument")), contentChanges <- contentChanges ]]

struct DidSaveTextDocumentParams
	textDocument : TextDocumentIdentifier
	text : string

def DidSaveTextDocumentParams(val : JsonValue?)
	return [[DidSaveTextDocumentParams textDocument = TextDocumentIdentifier(joj(val, "textDocument")), text = jos(val, "text") ]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#diagnostic
enum DiagnosticSeverity
	Error = 1
	Warning = 2
	Information = 3
	Hint = 4

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#diagnostic
enum DiagnosticTag
	Unnecessary = 1
	Deprecated = 2

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#diagnostic
struct DiagnosticRelatedInformation
	location : Location
	message : string

def toJson(val : DiagnosticRelatedInformation)
	return JV({{ "location" => toJson(val.location); "message" => JV(val.message)}})

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#diagnostic
struct Diagnostic
	_range : Range
	severity : DiagnosticSeverity?
	code : double? // or string
	source : string?
	message : string
	tags : array<DiagnosticTag>?
	relatedInformation : array<DiagnosticRelatedInformation>?

def toJson(val : Diagnostic)
	var res <- {{ "range" => toJson(val._range); "message" => JV(val.message) }}
	if val.severity != null
		res["severity"] = JV(double(int(*(val.severity))))
	if val.code != null
		res["code"] = JV(val.code)
	if val.source != null
		res["source"] = JV(val.source)
	if val.relatedInformation != null
		var info : array<JsonValue?>
		for it in *(val.relatedInformation)
			info |> push(toJson(it))
		res["relatedInformation"] = JV(info)
	return JV(res)

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_publishDiagnostics
struct PublishDiagnosticsParams
	uri : DocumentUri
	version : double?
	diagnostics : array<Diagnostic>

def toJson(val : PublishDiagnosticsParams)
	var res <- {{ "uri" => JV(val.uri) }}
	if val.version != null
		res["version"] = JV(*(val.version))
	var diagnostics : array<JsonValue?>
	for it in val.diagnostics
		diagnostics |> push(toJson(it))
	res["diagnostics"] = JV(diagnostics)
	return JV(res)