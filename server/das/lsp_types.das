// options heap_page = 0
// options string_heap_page = 0

// options log = true
require daslib/json
require json_tools
require uriparser

typedef
	JSTab = table<string; JsonValue?>
    DocumentUri = string

// def urlDecode(str : string) : string
// 	return join(split(str, "%3A"), ":")

// def urlEncode(str : string) : string
// 	return join(split(str, ":"), "%3A")

// [unsafe]
// def uri_to_path(str : string)
// 	let path = urlDecode(str)
// 	return path |> character_at(9) == ':' ? chop(path, 8, length(path) - 8) : chop(path, 7, length(path) - 7)

// def path_to_uri(str : string)
// 	let prefix = str |> character_at(1) == ':' ? "file:///" : "file://"
// 	return "{prefix}{urlEncode(str)}"

def uri_to_path(str : string)
	return uri_to_file_name(str)

def path_to_uri(str : string)
	return file_name_to_uri(str) // escape_uri(str, false, false)

def js(val : int)
	return new [[JsonValue value <- [[JsValue _number = double(val)]]]]

def js(val : bool)
	return new [[JsonValue value <- [[JsValue _bool = val]]]]

def js(val : double)
	return new [[JsonValue value <- [[JsValue _number = val]]]]

def js(val : string)
	return new [[JsonValue value <- [[JsValue _string = val]]]]

def js(var val : JSTab)
	return new [[JsonValue value <- [[JsValue _object <- val]]]]

def js(var val : array<JsonValue?>)
	return new [[JsonValue value <- [[JsValue _array <- val]]]]

def js()
	return new [[JsonValue value <- [[JsValue _null = null]]]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize
struct ServerInfo
	name : string
	version : string // ?

def js(val : ServerInfo)
	return js({{ "name" => js(val.name); "version" => js(val.version) }})

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_synchronization
enum TextDocumentSyncKind
	None = 0
	Full = 1
	Incremental = 2

struct CompletionOptions
	triggerCharacters : array<string> // ?
	allCommitCharacters : array<string> // ?
	resolveProvider : bool

def js(val : CompletionOptions)
	var res <- {{ "resolveProvider" => js(val.resolveProvider) }} // TODO:
	return js(res)

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize
struct ServerCapabilities
	textDocumentSync : TextDocumentSyncKind // TextDocumentSyncOptions | number; number == TextDocumentSyncKind
	completionProvider : CompletionOptions
	hoverProvider : bool
	// TODO:

def js(val : ServerCapabilities)
	return js({{
		"textDocumentSync" => js(int(val.textDocumentSync));
		"completionProvider" => js(val.completionProvider);
		"hoverProvider" => js(val.hoverProvider)
	}})

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize
struct InitializeResult
	capabilities : ServerCapabilities
	serverInfo : ServerInfo // ?

def js(val : InitializeResult)
	return js({{ "capabilities" => js(val.capabilities); "serverInfo" => js(val.serverInfo) }})

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#responseMessage
struct ResponseError
    code : double
    message : string
    data : JsonValue? // ? string | number | boolean | array | object | null

def js(val : ResponseError)
	return js({{ "code" => js(val.code); "message" => js(val.message); "data" => val.data }})

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#position
struct Position
    line : int = 0
    character : int = 0

def Position(val : JsonValue?)
	if val == null | val.value is _null
		return [[Position]]
	return [[Position line = int(jon(val, "line")), character = int(jon(val, "character")) ]]

def js(val : Position)
	return js({{ "line" => js(double(val.line)); "character" => js(double(val.character)) }})

def operator <(a, b : Position)
	return a.line < b.line | (a.line == b.line & a.character < b.character)

def operator <=(a, b : Position)
	return a.line < b.line | (a.line == b.line & a.character <= b.character)

def to_string(val : Position)
	return "{int(val.line)}:{int(val.character)}"
// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#range
struct Range
    start : Position
    end : Position

def Range(val : JsonValue?)
	if val == null | val.value is _null
		return [[Range start = [[Position]], end = [[Position]] ]]
	return [[Range start = Position(joj(val, "start")), end = Position(joj(val, "end")) ]]

def range_in(r : Range; pos : Position)
	return r.start <= pos & pos <= r.end

def range_in(a, b : Range)
	return a.start <= b.start & b.end <= a.end

def js(val : Range)
	return JV({{ "start" => js(val.start); "end" => js(val.end) }})

def to_string(val : Range)
	return "{to_string(val.start)} - {to_string(val.end)}"

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#location
struct Location
    uri : DocumentUri
    _range : Range

def js(val : Location)
	return JV({{ "uri" => js(val.uri); "range" => js(val._range)}})

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#locationLink
struct LocaltionLink
    originSelectionRange : Range // ?
    targetUri : DocumentUri
    targetRange : Range
    targetSelectionRange : Range

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocumentItem
struct TextDocumentItem
    uri : DocumentUri
    languageId : string
    version : double
    text : string

// [unsafe] def finalize ( var s : TextDocumentItem& )
// 	delete s.text

def TextDocumentItem(val : JsonValue?)
    return [[TextDocumentItem uri = jos(val, "uri"), languageId = jos(val, "languageId"), version = jon(val, "version"), text = jos(val, "text") ]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocumentIdentifier
struct TextDocumentIdentifier
    uri : DocumentUri

def TextDocumentIdentifier(val : JsonValue?)
	return [[TextDocumentIdentifier uri = jos(val, "uri")]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_didOpen
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
	version : double // ?

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_didChange
def VersionedTextDocumentIdentifier(val : JsonValue?)
	return [[VersionedTextDocumentIdentifier uri = jos(val, "uri"), version = jon(val, "version", -1lf) ]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_didChange
struct TextDocumentContentChangeEvent
	_range : Range
	rangeLength : double // ?
	text : string

def TextDocumentContentChangeEvent(val : JsonValue?)
	return [[TextDocumentContentChangeEvent _range = Range(joj(val, "range")), rangeLength = jon(val, "rangeLength", -1lf), text = jos(val, "text") ]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_didChange
struct DidChangeTextDocumentParams
	textDocument : VersionedTextDocumentIdentifier
	contentChanges : array<TextDocumentContentChangeEvent>

def DidChangeTextDocumentParams(val : JsonValue?)
	var contentChanges : array<TextDocumentContentChangeEvent>
	let changesVal = joj(val, "contentChanges")
	if changesVal != null
		for it in changesVal.value as _array
			contentChanges |> push(TextDocumentContentChangeEvent(it))
	return <- [[DidChangeTextDocumentParams textDocument = VersionedTextDocumentIdentifier(joj(val, "textDocument")), contentChanges <- contentChanges ]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_didSave
struct DidSaveTextDocumentParams
	textDocument : TextDocumentIdentifier
	text : string // ?

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

def js(val : DiagnosticRelatedInformation)
	return js({{ "location" => js(val.location); "message" => js(val.message)}})

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#diagnostic
variant DiagnosticsCode
	_double : double
	_string : string
	_null : void?

struct Diagnostic
	_range : Range
	severity : DiagnosticSeverity // ?
	code : double // ? number | string
	source : string // ?
	message : string
	tags : array<DiagnosticTag> // TODO:
	relatedInformation : array<DiagnosticRelatedInformation>

def js(val : Diagnostic)
	var res <- {{ "range" => js(val._range); "message" => js(val.message); "code" => js(val.code); "severity" => js(int(val.severity)) }}
	if val.source != ""
		res["source"] = js(val.source)
	if length(val.relatedInformation) > 0
		var info : array<JsonValue?>
		for it in val.relatedInformation
			info |> push(js(it))
		res["relatedInformation"] = js(info)
	return JV(res)

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_publishDiagnostics
struct PublishDiagnosticsParams
	uri : DocumentUri
	version : double?
	diagnostics : array<Diagnostic>

def js(val : PublishDiagnosticsParams)
	var res <- {{ "uri" => JV(val.uri) }}
	if val.version != null
		res["version"] = JV(*(val.version))
	var diagnostics : array<JsonValue?>
	for it in val.diagnostics
		diagnostics |> push(js(it))
	res["diagnostics"] = JV(diagnostics)
	return JV(res)

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocumentPositionParams
struct TextDocumentPositionParams
	textDocument : TextDocumentIdentifier
	position : Position

enum CompletionItemKind
	Text = 1
	Method = 2
	Function = 3
	Constructor = 4
	Field = 5
	Variable = 6
	Class = 7
	Interface = 8
	Module = 9
	Property = 10
	Unit = 11
	Value = 12
	Enum = 13
	Keyword = 14
	Snippet = 15
	Color = 16
	File = 17
	Reference = 18
	Folder = 19
	EnumMember = 20
	Constant = 21
	Struct = 22
	Event = 23
	Operator = 24
	TypeParameter = 25

struct CompletionItem
	_label : string
	kind : CompletionItemKind // ?
	// tags : array<CompletionItemTag>
	detail : string
	documentation : string // ?
	// deprecated : bool // ?
	// preselect : bool // ?
	sortText : string // ?
	filterText : string // ?
	insertText : string // ?
	// TODO

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_completion
struct CompletionList
	isIncomplete : bool = false
	items : array<CompletionItem>

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_completion
struct CompletionParams : TextDocumentPositionParams {}
	// context : void? //{ triggerKind : 1}

def CompletionParams(val : JsonValue?)
	return [[CompletionParams textDocument = TextDocumentIdentifier(joj(val, "textDocument")), position = Position(joj(val, "position"))]]

struct HoverParams : TextDocumentPositionParams {}

def HoverParams(val : JsonValue?)
	return [[HoverParams textDocument = TextDocumentIdentifier(joj(val, "textDocument")), position = Position(joj(val, "position"))]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_hover
struct Hover
	contents : array<string>
	_range : Range

def js(val : Hover)
	var contents : array<JsonValue?>
	for it in val.contents
		contents |> emplace(js(it))
	return js({{ "contents" => js(contents); "range" => js(val._range)  }})