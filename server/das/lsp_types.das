options indenting = 4
options no_aot = true

require math
require json_tools
require strings
require daslib/json_boost

typedef
    JSTab = table<string; JsonValue?>
    DocumentUri = string


// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize
struct WorkspaceFolder
    name : string
    uri : DocumentUri

def WorkspaceFolder(val : JsonValue?)
    return <- [[WorkspaceFolder name = jos(val, "name"), uri = jos(val, "uri") ]]

struct ClientInfo
    name : string
    version : string

def ClientInfo(val : JsonValue?)
    return <- [[ClientInfo name = jos(val, "name"), version = jos(val, "version")]]

struct TextDocumentSyncClientCapabilities
    dynamicRegistration : bool
    willSave : bool
    willSaveWaitUntil : bool
    didSave : bool

def TextDocumentSyncClientCapabilities(val : JsonValue?)
    return <- [[TextDocumentSyncClientCapabilities
        dynamicRegistration = job(val, "dynamicRegistration"),
        willSave = job(val, "willSave"),
        willSaveWaitUntil = job(val, "willSaveWaitUntil"),
        didSave = job(val, "didSave")
    ]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_completion
struct CompletionClientCapabilitiesCompletionItem
    snippetSupport : bool
    commitCharactersSupport : bool
    documentationFormat : array<string>
    deprecatedSupport : bool
    preselectSupport : bool
    // tagSupport : { valueSet: CompletionItemTag[] }
    // insertReplaceSupport: bool // ?
    // resolveSupport: { properties: string[] }
    // insertTextModeSupport: {	valueSet: InsertTextMode[]; }

def CompletionClientCapabilitiesCompletionItem(val : JsonValue?)
    var documentationFormat : array<string>
    return <- [[CompletionClientCapabilitiesCompletionItem
        snippetSupport = job(val, "snippetSupport"),
        commitCharactersSupport = job(val, "commitCharactersSupport"),
        documentationFormat <- documentationFormat,
        deprecatedSupport = job(val, "deprecatedSupport"),
        preselectSupport = job(val, "preselectSupport")
    ]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_completion
struct CompletionClientCapabilities
    dynamicRegistration : bool
    completionItem : CompletionClientCapabilitiesCompletionItem
    // completionItemKind : array<CompletionItemKind> // { valueSet?: CompletionItemKind[]; }
    contextSupport : bool

def CompletionClientCapabilities(val : JsonValue?)
    return <- [[CompletionClientCapabilities
        dynamicRegistration = job(val, "dynamicRegistration"),
        completionItem <- CompletionClientCapabilitiesCompletionItem(joj(val, "completionItem")),
        contextSupport = job(val, "contextSupport")
    ]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_hover
struct HoverClientCapabilities
    dynamicRegistration : bool
    // contentFormat : array<MarkupKind>

def HoverClientCapabilities(val : JsonValue?)
    return <- [[HoverClientCapabilities
        dynamicRegistration = job(val, "dynamicRegistration")
    ]]

struct PublishDiagnosticsClientCapabilities
    relatedInformation : bool
    // tagSupport : // valueSet: DiagnosticTag[];
    versionSupport : bool

def PublishDiagnosticsClientCapabilities(val : JsonValue?)
    return <- [[PublishDiagnosticsClientCapabilities
        relatedInformation = job(val, "relatedInformation"),
        versionSupport = job(val, "versionSupport")
    ]]

struct TextDocumentClientCapabilities
    synchronization : TextDocumentSyncClientCapabilities
    completion : CompletionClientCapabilities
    hover : HoverClientCapabilities
    // signatureHelp : SignatureHelpClientCapabilities
    // declaration : DeclarationClientCapabilities
    // definition : DefinitionClientCapabilities
    // typeDefinition : TypeDefinitionClientCapabilities
    // implementation : ImplementationClientCapabilities
    // references : ReferenceClientCapabilities
    // documentHighlight : DocumentHighlightClientCapabilities
    // documentSymbol : DocumentSymbolClientCapabilities
    // codeAction : CodeActionClientCapabilities
    // codeLens : CodeLensClientCapabilities
    // documentLink : DocumentLinkClientCapabilities
    // colorProvider : DocumentColorClientCapabilities
    // formatting : DocumentFormattingClientCapabilities
    // rangeFormatting : DocumentRangeFormattingClientCapabilities
    // onTypeFormatting : DocumentOnTypeFormattingClientCapabilities
    // rename : RenameClientCapabilities
    publishDiagnostics : PublishDiagnosticsClientCapabilities
    // foldingRange : FoldingRangeClientCapabilities
    // selectionRange : SelectionRangeClientCapabilities

def TextDocumentClientCapabilities(val : JsonValue?)
    return <- [[TextDocumentClientCapabilities
        synchronization <- TextDocumentSyncClientCapabilities(joj(val, "synchronization")),
        completion <- CompletionClientCapabilities(joj(val, "completion")),
        hover <- HoverClientCapabilities(joj(val, "hover")),
        publishDiagnostics <- PublishDiagnosticsClientCapabilities(joj(val, "publishDiagnostics"))
    ]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#workspaceEditClientCapabilities
struct WorkspaceEditClientCapabilities
    documentChanges : bool
    resourceOperations : array<string> // ResourceOperationKind = 'create' | 'rename' | 'delete'
    failureHandling : string // FailureHandlingKind = 'abort' | 'transactional' | 'undo' | 'textOnlyTransactional';

def WorkspaceEditClientCapabilities(val : JsonValue?)
    var resourceOperations : array<string>
    var resourceOperationsVal = joj(val, "resourceOperations")
    if resourceOperationsVal != null
        for it in resourceOperationsVal.value as _array
            resourceOperations |> push(j_s(it))
    return <- [[WorkspaceEditClientCapabilities
        documentChanges = job(val, "documentChanges"),
        resourceOperations <- resourceOperations,
        failureHandling = jos(val, "failureHandling")
    ]]


// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#workspace_didChangeConfiguration
struct DidChangeConfigurationClientCapabilities
    dynamicRegistration : bool

def DidChangeConfigurationClientCapabilities(val : JsonValue?)
    return <- [[DidChangeConfigurationClientCapabilities
        dynamicRegistration = job(val, "dynamicRegistration")
    ]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#workspace_didChangeWatchedFiles
struct DidChangeWatchedFilesClientCapabilities
    dynamicRegistration : bool

def DidChangeWatchedFilesClientCapabilities(val : JsonValue?)
    return <- [[DidChangeWatchedFilesClientCapabilities
        dynamicRegistration = job(val, "dynamicRegistration")
    ]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#workspace_symbol
struct WorkspaceSymbolClientCapabilities
    dynamicRegistration : bool
    // symbolKind // { valueSet?: SymbolKind[]; }

def WorkspaceSymbolClientCapabilities(val : JsonValue?)
    return <- [[WorkspaceSymbolClientCapabilities
        dynamicRegistration = job(val, "dynamicRegistration")
    ]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#workspace_executeCommand
struct ExecuteCommandClientCapabilities
    dynamicRegistration : bool

def ExecuteCommandClientCapabilities(val : JsonValue?)
    return <- [[ExecuteCommandClientCapabilities
        dynamicRegistration = job(val, "dynamicRegistration")
    ]]

struct ClientCapabilitiesWorkspace
    applyEdit : bool
    workspaceEdit : WorkspaceEditClientCapabilities
    didChangeConfiguration : DidChangeConfigurationClientCapabilities
    didChangeWatchedFiles : DidChangeWatchedFilesClientCapabilities
    symbol : WorkspaceSymbolClientCapabilities
    executeCommand : ExecuteCommandClientCapabilities
    workspaceFolders : bool
    configuration : bool

def ClientCapabilitiesWorkspace(val : JsonValue?)
    return <- [[ClientCapabilitiesWorkspace
        applyEdit = job(val, "applyEdit"),
        workspaceEdit <- WorkspaceEditClientCapabilities(joj(val, "workspaceEdit")),
        didChangeConfiguration <- DidChangeConfigurationClientCapabilities(joj(val, "didChangeConfiguration")),
        didChangeWatchedFiles <- DidChangeWatchedFilesClientCapabilities(joj(val, "didChangeWatchedFiles")),
        symbol <- WorkspaceSymbolClientCapabilities(joj(val, "symbol")),
        executeCommand <- ExecuteCommandClientCapabilities(joj(val, "executeCommand")),
        workspaceFolders = job(val, "workspaceFolders"),
        configuration = job(val, "configuration")
    ]]

struct ClientCapabilitiesWindow
    workDoneProgress : bool

def ClientCapabilitiesWindow(val : JsonValue?)
    return <- [[ClientCapabilitiesWindow
        workDoneProgress = job(val, "workDoneProgress")
    ]]

struct ClientCapabilities
    workspace : ClientCapabilitiesWorkspace
    textDocument : TextDocumentClientCapabilities
    window : ClientCapabilitiesWindow
    // experimental : void?

def ClientCapabilities(val : JsonValue?)
    return <- [[ClientCapabilities
        workspace <- ClientCapabilitiesWorkspace(joj(val, "workspace")),
        textDocument <- TextDocumentClientCapabilities(joj(val, "textDocument")),
        window <- ClientCapabilitiesWindow(joj(val, "window"))
    ]]

struct InitializeParams
    processId : double
    clientInfo : ClientInfo
    // rootPath : string // deprecated
    rootUri : string
    // initializationOptions : any?
    capabilities : ClientCapabilities
    trace : string // 'off' | 'messages' | 'verbose'
    workspaceFolders : array<WorkspaceFolder>

def InitializeParams(val : JsonValue?)
    var workspaceFolders : array<WorkspaceFolder>
    let workspaceFoldersVal = joj(val, "workspaceFolders")
    if workspaceFoldersVal != null
        for it in workspaceFoldersVal.value as _array
            workspaceFolders |> emplace(WorkspaceFolder(it))
    return <- [[InitializeParams
        processId = jon(val, "processId"),
        clientInfo <- ClientInfo(joj(val, "clientInfo")),
        capabilities <- ClientCapabilities(joj(val, "capabilities")),
        rootUri = jos(val, "rootUri"),
        trace = jos(val, "trace"),
        workspaceFolders <- workspaceFolders
    ]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize
struct ServerInfo
    name : string
    version : string // ?

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_synchronization
enum TextDocumentSyncKind
    None = 0
    Full = 1
    Incremental = 2

def JV(val : TextDocumentSyncKind)
    return JV(int(val))

struct CompletionOptions
    triggerCharacters : array<string> // ?
    allCommitCharacters : array<string> // ?
    resolveProvider : bool

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#workspace_workspaceFolders
struct WorkspaceFoldersServerCapabilities
    supported : bool
    changeNotifications : bool

struct ServerCapabilitiesWorkspace
    workspaceFolders : WorkspaceFoldersServerCapabilities

struct SignatureHelpOptions
    triggerCharacters : array<string>
    // retriggerCharacters : array<string>

struct RenameOptions
    prepareProvider : bool

struct SemanticTokensLegend
    tokenTypes : array<string>
    tokenModifiers : array<string>

struct SemanticTokensOptions
    legend : SemanticTokensLegend
    full : bool
    // range: bool

let
    SemanticToken_type = "type"
    SemanticToken_class = "class"
    SemanticToken_enum = "enum"
    SemanticToken_interface = "interface"
    SemanticToken_struct = "struct"
    SemanticToken_typeParameter = "typeParameter"
    SemanticToken_parameter = "parameter"
    SemanticToken_variable = "variable"
    SemanticToken_property = "property"
    SemanticToken_enumMember = "enumMember"
    SemanticToken_event = "event"
    SemanticToken_function = "function"
    SemanticToken_method = "method"
    SemanticToken_macro = "macro"
    SemanticToken_keyword = "keyword"
    SemanticToken_modifier = "modifier"
    SemanticToken_comment = "comment"
    SemanticToken_string = "string"
    SemanticToken_number = "number"
    SemanticToken_regexp = "regexp"
    SemanticToken_operator = "operator"

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize
struct ServerCapabilities
    textDocumentSync : TextDocumentSyncKind // TextDocumentSyncOptions | number; number == TextDocumentSyncKind
    completionProvider : CompletionOptions
    hoverProvider : bool // | HoverOptions
    signatureHelpProvider : SignatureHelpOptions
    // declarationProvider : bool // DeclarationOptions
    definitionProvider : bool // DefinitionOptions
    typeDefinitionProvider : bool // TypeDefinitionOptions
    // implementationProvider : ImplementationOptions
    referencesProvider : bool // ReferenceOptions
    // documentHighlightProvider : DocumentHighlightOptions
    documentSymbolProvider : bool // DocumentSymbolOptions
    // codeActionProvider : CodeActionOptions
    // codeLensProvider : CodeLensOptions
    documentLinkProvider : bool //DocumentLinkOptions
    colorProvider : bool // DocumentColorOptions
    documentFormattingProvider : bool
    documentRangeFormattingProvider : bool
    // documentOnTypeFormattingProvider : DocumentOnTypeFormattingOptions
    renameProvider : RenameOptions
    // foldingRangeProvider : FoldingRangeOptions
    // executeCommandProvider : ExecuteCommandOptions
    // selectionRangeProvider : SelectionRangeOptions
    semanticTokensProvider : SemanticTokensOptions
    workspaceSymbolProvider : bool
    workspace : ServerCapabilitiesWorkspace
    // experimental : void?

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize
struct InitializeResult
    capabilities : ServerCapabilities
    serverInfo : ServerInfo // ?

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#responseMessage
struct ResponseError
    code : double
    message : string
    data : JsonValue? // ? string | number | boolean | array | object | null

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#position
struct Position
    line : int = 0
    character : int = 0

def Position(line, character : int)
    return <- [[Position line = line, character = character]]

def Position(val : JsonValue?)
    if val == null || val.value is _null
        return <- [[Position]]
    return <- [[Position line = int(jon(val, "line")), character = int(jon(val, "character")) ]]

def operator <(a, b : Position)
    return a.line < b.line || (a.line == b.line && a.character < b.character)

def operator >(a, b : Position)
    return a.line > b.line || (a.line == b.line && a.character > b.character)

def operator <=(a, b : Position)
    return a.line < b.line || (a.line == b.line && a.character <= b.character)

def operator ==(a, b : Position)
    return a.line == b.line && a.character == b.character

def operator !=(a, b : Position)
    return a.line != b.line || a.character != b.character

def pos_to_index(pos : Position; text : string)
    var line = 0
    var charPos = 0
    for i, ch in iter_range(text), text
        if ch == '\n'
            if line == pos.line
                return i // something wrong with char pos, return last char in the line before new line
            line++
        if line == pos.line
            if charPos == pos.character
                return i
            charPos++
    return -1

def index_to_pos(index : int; text : string) : Position
    var line = 0
    var charPos = 0
    for i in range(min(index, length(text)))
        var ch = unsafe(character_uat(text, i))
        if ch == '\n'
            line++
            charPos = 0
            continue
        charPos++
    return [[Position line = line, character = charPos]]

def to_string(val : Position)
    return "{int(val.line)}:{int(val.character)}"

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#range
struct Range
    start : Position
    end : Position

def Range(start, end : Position)
    return <- [[Range start = start, end = end]]

def Range(start_line, start_char, end_line, end_char : int)
    return <- [[Range start = Position(start_line, start_char), end = Position(end_line, end_char)]]

def Range(val : JsonValue?)
    if val == null || val.value is _null
        return <- [[Range start = [[Position]], end = [[Position]] ]]
    return <- [[Range start <- Position(joj(val, "start")), end <- Position(joj(val, "end")) ]]

def in_range(pos : Position; r : Range)
    return r.start <= pos && pos <= r.end

def in_range(b, a : Range)
    return b.start |> in_range(a) && b.end |> in_range(a)

def overlaps(b, a : Range)
    return b.start |> in_range(a) || b.end |> in_range(a) || a |> in_range(b)

def range_oneline(a : Range)
    return a.start.line == a.end.line

def operator ==(a, b : Range)
    return a.start == b.start && a.end == b.end

def operator !=(a, b : Range)
    return a.start != b.start || a.end != b.end

def is_zero(a : Range)
    return a.start.character == 0 && a.start.line == 0 && a.end.character == 0 && a.end.line == 0

// works only with oneline ranges, @see range_oneline
def length(a : Range)
    return a.end.character - a.start.character

def range_ratio(a : Range)
    return (!range_oneline(a) ? 1000 * (a.end.line - a.start.line) : 0) + a.end.character - a.start.character

def to_string(val : Range)
    return "{to_string(val.start)} - {to_string(val.end)}"

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#location
struct Location
    uri : DocumentUri
    _range : Range

def JV(val : Location)
    return JV({{ "uri" => JV(val.uri); "range" => JV(val._range)}})

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

// def finalize ( var s : TextDocumentItem& )
//     delete s.text

def TextDocumentItem(val : JsonValue?)
    return <- [[TextDocumentItem
        uri = jos(val, "uri"),
        languageId = jos(val, "languageId"),
        version = jon(val, "version"),
        text = jos(val, "text")
    ]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocumentIdentifier
struct TextDocumentIdentifier
    uri : DocumentUri

def TextDocumentIdentifier(val : JsonValue?)
    return <- [[TextDocumentIdentifier uri = jos(val, "uri")]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_didOpen
struct DidOpenTextDocumentParams
    textDocument : TextDocumentItem

def DidOpenTextDocumentParams(val : JsonValue?)
    return <- [[DidOpenTextDocumentParams textDocument <- TextDocumentItem(joj(val, "textDocument")) ]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_didClose
struct DidCloseTextDocumentParams
    textDocument : TextDocumentIdentifier

def DidCloseTextDocumentParams(val : JsonValue?)
    return <- [[DidCloseTextDocumentParams textDocument <- TextDocumentIdentifier(joj(val, "textDocument")) ]]

struct VersionedTextDocumentIdentifier : TextDocumentIdentifier
    version : double // ?

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_didChange
def VersionedTextDocumentIdentifier(val : JsonValue?)
    return <- [[VersionedTextDocumentIdentifier uri = jos(val, "uri"), version = jon(val, "version", -1lf) ]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_didChange
struct TextDocumentContentChangeEvent
    _range : Range
    rangeLength : double // ?
    text : string

def TextDocumentContentChangeEvent(val : JsonValue?)
    return <- [[TextDocumentContentChangeEvent _range = Range(joj(val, "range")), rangeLength = jon(val, "rangeLength", -1lf), text = jos(val, "text") ]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_didChange
struct DidChangeTextDocumentParams
    textDocument : VersionedTextDocumentIdentifier
    contentChanges : array<TextDocumentContentChangeEvent>

def DidChangeTextDocumentParams(val : JsonValue?)
    var contentChanges : array<TextDocumentContentChangeEvent>
    let changesVal = joj(val, "contentChanges")
    if changesVal != null
        for it in changesVal.value as _array
            contentChanges |> emplace(TextDocumentContentChangeEvent(it))
    return <- [[DidChangeTextDocumentParams
        textDocument <- VersionedTextDocumentIdentifier(joj(val, "textDocument")),
        contentChanges <- contentChanges
    ]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_didSave
struct DidSaveTextDocumentParams
    textDocument : TextDocumentIdentifier
    text : string // ?

def DidSaveTextDocumentParams(val : JsonValue?)
    return <- [[DidSaveTextDocumentParams textDocument = TextDocumentIdentifier(joj(val, "textDocument")), text = jos(val, "text") ]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#diagnostic
enum DiagnosticSeverity
    Error = 1
    Warning = 2
    Information = 3
    Hint = 4

def JV(val : DiagnosticSeverity)
    return JV(int(val))

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#diagnostic
enum DiagnosticTag
    Unnecessary = 1
    Deprecated = 2

def JV(val : DiagnosticTag)
    return JV(int(val))

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#diagnostic
struct DiagnosticRelatedInformation
    location : Location
    message : string

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

def JV(val : Diagnostic)
    var res <- {{ "range" => JV(val._range); "message" => JV(val.message); "code" => JV(val.code); "severity" => JV(val.severity) }}
    if !empty(val.source)
        res["source"] = JV(val.source)
    if length(val.relatedInformation) > 0
        res["relatedInformation"] = JV([{ for it in val.relatedInformation; JV(it) }])
    return JV(res)

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_publishDiagnostics
struct PublishDiagnosticsParams
    uri : DocumentUri
    version : double?
    diagnostics : array<Diagnostic>


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

def JV(val : CompletionItemKind)
    return JV(int(val))

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
    return <- [[CompletionParams textDocument <- TextDocumentIdentifier(joj(val, "textDocument")), position <- Position(joj(val, "position"))]]

struct HoverParams : TextDocumentPositionParams {}

def HoverParams(val : JsonValue?)
    return <- [[HoverParams textDocument <- TextDocumentIdentifier(joj(val, "textDocument")), position <- Position(joj(val, "position"))]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_hover
struct Hover
    contents : array<string>
    _range : Range

def JV(val : Hover)
    return JV({{ "contents" => JV([{ for it in val.contents; JV(it) }]); "range" => JV(val._range) }})


struct DefinitionParams : TextDocumentPositionParams {}

def DefinitionParams(val : JsonValue?)
    return <- [[DefinitionParams
        textDocument <- TextDocumentIdentifier(joj(val, "textDocument")),
        position <- Position(joj(val, "position"))
    ]]

struct TypeDefinitionParams : TextDocumentPositionParams {}

def TypeDefinitionParams(val : JsonValue?)
    return <- [[TypeDefinitionParams
        textDocument <- TextDocumentIdentifier(joj(val, "textDocument")),
        position <- Position(joj(val, "position"))
    ]]

struct DocumentSymbolParams
    textDocument : TextDocumentIdentifier

def DocumentSymbolParams(val : JsonValue?)
    return <- [[DocumentSymbolParams textDocument = TextDocumentIdentifier(joj(val, "textDocument")) ]]

struct WorkspaceSymbolParams
    query : string

def WorkspaceSymbolParams(val : JsonValue?)
    return [[WorkspaceSymbolParams query=jos(val, "query") ]]

enum MessageType
    Error = 1
    Warning = 2
    Info = 3
    Log = 4

def JV(val : MessageType)
    return JV(int(val))

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#window_logMessage
struct LogMessageParams
    _type : MessageType
    message : string

def JV(val : LogMessageParams)
    return JV({{ "type" => JV(val._type); "message" => JV(val.message) }})

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#workspace_didChangeWorkspaceFolders
struct WorkspaceFoldersChangeEvent
    added : array<WorkspaceFolder>
    removed : array<WorkspaceFolder>

def WorkspaceFoldersChangeEvent(val : JsonValue?)
    var res : WorkspaceFoldersChangeEvent
    var added = joj(val, "added")
    if added != null
        for it in added.value as _array
            res.added |> emplace(WorkspaceFolder(it))
    var removed = joj(val, "removed")
    if removed != null
        for it in removed.value as _array
            res.removed |> emplace(WorkspaceFolder(it))
    return <- res

struct DidChangeWorkspaceFoldersParams
    event : WorkspaceFoldersChangeEvent

def DidChangeWorkspaceFoldersParams(val : JsonValue?)
    return <- [[DidChangeWorkspaceFoldersParams event <- WorkspaceFoldersChangeEvent(joj(val, "event"))]]


struct Registration
    id : string
    method : string
    // registerOptions : any

struct RegistrationParams
    registrations : array<Registration>

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#workspace_configuration
struct ConfigurationItem
    scopeUri : string // ?
    section : string // ?

struct ConfigurationParams
    items : array<ConfigurationItem>

struct ReferenceContext
    includeDeclaration : bool

def ReferenceContext(val : JsonValue?)
    return [[ReferenceContext includeDeclaration=job(val, "includeDeclaration")]]

// https://microsoft.github.io/language-server-protocol/specification#textDocument_references
struct ReferenceParams : TextDocumentPositionParams
    context : ReferenceContext
// textDocument : TextDocumentIdentifier
//     position : Position

def ReferenceParams(val : JsonValue?)
    return <- [[ReferenceParams
        textDocument <- TextDocumentIdentifier(joj(val, "textDocument")),
        position <- Position(joj(val, "position")),
        context <- ReferenceContext(joj(val, "contenxt"))
    ]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_signatureHelp
struct SignatureHelpParams  : TextDocumentPositionParams {}
    // context : SignatureHelpContext

def SignatureHelpParams(val : JsonValue?)
    return <- [[SignatureHelpParams
        textDocument <- TextDocumentIdentifier(joj(val, "textDocument")),
        position <- Position(joj(val, "position"))
    ]]

struct SignatureInformation
    _label : string
    // documentation : string // string | MarkupContent;
    // parameters?: ParameterInformation[];

def JV(val : SignatureInformation)
    return JV({{
        "label" => JV(val._label)//; "documentation" => JV(val.documentation)
    }})

struct SignatureHelp
    signatures : array<SignatureInformation>


// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_documentColor
struct DocumentColorParams
    textDocument : TextDocumentIdentifier

def DocumentColorParams(val : JsonValue?)
    return <- [[DocumentColorParams
        textDocument <- TextDocumentIdentifier(joj(val, "textDocument"))
    ]]

struct Color
    red : double
    green : double
    blue : double
    alpha : double

def Color(val : JsonValue?)
    return [[Color
        red = jon(val, "red"),
        green = jon(val, "green"),
        blue = jon(val, "blue"),
        alpha = jon(val, "alpha")
    ]]

def to_string(c : Color; len : int) : string
    return build_string() <| $(var str)
        write(str, "0x")
        if len > 8
            format(str, "%02X", int(c.alpha * 255lf))
        format(str, "%02X", int(c.red * 255lf))
        format(str, "%02X", int(c.green * 255lf))
        format(str, "%02X", int(c.blue * 255lf))

struct ColorInformation
    _range : Range
    color : Color

def JV(val : ColorInformation)
    return JV({{"range" => JV(val._range); "color" => JV(val.color) }})

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_colorPresentation
struct ColorPresentationParams
    textDocument : TextDocumentIdentifier
    color : Color
    _range : Range

def ColorPresentationParams(val : JsonValue?)
    return <- [[ColorPresentationParams
        textDocument <- TextDocumentIdentifier(joj(val, "textDocument")),
        color <- Color(joj(val, "color")),
        _range <- Range(joj(val, "range"))
    ]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_colorPresentation
struct ColorPresentation
    _label : string
    // textEdit?: TextEdit;
    // additionalTextEdits?: TextEdit[];

def JV(val : ColorPresentation)
    return JV({{ "label" => JV(val._label) }})

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textEdit
struct TextEdit
    _range : Range
    newText : string

def JV(val : TextEdit)
    return JV({{ "range" => JV(val._range); "newText" => JV(val.newText) }})


// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_rename
struct RenameParams : TextDocumentPositionParams
    newName : string

def RenameParams(val : JsonValue?)
    return <- [[RenameParams
        textDocument <- TextDocumentIdentifier(joj(val, "textDocument")),
        position <- Position(joj(val, "position")),
        newName = jos(val, "newName")
    ]]

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_prepareRename
struct PrepareRenameParams : TextDocumentPositionParams {}

def PrepareRenameParams(val : JsonValue?)
    return <- [[PrepareRenameParams
        textDocument <- TextDocumentIdentifier(joj(val, "textDocument")),
        position <- Position(joj(val, "position"))
    ]]

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_formatting
struct FormattingOptions
    tabSize : double
    insertSpaces : bool

def FormattingOptions(val : JsonValue?)
    return <- [[FormattingOptions
        tabSize = jon(val, "tabSize"),
        insertSpaces = job(val, "insertSpaces")
    ]]

struct DocumentFormattingParams
    textDocument : TextDocumentIdentifier
    _options : FormattingOptions

def DocumentFormattingParams(val : JsonValue?)
    return <- [[DocumentFormattingParams
        textDocument <- TextDocumentIdentifier(joj(val, "textDocument")),
        _options <- FormattingOptions(joj(val, "options"))
    ]]

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_rangeFormatting
struct DocumentRangeFormattingParams
    textDocument : TextDocumentIdentifier
    _range : Range
    _options : FormattingOptions

def DocumentRangeFormattingParams(val : JsonValue?)
    return <- [[DocumentRangeFormattingParams
        textDocument <- TextDocumentIdentifier(joj(val, "textDocument")),
        _range <- Range(joj(val, "range")),
        _options <- FormattingOptions(joj(val, "options"))
    ]]

struct WorkspaceEdit
    changes : table<string; array<TextEdit>>

// https://microsoft.github.io/language-server-protocol/specifications/specification-3-17/#semanticTokensParams
struct SemanticTokensParams
    textDocument : TextDocumentIdentifier


def SemanticTokensParams(val : JsonValue?)
    return <- [[SemanticTokensParams
        textDocument <- TextDocumentIdentifier(joj(val, "textDocument"))
    ]]

struct SemanticTokens
    data : array<int>