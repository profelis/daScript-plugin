import {
	createConnection, TextDocuments, ProposedFeatures, TextDocumentSyncKind, DiagnosticSeverity, Diagnostic, Position, TextDocument, DidChangeConfigurationNotification, WorkspaceFolder, TextDocumentPositionParams, CompletionItem, CompletionItemKind
} from 'vscode-languageserver'
import {
	execFile
} from 'child_process'

import {
	isAbsolute, join
} from 'path'

import {
	existsSync
} from 'fs'

let connection = createConnection(ProposedFeatures.all)
let documents = new TextDocuments()

let workspaceFolders: WorkspaceFolder[] | null

let hasConfigurationCapability = false
let hasDiagnosticRelatedInformationCapability = false

interface DascriptSettings {
	compiler: string
	compilerArgs: Array<string>
	projectRoots: Array<string>
}

const defaultSettings: DascriptSettings = { compiler: "daScript", compilerArgs: ["${file}"], projectRoots: [] }
let globalSettings: DascriptSettings = defaultSettings

let documentSettings: Map<string /*uri*/, Thenable<DascriptSettings>> = new Map()
let dependencies: Map<string /*uri*/, Array<string>> = new Map()
let lazyCompletions: Map<string /*uri*/, CompletionItem[]> = new Map()
let globalCompletion: CompletionItem[] = []
let globalCompletionKeys: Set<string> = new Set()

connection.onDidChangeConfiguration(change => {
	if (hasConfigurationCapability)
		documentSettings.clear()
	else
		globalSettings = <DascriptSettings>((change.settings.dascript ?? defaultSettings))
	documents.all().forEach(validate)
})

function getDocumentSettings(uri: string): Thenable<DascriptSettings> {
	if (!hasConfigurationCapability)
		return Promise.resolve(globalSettings)
	let result = documentSettings.get(uri)
	if (!result) {
		result = connection.workspace.getConfiguration({ scopeUri: uri, section: "dascript" })
		documentSettings.set(uri, result)
	}
	return result
}

connection.onInitialize((params) => {
	let capabilities = params.capabilities
	hasConfigurationCapability = !!capabilities.workspace?.configuration
	hasDiagnosticRelatedInformationCapability = !!capabilities.textDocument?.publishDiagnostics?.relatedInformation

	workspaceFolders = params.workspaceFolders
	connection.console.log(`[server: (${process.pid}) dirs: ${JSON.stringify(workspaceFolders)}]`)
	return {
		capabilities: {
			textDocumentSync: TextDocumentSyncKind.Full,
			completionProvider: { resolveProvider: true }
		}
	}
})

connection.onInitialized(() => {
	if (hasConfigurationCapability)
		connection.client.register(DidChangeConfigurationNotification.type, undefined)
})

connection.onCompletion((doc: TextDocumentPositionParams): CompletionItem[] => {
	let lazy = lazyCompletions.has(doc.textDocument.uri) ? lazyCompletions.get(doc.textDocument.uri) : []
	if (!lazy)
		return globalCompletion
	var res = globalCompletion.slice()
	lazy.forEach(it => { if (!globalCompletionKeys.has(it.label)) res.push(it) })
	return res
})

connection.onCompletionResolve((completion: CompletionItem): CompletionItem => {
	return completion
})

documents.onDidClose(event => {
	documentSettings.delete(event.document.uri)
	connection.sendDiagnostics({ diagnostics: [], uri: event.document.uri })
	for (const it of dependencies.get(event.document.uri) ?? [])
		connection.sendDiagnostics({ diagnostics: [], uri: it })
})

documents.onDidOpen(event => {
	connection.console.log(`[open ${event.document.uri}]`)
	validate(event.document)
	lazyCompletion(event.document)
})

documents.onDidSave(event => {
	connection.console.log(`[save ${event.document.uri}]`)
	validate(event.document)
	lazyCompletion(event.document)
})

documents.onDidChangeContent(event => {
	connection.console.log(`[changed ${event.document.uri}]`)
	lazyCompletion(event.document)
})

documents.listen(connection)
connection.listen()

function lazyCompletion(doc: TextDocument) {
	let text = doc.getText()
	let tokens = new Set<string>()
	let last: string
	var tokenEreg = /\w+/g
	var m: RegExpExecArray
	do {
		m = tokenEreg.exec(text)
		if (!m)
			break;
		let t = m.toString()
		if (!globalCompletionKeys.has(t))
			tokens.add(last = t)
	} while (true)
	if (last != null)
		tokens.delete(last)

	let list = lazyCompletions.has(doc.uri) ? lazyCompletions.get(doc.uri) : []
	lazyCompletions.set(doc.uri, list)
	list.splice(0, list.length)
	tokens.forEach(it => list.push(CompletionItem.create(it)))
}

function fixPath(path: string, settings: DascriptSettings): string {
	if (isAbsolute(path))
		return path
	if (settings.projectRoots) {
		for (const it of settings.projectRoots) {
			const res = join(it, path)
			if (existsSync(res))
				return res
		}
	}
	if (workspaceFolders)
		for (const it of workspaceFolders) {
			const res = join(it.uri, path)
			if (existsSync(res))
				return res
		}
	return path
}

function addToDiagnostics(diagnostics: Map<string, Array<Diagnostic>>, uri: string, diagnostic: Diagnostic) {
	if (diagnostics.has(uri))
		diagnostics.get(uri).push(diagnostic)
	else
		diagnostics.set(uri, [diagnostic])
}

function setupArgs(settings: DascriptSettings, path: string): Array<string> {
	let found = false
	let args = settings.compilerArgs.map(it => {
		if (it.indexOf("${file}") == -1)
			return it
		found = true
		return it.replace("${file}", path)
	})
	if (!found)
		args.push(path)
	return args
}

async function validate(doc: TextDocument): Promise<void> {
	let uri = doc.uri
	let settings = await getDocumentSettings(uri)
	let path = decodeURIComponent(uri.replace("file:///", ""))
	let args = setupArgs(settings, path)

	connection.console.log(`> ${settings.compiler} ${args.join(' ')}`)
	execFile(settings.compiler, args, function (err, data) {
		if (err)
			connection.console.log(err.message)
		connection.console.log(data)

		var diagnostics: Map<string, Array<Diagnostic>> = new Map()
		try {
			let json = JSON.parse(data)
			let diagnosticsData: Array<any> = json.diagnostics
			if (diagnosticsData != null) {
				for (let it of diagnosticsData) {
					let uri = encodeURIComponent(fixPath(it.uri ?? "", settings))
					let relatedInformation: Array<any> = it.relatedInformation ?? []
					for (let info of relatedInformation)
						if (info.location)
							info.location.uri = encodeURIComponent(fixPath(info.location.uri ?? "", settings))
					addToDiagnostics(diagnostics, uri, it)
				}
			}
		} catch (error) {
			connection.console.log(error.message)
			connection.console.log("> fallback to text log parser")
			diagnostics.clear()
			validateTextOutput(doc, path, data, settings, diagnostics)
		}

		let depend: Array<string> = []
		for (const file of diagnostics.keys()) {
			const fileDiagnostics = diagnostics.get(file)
			connection.sendDiagnostics({ diagnostics: fileDiagnostics, uri: file })
			depend.push(file)
		}
		for (const it of (dependencies.get(uri) ?? [])) {
			if (!diagnostics.has(it))
				connection.sendDiagnostics({ diagnostics: [], uri: it })
		}
		dependencies.set(uri, depend)
	})

	getCompletion("items.das", settings)
	getCompletion("items2.das", settings)
}

function getCompletion(path: string, settings: DascriptSettings) {
	let itemsArgs = setupArgs(settings, join(__dirname, path))
	connection.console.log(`> ${settings.compiler} ${itemsArgs.join(' ')}`)
	execFile(settings.compiler, itemsArgs, function (err, data) {
		if (err)
			connection.console.log(err.message)
		try {
			let json = JSON.parse(data)
			let items: CompletionItem[] = json.items
			if (items != null) {
				connection.console.log(`> got ${items.length} completion items`)
				globalCompletion = items
				globalCompletionKeys.clear()
				for (const it of globalCompletion)
					globalCompletionKeys.add(it?.insertText ?? it.label)
			}
		} catch (error) {
			connection.console.log("> error")
			connection.console.log(error.message)
			connection.console.log(data)
		}
	})
}

function validateTextOutput(doc: TextDocument, path: string, data: string, settings: DascriptSettings, diagnostics: Map<string, Array<Diagnostic>>) {
	let encodedPath = encodeURIComponent(path)
	let lines = data.replace("\r\n", "\n").replace("\r", "\n").split("\n")
	var current: Diagnostic
	var currentUri: string
	var currentHint = ""
	var readHint = false

	function pushCurrentDiagnostic(force: boolean) {
		if (!current || (!force && current.message == null))
			return
		currentUri = currentUri ?? encodedPath
		current.message = current.message ?? ""
		if (hasDiagnosticRelatedInformationCapability && currentHint.length > 0)
			current.relatedInformation = [{ location: { uri: currentUri, range: current.range }, message: currentHint }]
		addToDiagnostics(diagnostics, currentUri, current)
		current = null
		currentUri = null
		currentHint = ""
		readHint = false
	}

	for (let line of lines) {
		if (!current) {
			let executionData = /(.+) at (.+):(\d+):(\d+)/.exec(line)
			if (executionData) {
				let pos: Position = { line: parseInt(executionData[3]) - 1, character: parseInt(executionData[4]) - 1 }
				current = { range: { start: pos, end: pos }, message: executionData[1], severity: DiagnosticSeverity.Information }
				currentUri = encodeURIComponent(fixPath(executionData[2], settings))
				pushCurrentDiagnostic(true)
				continue
			}
		}
		let fileWithErrorData = /(\S+):(\d+):(\d+):\s*(.+)/.exec(line)
		if (fileWithErrorData) {
			pushCurrentDiagnostic(true)
			let pos: Position = { line: parseInt(fileWithErrorData[2]) - 1, character: parseInt(fileWithErrorData[3]) - 1 }
			current = { range: { start: pos, end: pos }, message: fileWithErrorData[4], severity: DiagnosticSeverity.Error }
			currentUri = encodeURIComponent(fixPath(fileWithErrorData[1], settings))
			continue
		}
		let fileData = /(\S+):(\d+):(\d+):/.exec(line)
		if (fileData) {
			pushCurrentDiagnostic(true)
			let pos: Position = { line: parseInt(fileData[2]) - 1, character: parseInt(fileData[3]) - 1 }
			current = { range: { start: pos, end: pos }, message: null, severity: DiagnosticSeverity.Error }
			currentUri = encodeURIComponent(fixPath(fileData[1], settings))
			continue
		}
		let errorData = /^(\d+):\s*(.*)/.exec(line)
		if (errorData) {
			if (current && current.message != null)
				pushCurrentDiagnostic(false)
			if (!current) {
				let pos: Position = { line: 0, character: 0 }
				current = { range: { start: pos, end: pos }, message: "", severity: DiagnosticSeverity.Error }
			}
			current.message = errorData[2]
			current.code = errorData[1]
			readHint = true
			continue
		}
		if (readHint)
			currentHint += line
	}
	pushCurrentDiagnostic(true)
}
