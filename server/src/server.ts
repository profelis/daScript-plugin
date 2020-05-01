import {
	Range, createConnection, TextDocuments, ProposedFeatures, TextDocumentSyncKind, DiagnosticSeverity, Diagnostic, Position, TextDocument, DidChangeConfigurationNotification, WorkspaceFolder, TextDocumentPositionParams, CompletionItem, DiagnosticRelatedInformation, Location, Hover, MarkedString
} from 'vscode-languageserver'
import { execFile, execFileSync } from 'child_process'
import { isAbsolute, join } from 'path'
import { existsSync } from 'fs'
import { uriToFile, fixRange } from './lspUtil'
import { parseCursor, funcToString, callToString, variableToString } from './cursor'
import { lazyCompletion } from './lazyCompletion'

let connection = createConnection(ProposedFeatures.all)
let documents = new TextDocuments()

let workspaceFolders: WorkspaceFolder[] | null

let hasConfigurationCapability = false
let hasDiagnosticRelatedInformationCapability = false

export interface DascriptSettings {
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

export function getDocumentSettings(uri: string): Thenable<DascriptSettings> {
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
			completionProvider: { resolveProvider: true },
			hoverProvider: true,
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

connection.onHover(async (doc: TextDocumentPositionParams) => {
	return cursor(doc.textDocument.uri, doc.position.character, doc.position.line + 1)
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
	lazyCompletion(event.document, lazyCompletions)
})

documents.onDidSave(event => {
	connection.console.log(`[save ${event.document.uri}]`)
	validate(event.document)
	lazyCompletion(event.document, lazyCompletions)
})

documents.onDidChangeContent(event => {
	connection.console.log(`[changed ${event.document.uri}]`)
	lazyCompletion(event.document, lazyCompletions)
})

documents.listen(connection)
connection.listen()

export function fixPath(path: string, settings: DascriptSettings): string {
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

async function cursor(uri: string, x: number, y: number): Promise<Hover> {
	const settings = await getDocumentSettings(uri)
	const path = uriToFile(uri)
	const args = setupArgs(settings, path)
	args.push(`-cursor`)
	args.push(x.toString())
	args.push(y.toString())

	let buffer: string
	try {
		connection.console.log(`> ${settings.compiler} ${args.join(' ')}`)
		buffer = execFileSync(settings.compiler, args).toString()

		let res: MarkedString[] = []
		const data = JSON.parse(buffer)
		let cursor = parseCursor(data, path, settings)
		let range = cursor?.range
		if (cursor) {
			if (cursor.func) {
				if (cursor.func.generic)
					res.push({ language: "dascript", value: funcToString(cursor.func.generic) })
				// else
				res.push({ language: "dascript", value: funcToString(cursor.func) })
			}
			if (cursor.call) {
				if (cursor.call.func) {
					if (cursor.call.func?.generic)
						res.push({ language: "dascript", value: callToString(cursor.call.func.generic) })
					res.push({ language: "dascript", value: callToString(cursor.call.func) })
				}
				else
					res.push({ language: "dascript", value: callToString(cursor.call) })
				range = null
			}
			if (cursor.variable) {
				res.push({ language: "dascript", value: variableToString(cursor.variable) })
				range = null
			}
		} else {
			connection.console.log(buffer)
		}
		// connection.console.log(JSON.stringify(range))
		// for (const it of res)
		// 	connection.console.log(JSON.stringify(it))

		if (res.length == 0)
			return { contents: { language: "json", value: JSON.stringify(range) } }
		return { contents: res, range: range }
	} catch (error) {
		connection.console.log(error.message)
		connection.console.log("> cursor error")
		connection.console.log(buffer)
		return null
	}
}

async function validate(doc: TextDocument): Promise<void> {
	let settings = await getDocumentSettings(doc.uri)
	let path = uriToFile(doc.uri)
	let args = setupArgs(settings, path)

	connection.console.log(`> ${settings.compiler} ${args.join(' ')}`)
	execFile(settings.compiler, args, function (err, data) {
		if (err)
			connection.console.log(err.message)
		connection.console.log(data)
		var diagnostics: Map<string, Array<Diagnostic>> = new Map()
		if (data.trim().length > 0) {
			try {
				let json = JSON.parse(data)
				let diagnosticsData: Array<any> = json?.diagnostics ?? []
				for (let it of diagnosticsData) {
					let localPath = it.uri || path
					let uri = encodeURIComponent(fixPath(localPath, settings))
					let diagnostic = it as Diagnostic
					if (diagnostic == null || diagnostic.message == null)
						continue
					const offset = "tab" in it ? 1 : 0
					diagnostic.range = fixRange(diagnostic?.range, -offset, offset)
					let relatedInformation: DiagnosticRelatedInformation[] = diagnostic?.relatedInformation ?? []
					for (let info of relatedInformation) {
						info.location = info?.location ?? Location.create(uri, Range.create(0, 0, 0, 0))
						info.location.uri = encodeURIComponent(fixPath(info.location.uri ?? localPath, settings))
						info.location.range = fixRange(info.location?.range, -offset, offset)
					}
					addToDiagnostics(diagnostics, uri, diagnostic)
				}
			} catch (error) {
				connection.console.log(error.message)
				connection.console.log("> fallback to text log parser")
				diagnostics.clear()
				validateTextOutput(doc, path, data, settings, diagnostics)
			}
		}

		let depend: Array<string> = []
		for (const file of diagnostics.keys()) {
			const fileDiagnostics = diagnostics.get(file)
			connection.sendDiagnostics({ diagnostics: fileDiagnostics, uri: file })
			depend.push(file)
		}
		for (const it of (dependencies.get(doc.uri) ?? [])) {
			if (!diagnostics.has(it))
				connection.sendDiagnostics({ diagnostics: [], uri: it })
		}
		dependencies.set(doc.uri, depend)
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
