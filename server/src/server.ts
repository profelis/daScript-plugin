import {
	Range, createConnection, TextDocuments, ProposedFeatures, TextDocumentSyncKind, DiagnosticSeverity, Diagnostic, Position, TextDocument,
	DidChangeConfigurationNotification, WorkspaceFolder, TextDocumentPositionParams, CompletionItem, DiagnosticRelatedInformation, Location,
	Hover, WorkspaceFoldersChangeEvent, MarkupKind, MarkupContent
} from 'vscode-languageserver'
import { execFile } from 'child_process'
import { isAbsolute, resolve } from 'path'
import { existsSync } from 'fs'
import { uriToFile, fixRange, strToMarkdown } from './lspUtil'
import { parseJson } from './jsonUtil'
import { functionToString, callToString, variableToString, CursorData, FunctionData, CallData, VariableData } from './cursor'
import { lazyCompletion } from './lazyCompletion'

const connection = createConnection(ProposedFeatures.all)
const documents = new TextDocuments()

let workspaceFolders: WorkspaceFolder[] | null

let hasConfigurationCapability = false
let hasDiagnosticRelatedInformationCapability = false

export interface DascriptSettings {
	compiler: string
	compilerArgs: string[]
	cursorArgs: string[]
	projectRoots: string[]
	verboseHover: boolean
}

const defaultSettings: DascriptSettings = {
	compiler: "dasAot",
	compilerArgs: ["${file}", "dummy.cpp", "-j"],
	cursorArgs: ["${file}", "dummy.cpp", "-j", "-cursor", "${character}", "${line}"],
	projectRoots: [],
	verboseHover: false,
}
let globalSettings: DascriptSettings = defaultSettings

const documentSettings: Map<string /*uri*/, Thenable<DascriptSettings>> = new Map()
const dependencies: Map<string /*uri*/, string[]> = new Map()
const lazyCompletions: Map<string /*uri*/, CompletionItem[]> = new Map()
let globalCompletion: CompletionItem[] = []
const globalCompletionKeys: Set<string> = new Set()

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
	const capabilities = params.capabilities
	hasConfigurationCapability = !!capabilities.workspace?.configuration
	hasDiagnosticRelatedInformationCapability = !!capabilities.textDocument?.publishDiagnostics?.relatedInformation

	workspaceFolders = params.workspaceFolders
	connection.console.log(`[server: (${process.pid}) dirs: ${JSON.stringify(workspaceFolders)}]`)
	return {
		capabilities: {
			textDocumentSync: TextDocumentSyncKind.Full,
			completionProvider: { resolveProvider: true },
			hoverProvider: true,
			definitionProvider: true,
			workspace: {
				workspaceFolders: {
					supported: true,
					changeNotifications: true
				}
			}
		}
	}
})

connection.onInitialized(() => {
	if (hasConfigurationCapability)
		connection.client.register(DidChangeConfigurationNotification.type, undefined)

	connection.workspace.onDidChangeWorkspaceFolders(async (event: WorkspaceFoldersChangeEvent) => {
		workspaceFolders = await connection.workspace.getWorkspaceFolders()
	})
})

connection.onCompletion((doc: TextDocumentPositionParams): CompletionItem[] => {
	const lazy = lazyCompletions.has(doc.textDocument.uri) ? lazyCompletions.get(doc.textDocument.uri) : []
	if (!lazy)
		return globalCompletion
	const res = globalCompletion.slice()
	lazy.forEach(it => { if (!globalCompletionKeys.has(it.label)) res.push(it) })
	return res
})

connection.onCompletionResolve((completion: CompletionItem): CompletionItem => {
	return completion
})

connection.onHover(async (doc: TextDocumentPositionParams) => {
	return cursor(doc.textDocument.uri, doc.position.character, doc.position.line + 1)
})

connection.onDefinition(async (doc: TextDocumentPositionParams) => {
	const settings = await getDocumentSettings(doc.textDocument.uri)
	const cursorData = await getCursorData(doc.textDocument.uri, doc.position.character, doc.position.line + 1, settings)
	if (!cursorData)
		return null
	let res: Location = null
	const doUri = path => encodeURIComponent(fixPath(path || "", settings))
	if (cursorData.variable)
		res = { uri: doUri(cursorData.variable.uri), range: fixRange(cursorData.variable.range) }
	else if (cursorData.call?.function) {
		if (cursorData.call.function.generic)
			res = { uri: doUri(cursorData.call.function.generic.uri), range: fixRange(cursorData.call.function.generic.range) }
		else
			res = { uri: doUri(cursorData.call.function.uri), range: fixRange(cursorData.call.function.range) }
	}
	if (!res || res.uri == "") {
		connection.console.log(`> goto: error \n ${JSON.stringify(cursorData, null, 2)}`)
		res = null
	} else
		connection.console.log(`> goto ${JSON.stringify(res)}`)
	return res
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
	path = path || ""
	path = path.trim()
	if (path.length == 0)
		return path
	if (isAbsolute(path))
		return path
	if (settings.projectRoots) {
		for (const it of settings.projectRoots) {
			const res = resolve(it, path)
			if (existsSync(res))
				return res
		}
	}
	if (workspaceFolders)
		for (const it of workspaceFolders) {
			const res = resolve(uriToFile(it.uri), path)
			if (existsSync(res))
				return res
		}
	return path
}

function setupArgs(initialArgs: string[], path: string): string[] {
	let found = false
	const args = initialArgs.map(it => {
		if (it.indexOf("${file}") == -1)
			return it
		found = true
		return it.replace("${file}", path)
	})
	if (!found)
		args.push(path)
	return args
}

async function getCursorData(uri: string, x: number, y: number, settings: DascriptSettings): Promise<CursorData | null> {
	const path = uriToFile(uri)
	const args = setupArgs(settings.cursorArgs, path)
	// tslint:disable-next-line: no-for-in
	for (const i in args) {
		if (args[i].indexOf("${line}") >= 0) {
			args[i] = args[i].replace("${line}", y.toString())
		}
		if (args[i].indexOf("${character}") >= 0) {
			args[i] = args[i].replace("${character}", x.toString())
		}
	}

	return new Promise<CursorData | null>((onResolve, _) => {
		connection.console.log(`> ${settings.compiler} ${args.join(' ')}`)
		execFile(settings.compiler, args, (err, data) => {
			if (err)
				connection.console.log(err.message)
			// connection.console.log(data)
			if (data.trim().length == 0) {
				connection.console.log("> cursor: data is empty")
				onResolve(null)
				return
			}
			try {
				const res = <CursorData>parseJson(data)
				if (!res) {
					connection.console.log("> cursor: parse error")
					connection.console.log(data)
				}
				onResolve(res)
				return
			} catch (error) {
				connection.console.log("> cursor: parse json data error")
				connection.console.log(error.message)
				connection.console.log(data)
			}
			onResolve(null)
		})
	})
}

async function cursor(uri: string, x: number, y: number): Promise<Hover> {
	const settings = await getDocumentSettings(uri)
	const cursorData = await getCursorData(uri, x, y, settings)
	const res: string[] = []
	let range = fixRange(cursorData?.cursor?.range)
	const HR = "---"

	let describeFunction = (data: FunctionData, includeGeneric = false) => {
		if (res.length > 0)
			res.push(HR)
		if (includeGeneric && data.generic) {
			res.push(strToMarkdown(functionToString(data.generic, settings, settings.verboseHover)))
			res.push(HR)
		}
		res.push(strToMarkdown(functionToString(data, settings, settings.verboseHover)))
	}
	let describeCall = (data: CallData, includeGeneric = false) => {
		if (res.length > 0)
			res.push(HR)
		if (data.function) {
			if (includeGeneric && data.function.generic) {
				res.push(strToMarkdown(functionToString(data.function.generic, settings, settings.verboseHover)))
				res.push(HR)
			}
			res.push(strToMarkdown(functionToString(data.function, settings, settings.verboseHover)))
			const shortname = data.function.generic?.shortname ?? data.function.shortname
			if (shortname && shortname.length > 0) {
				let firstDoc = true
				globalCompletion.forEach(it => {
					if (it.filterText == shortname && it.documentation) {
						if (firstDoc)
							res.push(HR)
						firstDoc = false
						res.push(((<MarkupContent>it.documentation)?.value ?? it.documentation).toString())
					}
				})
			}
		}
		else
			res.push(strToMarkdown(callToString(data, settings, settings.verboseHover)))
		range = null
	}
	let describeVariable = (data: VariableData, showCall = false) => {
		if (res.length > 0)
			res.push(HR)
		res.push(strToMarkdown(variableToString(data, settings, settings.verboseHover, showCall)))
		range = null
	}

	if (cursorData) {
		if (!(cursorData.call || cursorData.variable)) {
			if (cursorData.functions && cursorData.functions.length > 0)
				cursorData.functions.forEach(it => describeFunction(it, cursorData.functions.length == 1))
			else if (cursorData.function)
				describeFunction(cursorData.function, true)
		}
		if (settings.verboseHover || !cursorData.variable) {
			if (cursorData.calls && cursorData.calls.length > 0)
				cursorData.calls.forEach(it => describeCall(it, cursorData.calls.length == 1))
			else if (cursorData.call)
				describeCall(cursorData.call, true)
		}
		if (cursorData.variables && cursorData.variables.length > 0)
			cursorData.variables.forEach(it => describeVariable(it, cursorData.variables.length > 1))
		else if (cursorData.variable)
			describeVariable(cursorData.variable)
	}
	// connection.console.log(JSON.stringify(range))
	// for (const it of res)
	// 	connection.console.log(JSON.stringify(it))

	if (res.length == 0)
		return settings.verboseHover ? { contents: { kind: MarkupKind.Markdown, value: strToMarkdown(JSON.stringify(range), "json") } } : null
	return { contents: { kind: MarkupKind.Markdown, value: res.join("\n") }, range: range }
}

function addToDiagnostics(diagnostics: Map<string, Diagnostic[]>, uri: string, diagnostic: Diagnostic) {
	if (diagnostics.has(uri))
		diagnostics.get(uri).push(diagnostic)
	else
		diagnostics.set(uri, [diagnostic])
}

async function validate(doc: TextDocument): Promise<void> {
	const settings = await getDocumentSettings(doc.uri)
	const path = uriToFile(doc.uri)
	const args = setupArgs(settings.compilerArgs, path)

	connection.console.log(`> ${settings.compiler} ${args.join(' ')}`)
	execFile(settings.compiler, args, (err, data) => {
		if (err)
			connection.console.log(err.message)
		connection.console.log(data)
		const diagnostics: Map<string, Diagnostic[]> = new Map()
		if (data.trim().length > 0) {
			try {
				const json = parseJson(data)
				const diagnosticsData: any[] = json.diagnostics
				for (const it of diagnosticsData) {
					const localPath = it.uri || path
					const uri = encodeURIComponent(fixPath(localPath, settings))
					const diagnostic = <Diagnostic>it
					if (diagnostic?.message == null)
						continue
					const offset = "tab" in it ? 1 : 0
					diagnostic.range = fixRange(diagnostic?.range, -offset, offset)
					const relatedInformation: DiagnosticRelatedInformation[] = diagnostic?.relatedInformation ?? []
					for (const info of relatedInformation) {
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

		const depend: string[] = []
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

	getGlobalCompletion("items.das", settings)
	getGlobalCompletion("items2.das", settings)
}

function getGlobalCompletion(path: string, settings: DascriptSettings) {
	const itemsArgs = setupArgs(settings.compilerArgs, resolve(__dirname, path))
	connection.console.log(`> ${settings.compiler} ${itemsArgs.join(' ')}`)
	execFile(settings.compiler, itemsArgs, (err, data) => {
		if (err)
			connection.console.log(err.message)
		try {
			const json = parseJson(data)
			const items: CompletionItem[] = json.items
			if (items != null) {
				connection.console.log(`> got ${items.length} completion items`)
				globalCompletion = items
				globalCompletionKeys.clear()
				for (const it of globalCompletion)
					globalCompletionKeys.add(it?.insertText ?? it.label)
			}
		} catch (error) {
			connection.console.log("> global completion: error")
			connection.console.log(error.message)
			connection.console.log(data)
		}
	})
}

function validateTextOutput(doc: TextDocument, path: string, data: string, settings: DascriptSettings, diagnostics: Map<string, Diagnostic[]>) {
	const encodedPath = encodeURIComponent(path)
	const lines = data.replace("\r\n", "\n").replace("\r", "\n").split("\n")
	let current: Diagnostic
	let currentUri: string
	let currentHint = ""
	let readHint = false

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

	for (const line of lines) {
		if (!current) {
			const executionData = /(.+) at (.+):(\d+):(\d+)/.exec(line)
			if (executionData) {
				const pos: Position = { line: parseInt(executionData[3]) - 1, character: parseInt(executionData[4]) }
				current = { range: { start: pos, end: pos }, message: executionData[1], severity: DiagnosticSeverity.Information }
				currentUri = encodeURIComponent(fixPath(executionData[2], settings))
				pushCurrentDiagnostic(true)
				continue
			}
		}
		const fileWithErrorData = /(\S+):(\d+):(\d+):\s*(.+)/.exec(line)
		if (fileWithErrorData) {
			pushCurrentDiagnostic(true)
			const pos: Position = { line: parseInt(fileWithErrorData[2]) - 1, character: parseInt(fileWithErrorData[3]) }
			current = { range: { start: pos, end: pos }, message: fileWithErrorData[4], severity: DiagnosticSeverity.Error }
			currentUri = encodeURIComponent(fixPath(fileWithErrorData[1], settings))
			continue
		}
		const fileData = /(\S+):(\d+):(\d+):/.exec(line)
		if (fileData) {
			pushCurrentDiagnostic(true)
			const pos: Position = { line: parseInt(fileData[2]) - 1, character: parseInt(fileData[3]) }
			current = { range: { start: pos, end: pos }, message: null, severity: DiagnosticSeverity.Error }
			currentUri = encodeURIComponent(fixPath(fileData[1], settings))
			continue
		}
		const errorData = /^(\d+):\s*(.*)/.exec(line)
		if (errorData) {
			if (current && current.message != null)
				pushCurrentDiagnostic(false)
			if (!current) {
				const pos: Position = { line: 0, character: 0 }
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
