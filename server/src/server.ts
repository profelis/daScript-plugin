
import {
	CodeAction,
	CodeActionKind,
	CompletionItem, CompletionItemKind,
	Diagnostic,
	DiagnosticSeverity,
	DiagnosticTag,
	DidChangeConfigurationNotification,
	DocumentSymbol,
	InlayHint, InlayHintKind,
	LSPAny,
	Location,
	MarkupContent,
	Position,
	ProposedFeatures,
	Range,
	SymbolKind,
	TextDocumentSyncKind,
	TextDocuments,
	TextEdit,
	WorkDoneProgressBegin,
	WorkDoneProgressEnd,
	WorkDoneProgressReport,
	WorkspaceEdit,
	WorkspaceFolder,
	createConnection,
	integer
} from 'vscode-languageserver/node'

import {
	DocumentUri,
	TextDocument
} from 'vscode-languageserver-textdocument'

import { ChildProcessWithoutNullStreams, spawn } from 'child_process'
import { URI } from 'vscode-uri'
import { AtToRange, AtToUri, BEFORE_ALL_SORT, BaseType, Brackets, CompletionAt, CompletionEnum, CompletionResult, CompletionStruct, DasToken, Delimiter, EXTENSION_FN_SORT, FIELD_SORT, FixedValidationResult, MODULE_SORT, ModuleRequirement, OPERATOR_SORT, PROPERTY_PREFIX, PROPERTY_SORT, TokenKind, ValidationResult, addUniqueLocation, addValidLocation, closedBracketPos, describeToken, enumDetail, enumDocs, enumValueDetail, enumValueDocs, findEnum, findFunction, findStruct, findTypeDecl, findTypeDef, findTypeDefNoMod, fixPropertyName, funcArgDetail, funcArgDocs, funcDetail, funcDocs, getParentStruct, globalDetail, globalDocs, isPositionLess, isPositionLessOrEqual, isRangeEqual, isRangeLengthZero, isRangeLess, isRangeZeroEmpty, isSpaceChar, isValidIdChar, isValidLocation, posInRange, primitiveBaseType, rangeCenter, rangeLength, tdkName, structDetail, structDocs, structFieldDetail, structFieldDocs, typeDeclCompletion, typeDeclDefinition, typeDeclDetail, typeDeclDocs, typeDeclFieldDetail, typeDeclIter, typedeclAssignOperator, typedefDetail, typedefDocs, CompletionTypeDef } from './completion'
import { DasSettings, defaultSettings, documentSettings } from './dasSettings'
import path = require('path')
import fs = require('fs')
import os = require('os')
import { promisify } from 'util'
import { readdir } from 'fs'
import { ValidatingQueue } from './validatingQueue'
import { join } from 'path'

enum DiagnosticsActionType {
	UnusedReq = 0,
}

interface DiagnosticsAction {
	type: DiagnosticsActionType
	data: LSPAny
}

interface WorkspaceValidationParams {
	saveCache: boolean,
	cacheFolder?: string,
	queueCapacity: number,
}

// Creates the LSP connection
const connection = createConnection(ProposedFeatures.all)

// Create a manager for open text documents
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument)

const validatingResults: Map<string/*uri*/, FixedValidationResult> = new Map()
const autoFormatResult: Map<string/*uri*/, string> = new Map()

// The workspace folder this server is operating on
let workspaceFolders: WorkspaceFolder[] | null

let hasConfigurationCapability = false
let hasWorkspaceFolderCapability = false
let hasDiagnosticRelatedInformationCapability = false


const serverCommandHandlers: { [k in string]: (args: any) => Promise<void> } = {
	'validateWorkspace': async args => await validateWorkspaceCommand(args),
	'clearValidationCache': async _ => await clearCachedValidationDataCommand(),
}


function setWorkspaceValidationParams(config: any): WorkspaceValidationParams {
	return {
		saveCache: config?.project?.storeCompilationData ?? false,
		cacheFolder: './dascript',
		queueCapacity: config?.project?.compilationConcurrency ?? 8,
	}
}


function mangleFileUri(uri: DocumentUri, version?: number): string {
	const filePath = URI.parse(uri).fsPath;

	const uriHash = stringHashCode(uri).toString(16);
	const versionHash = version?.toString(16);
	const filename = path.basename(filePath);

	return [
		uriHash,
		versionHash,
		filename
	].filter(p => !!p).join("_");
}

function debugWsFolders() {
	return workspaceFolders ? workspaceFolders.map(f => f.name).join(', ') : ''
}

// did change content will be called on open and on change
// documents.onDidOpen((event) => {
// 	console.log(`[Server(${process.pid}) ${debugWsFolders()}] Document opened: ${event.document.uri}`)
// 	validateTextDocument(event.document)
// })

documents.onDidChangeContent(async (event) => {
	console.log(`[Server(${process.pid}) ${debugWsFolders()}] Document changed: ${event.document.uri}`)
	await updateTextDocumentData(event.document)
})

documents.onDidSave(async (event) => {
	console.log(`[Server(${process.pid}) ${debugWsFolders()}] Document saved: ${event.document.uri}`)
	await updateTextDocumentData(event.document)
})

documents.onDidClose(e => {
	// documentSettings.delete(e.document.uri)
	// const prevProcess = validatingProcesses.get(e.document.uri)
	// if (prevProcess != null) {
	// 	prevProcess.process?.kill()
	// 	console.log('killed process for', e.document.uri)
	// 	validatingProcesses.delete(e.document.uri)
	// }
	// validatingResults.delete(e.document.uri) // TODO: remove only tokens?
	let diagnostics = new Map<string, Diagnostic[]>()
	diagnostics.set(e.document.uri, [])
	const prev = validatingResults.get(e.document.uri)
	if (prev != null) {
		// collect prev errors and update errors for the same uri-s (to remove transitive errors)
		for (const [errorsUri, _] of prev.diagnostics) {
			if (e.document.uri == errorsUri)
				continue;

			diagnostics.set(errorsUri, collectDiagnostics(errorsUri, e.document.uri));
		}
	}
	for (const [uri, errors] of diagnostics) {
		connection.sendDiagnostics({ uri: uri, diagnostics: errors })
	}
})

documents.listen(connection)

connection.onInitialize((params) => {
	workspaceFolders = params.workspaceFolders
	console.log(`[Server(${process.pid}) ${debugWsFolders()}] Started and initialize received`)

	const capabilities = params.capabilities

	// Does the client support the `workspace/configuration` request?
	// If not, we fall back using global settings.
	hasConfigurationCapability = !!(
		capabilities.workspace && !!capabilities.workspace.configuration
	)
	hasWorkspaceFolderCapability = !!(
		capabilities.workspace && !!capabilities.workspace.workspaceFolders
	)
	hasDiagnosticRelatedInformationCapability = !!(
		capabilities.textDocument &&
		capabilities.textDocument.publishDiagnostics &&
		capabilities.textDocument.publishDiagnostics.relatedInformation
	)

	return {
		capabilities: {
			completionProvider: {
				resolveProvider: false,
				triggerCharacters: ['.', '>', ':'],
			},
			hoverProvider: true,
			definitionProvider: true,
			// declarationProvider: true,
			typeDefinitionProvider: true,
			documentSymbolProvider: true,
			referencesProvider: true,
			inlayHintProvider: {
				resolveProvider: false,
			},
			documentFormattingProvider: true,
			// colorProvider: true,
			// documentHighlightProvider : true, // TODO: implement
			// workspace: {
			// fileOperations:{}
			// workspaceFolders: {}
			// },
			workspace: {
				workspaceFolders: {
					supported: true,
					changeNotifications: true,
				},
			},
			textDocumentSync: {
				openClose: true,
				change: TextDocumentSyncKind.Incremental,
				save: {
					includeText: true,
				},
			},
			codeActionProvider: {
				codeActionKinds: [
					CodeActionKind.QuickFix,
					// CodeActionKind.Refactor,
					// CodeActionKind.RefactorExtract,
					// CodeActionKind.RefactorInline,
					// CodeActionKind.RefactorRewrite,
					// CodeActionKind.Source,
					// CodeActionKind.SourceOrganizeImports,
				],
			},
		}
	}
})


connection.onCodeAction(async (params) => {
	const res: CodeAction[] = []
	if (params.context.only == null || params.context.only.includes(CodeActionKind.QuickFix)) {
		for (const diag of params.context.diagnostics) {
			if (!diag.data)
				continue
			const action = diag.data as DiagnosticsAction
			if (action.type == DiagnosticsActionType.UnusedReq) {
				const doc = documents.get(params.textDocument.uri)
				if (doc) {
					const cmd: WorkspaceEdit = { changes: {}, }
					const range = Range.create(diag.range.start, Position.create(diag.range.start.line + 1, 0))
					cmd.changes[params.textDocument.uri] = [TextEdit.del(range)]
					res.push(CodeAction.create(`Remove unused requirement: '${action.data}'`, cmd, CodeActionKind.QuickFix))

					const fileData = await getDocumentDataFast(params.textDocument.uri)
					if (fileData) {
						const cmdAll = { changes: {}, }
						for (const req of fileData.requirements) {
							if (!req._used) {
								const range = Range.create(req._range.start, Position.create(req._range.start.line + 1, 0))
								cmdAll.changes[params.textDocument.uri] = cmdAll.changes[params.textDocument.uri] || []
								cmdAll.changes[params.textDocument.uri].push(TextEdit.del(range))
							}
						}
						res.push(CodeAction.create(`Remove all unused requirements`, cmdAll, CodeActionKind.QuickFix))
					}
				}
			}
		}
	}
	return res
})


interface CallChain {
	obj: string
	objRange: Range
	tokens: DasToken[]
	tdks: Set<string>
	delimiter: Delimiter
	delimiterRange: Range
	brackets: Brackets
}

function findRequirement(doc: TextDocument, fileData: FixedValidationResult, pos: Position): boolean {
	const line = doc.getText(Range.create(pos.line, 0, pos.line, pos.character))
	return line.startsWith('require ')
}

interface StructCtor {
	name: string
	oldStyle: boolean
}

function findStructCtor(doc: TextDocument, fileData: FixedValidationResult, pos: Position, forAutocompletion: boolean, recursion: number): StructCtor {
	const line = doc.getText(Range.create(Math.max(0, pos.line - 30), 0, pos.line, pos.character))
	let i = line.length - 1
	let numO = 0 // ( )
	let numE = 0 // { }
	let numD = 0 // [ ]
	for (; i >= 0; i--) {
		const ch = line[i]
		if (ch === ')')
			numO++
		else if (ch === '(')
			numO--
		else if (ch === ']')
			numD++
		else if (ch === '[')
			numD--
		else if (ch === '}')
			numE++
		else if (ch === '{')
			numE--
		if ((ch == '(') && numO == -1 && numE == 0 && numD == 0) {
			let key = ''
			for (let j = i - 1; j >= 0; j--) {
				const ch = line[j]
				if (key == '' && isSpaceChar(ch))
					continue
				if (isValidIdChar(ch))
					key = ch + key
				else
					break
			}
			if (key.length > 0) {
				return { name: key, oldStyle: false }
			}
		}
		if ((ch == '[') && numO == 0 && numE == 0 && numD == -2) {
			let key = ''
			for (let j = i + 2; j < line.length - 1; j++) {
				const ch = line[j]
				if (key == '' && isSpaceChar(ch))
					continue
				if (isValidIdChar(ch))
					key += ch
				else
					break
			}
			if (key.length > 0) {
				return { name: key, oldStyle: true }
			}
		}
	}
	return { name: "", oldStyle: false }
}

function findCallChain(doc: TextDocument, fileData: FixedValidationResult, pos: Position, forAutocompletion: boolean): CallChain[] {
	return findCallChain_(doc, fileData, pos, forAutocompletion, 0)
}
function findCallChain_(doc: TextDocument, fileData: FixedValidationResult, pos: Position, forAutocompletion: boolean, recursion: number): CallChain[] {
	if (recursion > 20) {
		console.error('recursion limit reached. In file ', fileData.uri, ' at position ', pos.line, pos.character)
		return []
	}
	/// find chain of fields access - foo.key or foo().key  ... etc
	/// support sequences: foo.key.key2.key3
	/// also support function calls and array/table access: foo().key, foo[0].key, foo().key[0]
	const line = doc.getText(Range.create(pos.line, 0, pos.line, pos.character))
	let del = Delimiter.None
	if (line.length === 0)
		return []
	{
		let j = line.length - 1
		for (; j >= 0; j--) {
			const ch = line[j]
			if (!isSpaceChar(ch)) {
				break
			}
		}
		// ignore fully empty lines
		if (j >= 0 && line[j] === '=') {
			del = Delimiter.Assign
		}
	}
	let key = ''
	let keyRange: Range
	let i = line.length - 1
	let tokens: DasToken[] = []
	if (fileData.errors.length == 0) {
		// lets try to find token under cursor
		const cursorTokens = findTokensUnderCursor(doc, fileData, pos)
		for (const cursorTok of cursorTokens) {
			tokens.push(cursorTok)
			key = cursorTok.name
			keyRange = cursorTok._range
			i = doc.offsetAt(cursorTok._range.start) - doc.offsetAt(Position.create(pos.line, 0)) - 1
		}
	}

	// if (forAutocompletion || tokens.length === 0) {
	if (tokens.length === 0) {
		// auto completion or token not found - find it manually
		for (; i >= 0; i--) {
			const ch = line[i]
			if (isSpaceChar(ch)) {
				if (key.length === 0) {
					del = Delimiter.Space
					continue
				}
				break
			}
			if (isValidIdChar(ch))
				key = ch + key
			else
				break
		}
		if (!forAutocompletion) {
			// search the rest of the key
			const text = doc.getText(Range.create(pos.line, pos.character, pos.line, pos.character + 100))
			for (let j = 0; j < text.length; j++) {
				const ch = text[j]
				if (isValidIdChar(ch))
					key += ch
				else
					break
			}
		}
		keyRange = Range.create(pos.line, i + 1, pos.line, i + 1 + key.length)
	}

	if (del === Delimiter.Space && (key == 'as' || key == 'is')) {
		// skip 'as' 'is' '?as' in case when space is delimiter
		// `foo as |` - converts to ["foo", ""], not to ["foo", "as"]
		// `?as` will be also skipped
		i += key.length
		key = ''
		keyRange = Range.create(pos.line, i + 1, pos.line, i + 1)
	}
	else {
		// skip spaces
		// for (; i >= 0; i--) {
		// 	const ch = line[i]
		// 	if (!isSpaceChar(ch))
		// 		break
		// }
	}
	const keyData: CallChain = { obj: key, objRange: keyRange, tokens: tokens, tdks: tokensToTdks(tokens), delimiter: del, brackets: Brackets.None, delimiterRange: Range.create(0, 0, 0, 0) }

	let res: CallChain[] = [keyData]
	while (i > 0) {
		del = Delimiter.None
		let delimiterRange = Range.create(pos.line, i, pos.line, i)
		// '.' ' ' '?.' '->' 'as' 'is' '?as' '|>'

		// space can be delimiter only when chain is just started
		while (i >= 0) {
			if (line[i] === ' ') {
				i -= 1
				del = Delimiter.Space
				continue
			}
			else if (i > 0 && line[i] === '.' && line[i - 1] === '?') {
				i -= 2
				del = Delimiter.QuestionDot
				break
			}
			else if (line[i] === '.') {
				i -= 1
				del = Delimiter.Dot
				break
			}
			else if (i > 0 && line[i] === '>' && line[i - 1] === '-') {
				i -= 2
				del = Delimiter.Arrow
				break
			}
			else if (i > 1 && line[i] === 's' && line[i - 1] === 'a' && line[i - 2] === '?') {
				i -= 3
				del = Delimiter.QuestionAs
				break
			}
			else if (i > 0 && line[i] === 's' && line[i - 1] === 'a') {
				i -= 2
				del = Delimiter.As
				break
			}
			else if (i > 0 && line[i] === 's' && line[i - 1] === 'i') {
				i -= 2
				del = Delimiter.Is
				break
			}
			else if (i > 0 && line[i] === '>' && line[i - 1] === '|') {
				i -= 2
				del = Delimiter.Pipe
				break
			}
			else if (i > 0 && line[i] === ':' && line[i - 1] === ':') {
				i -= 2
				del = Delimiter.ColonColon
				break
			}
			// else if (line[i] == ',')
			// {
			// 	i--
			// 	continue // lets try to find the next delimiter
			// }
			else
				break
		}
		delimiterRange.start.character = i
		// if (del === Delimiter.None)
		// 	break
		// skip spaces
		for (; i >= 0; i--) {
			const ch = line[i]
			if (!isSpaceChar(ch))
				break
		}
		let tokenEnd = i + 1 // token + brackets

		let brackets = Brackets.None
		if (i >= 0) {
			brackets = line[i] === ')' ? Brackets.Round : line[i] === ']' ? Brackets.Square : Brackets.None
			if (brackets !== Brackets.None) {
				let numO = 0 // ( )
				let numE = 0 // { }
				let numD = 0 // [ ]
				for (; i >= 0; i--) {
					const ch = line[i]
					if (ch === ')')
						numO++
					else if (ch === '(')
						numO--
					else if (ch === ']')
						numD++
					else if (ch === '[')
						numD--
					else if (ch === '}')
						numE++
					else if (ch === '{')
						numE--
					if ((ch == '(' || ch == '[' || ch == '{') && numO == 0 && numE == 0 && numD == 0) {
						i-- // skip last bracket
						break
					}
				}

				if (brackets == Brackets.Square && i >= 0 && line[i] === '?') {
					// ?[] case, skip '?'
					brackets = Brackets.QuestionSquare
					i--
				}

				// skip spaces
				for (; i >= 0; i--) {
					const ch = line[i]
					if (!isSpaceChar(ch))
						break
				}
			}
		}

		if (del == Delimiter.None && brackets == Brackets.None)
			break

		let obj = ''
		for (; i >= 0; i--) {
			const ch = line[i]
			if (isValidIdChar(ch))
				obj = ch + obj
			else
				break
		}
		if (obj.length === 0 && brackets == Brackets.None) { break }
		// if (del === Delimiter.Space && i >= 0 && isSpaceChar(line[i]) && (obj == 'as' || obj == 'is' || obj == '?as')) {
		// 	// skip 'as' 'is' '?as' in case when space is delimiter and next char is space
		// 	i += obj.length // move back to beginning of the word
		// 	continue
		// }

		// space delimiter can be only at the beginning of the chain
		if (del == Delimiter.Space && res.length >= 2)
			break
		const objRange = Range.create(pos.line, i + 1, pos.line, tokenEnd)
		if (obj == '' && brackets == Brackets.Round && rangeLength(objRange) > 0) {
			const subStart = Position.create(objRange.end.line, objRange.end.character - 1)
			const sub = findCallChain_(doc, fileData, subStart, forAutocompletion, recursion + 1)
			if (sub.length > 1) {
				sub.pop()
				res = sub.concat(res)
				continue
			}
		}
		res.unshift({ obj: obj, objRange: objRange, tokens: [], tdks: new Set(), delimiter: del, brackets: brackets, delimiterRange: delimiterRange })
	}

	resolveChainTdks(doc, fileData, res, forAutocompletion, recursion)
	return res
}

function tokensToTdks(tokens: DasToken[]): Set<string> {
	const res = new Set<string>()
	for (const tok of tokens) {
		if (tok.tdk.length > 0)
			res.add(tok.tdk)
	}
	return res
}

function resolveChainTdks(doc: TextDocument, fileData: FixedValidationResult, callChain: CallChain[], forAutocompletion: boolean, recursion = 0): void {
	if (callChain.length === 0)
		return
	const last = callChain[callChain.length - 1]
	if (!forAutocompletion && last.tokens.length > 0) {
		return
	}

	const globalCompletion = getGlobalCompletion()

	let prevTdks: Set<string>
	let prevDelimiter: Delimiter = Delimiter.None
	let prevDelimiterRange: Range
	var idx = 0
	while (idx < callChain.length) {
		if (idx > 0) {
			prevDelimiter = callChain[idx - 1].delimiter
			prevDelimiterRange = callChain[idx - 1].delimiterRange
		}
		const call = callChain[idx]
		idx++
		// maybe already resolved node
		if (call.tdks.size > 0) {
			prevTdks = call.tdks
			continue
		}
		// don't change here anything, it works fine
		const searchPos = call.delimiter.length > 1 && prevDelimiterRange ? rangeCenter(prevDelimiterRange) : call.objRange.start
		let cursorTokens = findTokensUnderCursor(doc, fileData, searchPos)
		if (cursorTokens.length == 0 && searchPos != call.objRange.start) {
			cursorTokens = findTokensUnderCursor(doc, fileData, call.objRange.start)
		}
		if (cursorTokens.length > 0) {
			for (const cursorTok of cursorTokens) {
				call.tokens.push(cursorTok)
				if (cursorTok.tdk.length > 0)
					call.tdks.add(cursorTok.tdk)
			}
			if (call.tokens.length > 0) {
				prevTdks = call.tdks
				continue
			}
		}
		if (idx == 1) {
			const tokens = call.tokens.length > 0 ? call.tokens : findNearestTokensAt(doc, fileData, call, recursion)
			if (tokens.length > 0) {
				call.tokens = tokens
				call.tdks = tokensToTdks(call.tokens)
				prevTdks = call.tdks
				continue
			}
		}
		if (prevTdks && prevTdks.size > 0) {
			// resolve tdk for type decls
			for (const prevTdk of prevTdks) {
				let typeDeclData = findTypeDecl(prevTdk, fileData.completion, globalCompletion)
				if (typeDeclData != null) {
					var next: CompletionItem[] = []
					if (call.obj.length == 0 && call.brackets == Brackets.Square) {
						const nextTdk = typeDeclCompletion(typeDeclData, fileData.completion, globalCompletion, call.delimiter, call.brackets, call.obj, next)
						if (nextTdk.tdk != prevTdk) {
							call.tdks.add(nextTdk.tdk)
						}
					}
					if (call.obj.length > 0) {
						const nextTdk = typeDeclCompletion(typeDeclData, fileData.completion, globalCompletion, call.delimiter, call.brackets, call.obj, next)
						// if (call.obj.length > 0) {
						for (const it of next) {
							if (it.label == call.obj) {
								if (prevDelimiter == Delimiter.Is)
									call.tdks.add(BaseType.tBool)
								else if (it.data?.length > 0)
									call.tdks.add(it.data)
							}
						}
						// }
					}
				}
			}
			if (call.tdks.size > 0) {
				prevTdks = call.tdks
				continue
			}
		}
		if (call.obj.length > 0) {
			if (call.delimiter == Delimiter.Space || call.delimiter == Delimiter.Dot) {
				// maybe enum or bitfield
				let found = false
				let enumCb = (en: CompletionEnum) => {
					if (en.name === call.obj && en.tdk.length > 0) {
						call.tdks.add(en.tdk);
						found = true;
					}
					if (prevTdks && prevTdks.has(en.tdk)) {
						for (const it of en.values) {
							if (it.name == call.obj) {
								call.tdks.add(en.tdk);
								found = true;
								break;
							}
						}
					}
				}
				fileData.completion.enums.forEach(enumCb)
				if (globalCompletion)
					globalCompletion.enums.forEach(enumCb)
				// or alias
				let aliasCb = (td: CompletionTypeDef) => {
					if (td.name === call.obj && td.tdk.length > 0) {
						call.tdks.add(td.tdk);
						found = true;
					}
				}
				fileData.completion.typeDefs.forEach(aliasCb)
				if (globalCompletion)
					globalCompletion.typeDefs.forEach(aliasCb)

				if (found) {
					prevTdks = call.tdks
					continue
				}
			}
			else if (call.brackets == Brackets.Round) {
				let fnCb = (fn) => {
					if (fn.name === call.obj && fn.tdk.length > 0) {
						call.tdks.add(fn.tdk);
					}
				}
				fileData.completion.functions.forEach(fnCb)
				if (globalCompletion)
					globalCompletion.functions.forEach(fnCb)
				if (call.tdks.size > 0) {
					prevTdks = call.tdks
					continue
				}
			}
		}
		// failed to resolve tdk
		// break
	}
}

function findNearestTokensAt(doc: TextDocument, fileData: FixedValidationResult, call: CallChain, recursion: number): DasToken[] {
	if (call.obj.length === 0 || fileData.tokens.length === 0)
		return []
	let nearestPos = Position.create(0, 0)
	let nearestToken: DasToken = null
	let nearestAssume: DasToken = null
	let nearestAssumePos = Position.create(0, 0)
	for (const t of fileData.tokens) {
		// ignore fields, we need only top level tokens
		if (t.kind == TokenKind.ExprField)
			continue
		if (t.kind == TokenKind.ExprAssume) {
			if (t.name === call.obj && t._uri == fileData.uri && isPositionLess(t._range.start, call.objRange.start)
				&& isPositionLessOrEqual(nearestAssumePos, t._range.start)
			) {
				nearestAssumePos = t._range.start
				nearestAssume = t
			}
			continue
		}
		if (t.name === call.obj && t._uri == fileData.uri && isPositionLess(t._range.start, call.objRange.start)
			&& isPositionLessOrEqual(nearestPos, t._range.start) && t.tdk.length > 0
		) {
			nearestPos = t._range.start
			nearestToken = t
			continue
		}
	}
	if (nearestAssume != null && (nearestToken == null || isPositionLess(nearestPos, nearestAssumePos))) {
		// resolve assure expression
		// TODO: Assume.declAt is completely wrong in dascript
		const subChain = findCallChain_(doc, fileData, nearestAssume.declAt._range.end, /*forAutocompletion*/false, recursion + 1)
		// search for nearest token in subChain
		if (subChain.length > 0) {
			const last = subChain[subChain.length - 1]
			if (last.tokens.length > 0)
				return last.tokens
		}
	}
	if (nearestToken != null) {
		var res: DasToken[] = [nearestToken]
		for (const t of fileData.tokens) {
			// ignore fields, we need only top level tokens
			if (t.kind == TokenKind.ExprField || t == nearestToken)
				continue
			if (isRangeEqual(t._range, nearestToken._range)) {
				res.push(t)
			}
		}
		return res
	}
	// maybe we have exact match somewhere
	const exactName = fileData.tokens.find(t => t.name === call.obj && t._uri == fileData.uri && t.tdk.length > 0)
	return exactName != null ? [exactName] : []
}

function findTokensUnderCursor(doc: TextDocument, fileData: FixedValidationResult, position: Position): DasToken[] {
	let res: DasToken[] = []
	for (const tok of fileData.tokens) {
		if (tok._uri == fileData.uri
			&& tok._range.start.line == tok._range.end.line
			&& posInRange(position, tok._range)
			&& tok._originalText == doc.getText(tok._range)
		) {
			res.push(tok)
		}
	}
	res.sort((a, b) => isRangeLess(a._range, b._range) ? -1 : 1)
	return res
}

function repeat(ch: string, num: number) {
	let res = ''
	for (let i = 0; i < num; i++)
		res += ch
	return res
}

function fixCompletionSelf(c: CompletionItem, replaceStart: Position, cursorPos: Position): void {
	if (c.insertText != null) {
		fixCompletion(c, c.insertText, replaceStart, cursorPos)
		c.insertText = undefined
	}
}

function fixCompletion(c: CompletionItem, newText: string, replaceStart: Position, cursorPos: Position): void {
	const insertRange = Range.create(replaceStart, Position.create(replaceStart.line, replaceStart.character + newText.length))
	const delRange = Range.create(insertRange.start, cursorPos)
	if (posInRange(cursorPos, insertRange)) {
		// is enough to replace only part of the text, split text in 2 parts
		const delLen = rangeLength(delRange)
		c.additionalTextEdits = [TextEdit.replace(delRange, newText.substring(0, delLen))]
		c.textEdit = TextEdit.insert(cursorPos, newText.substring(delLen))
	} else {
		// text is too short, fully replace prefix part and just insert text as is
		c.additionalTextEdits = [TextEdit.replace(delRange, repeat(' ', rangeLength(delRange)))]
		c.textEdit = TextEdit.insert(cursorPos, newText)
	}
}

const OPERATORS = ['!', '~', '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '&&=', '||=', '^^=', '&&', '||', '^^', '+', '-',
	'*', '/', '%', '<', '>', '==', '!=', '<=', '>=', '&', '|', '^', '++', '--', '+++', '---', '<<', '>>', '<<=',
	'>>=', '<<<', '>>>', '<<<=', '>>>=', '[]', '?[]', '.', '?.', '??', ':=', '<-', '^^=']

const OPERATOR_REMAP: Map<string, string> = new Map([
	['.', '.'],
	['?.', '?.'],
	['[]', '['],
	['?[]', '?['],
])

function getGlobalCompletion(): CompletionResult {
	const globs = validatingResults.get(globalCompletionFile.uri)
	return globs ? globs.completion : null
}

function getGlobalCompletionItems(): CompletionItem[] {
	const globs = validatingResults.get(globalCompletionFile.uri)
	return globs ? globs.completionItems : null
}

connection.onCompletion(async (textDocumentPosition) => {
	const doc = documents.get(textDocumentPosition.textDocument.uri)
	if (!doc)
		return getGlobalCompletionItems()
	const fileData = await getDocumentDataFast(textDocumentPosition.textDocument.uri)
	if (!fileData)
		return getGlobalCompletionItems()
	const callChain = findCallChain(doc, fileData, textDocumentPosition.position, /*forAutocompletion*/true)
	const structCtor = findStructCtor(doc, fileData, textDocumentPosition.position, /*forAutocompletion*/true, 0)
	const res = new Array<Map<string, CompletionItem>>()
	let mergeWithFileCompletion = false
	const globalCompletion = getGlobalCompletion()
	const ignoreCallCain = structCtor.oldStyle && structCtor.name.length > 0 && (
		(
			// [[Foo() |]]
			callChain.length == 2
			&& callChain[callChain.length - 1].delimiter == Delimiter.Space
			&& callChain[callChain.length - 1].obj.length == 0
			&& callChain[callChain.length - 2].obj == structCtor.name
		) || (
			// [[Foo |]]
			callChain.length == 1
			&& callChain[callChain.length - 1].delimiter == Delimiter.Space
			&& callChain[callChain.length - 1].obj == structCtor.name
		)
	)
	if (callChain.length > 0 && !ignoreCallCain && callChain[0].delimiter != Delimiter.Assign) {
		const call = callChain.length >= 2 ? callChain[callChain.length - 2] : callChain[callChain.length - 1] // ignore last key (obj.key - we need obj)
		const replaceStart = call.objRange.end
		for (let completionTdk of call.tdks) {
			let actualTdk = completionTdk
			let typeDeclData = findTypeDecl(completionTdk, fileData.completion, globalCompletion)
			const items: CompletionItem[] = []
			if (typeDeclData != null) {
				let resTd = typeDeclCompletion(typeDeclData, fileData.completion, globalCompletion, call.delimiter, call.brackets, call.obj, items)
				if (resTd.tdk.length > 0)
					actualTdk = resTd.tdk

				for (let it of items) {
					fixCompletionSelf(it, replaceStart, textDocumentPosition.position)
					addCompletionItem(res, it)
				}
			}

			if ((call.delimiter == Delimiter.Dot || call.delimiter == Delimiter.Pipe)) {
				const tdks = [completionTdk]
				if (actualTdk != completionTdk)
					tdks.push(actualTdk)

				const tdkSuffix = `::${call.obj}`
				for (const tdk of tdks) {
					if (tdk == call.obj || tdk.endsWith(tdkSuffix))
						continue;
					// fill extension functions
					const extFn = (fn) => {
						if (fn.isClassMethod) {
							return;
						}
						if (fn.name.startsWith(PROPERTY_PREFIX)) {
							return;
						}
						// TODO: ignore const cases: Foo const == Foo
						if (fn.args.length > 0 && fn.args[0].tdk === tdk) {
							const propertyName = fixPropertyName(fn.name);
							const isProperty = propertyName != null;
							const isOperator = !isProperty && OPERATORS.includes(fn.name);
							const c = CompletionItem.create(isProperty ? propertyName : fn.name);
							c.detail = funcDetail(fn);
							c.documentation = funcDocs(fn);
							c.kind = isProperty ? CompletionItemKind.Property : isOperator ? CompletionItemKind.Operator : CompletionItemKind.Function;
							const newText = isProperty ? c.label : isOperator ? OPERATOR_REMAP.get(c.label) ?? c.label : `.${fn.name}(`;
							fixCompletion(c, newText, replaceStart, textDocumentPosition.position);
							c.sortText = isProperty ? PROPERTY_SORT : isOperator ? OPERATOR_SORT : EXTENSION_FN_SORT;
							const prev = items.find((it) => it.label === c.label && it.kind === c.kind && it.detail === c.detail && it.documentation === c.documentation);
							if (prev == null) {
								addCompletionItem(res, c);
							}
						}
					}
					fileData.completion.functions.forEach(extFn)
					if (globalCompletion)
						globalCompletion.functions.forEach(extFn)
				}
			}
		}
		if (call.delimiter == Delimiter.ColonColon && call.obj.length > 0) {
			let enumCb = (en: CompletionEnum) => {
				if (en.mod == call.obj) {
					const c = CompletionItem.create(en.name)
					c.detail = enumDetail(en)
					c.documentation = enumDocs(en)
					c.kind = CompletionItemKind.Enum
					c.sortText = FIELD_SORT
					addCompletionItem(res, c)
				}
			}
			fileData.completion.enums.forEach(enumCb)
			if (globalCompletion)
				globalCompletion.enums.forEach(enumCb)

			let structCb = (st) => {
				if (st.mod === call.obj) {
					const c = CompletionItem.create(st.name)
					c.detail = structDetail(st)
					c.documentation = structDocs(st)
					c.kind = CompletionItemKind.Struct
					c.sortText = FIELD_SORT
					addCompletionItem(res, c)
				}
			}
			fileData.completion.structs.forEach(structCb)
			if (globalCompletion)
				globalCompletion.structs.forEach(structCb)

			let fnCb = (fn) => {
				if (fn.mod == call.obj) {
					const c = CompletionItem.create(fn.name)
					c.detail = funcDetail(fn)
					c.documentation = funcDocs(fn)
					c.kind = CompletionItemKind.Function
					c.sortText = FIELD_SORT
					addCompletionItem(res, c)
				}
			}
			fileData.completion.functions.forEach(fnCb)
			if (globalCompletion)
				globalCompletion.functions.forEach(fnCb)

			let tdCb = td => {
				if (td.mod == call.obj) {
					const c = CompletionItem.create(td.name)
					c.detail = typedefDetail(td)
					c.documentation = typedefDocs(td)
					c.kind = CompletionItemKind.Interface
					c.sortText = FIELD_SORT
					addCompletionItem(res, c)
				}
			}
			fileData.completion.typeDefs.forEach(tdCb)
			if (globalCompletion)
				globalCompletion.typeDefs.forEach(tdCb)
		}
	}
	if (res.length == 0 && structCtor.name.length > 0 && (callChain.length == 0 || callChain[0].delimiter != Delimiter.Assign)) {
		let td = findTypeDefNoMod(structCtor.name, fileData.completion, globalCompletion)
		let tdName = td ? tdkName(td.tdk) : null
		let structCb = (st: CompletionStruct) => {
			if (st.name == structCtor.name || (tdName && tdName == st.name)) {
				for (const f of st.fields) {
					const c = CompletionItem.create(f.name)
					const td = findTypeDecl(f.tdk, fileData.completion, globalCompletion)
					c.insertText = `${f.name} ${typedeclAssignOperator(td)} `
					c.detail = structFieldDetail(f)
					c.documentation = structFieldDocs(f, st)
					c.kind = CompletionItemKind.Field
					c.sortText = BEFORE_ALL_SORT
					addCompletionItem(res, c)
					mergeWithFileCompletion = true
				}
			}
		}
		fileData.completion.structs.forEach(structCb)
		if (globalCompletion)
			globalCompletion.structs.forEach(structCb)
	}
	if (res.length > 0) {
		var items: CompletionItem[] = []
		for (const m of res) {
			items.push(...m.values())
		}
		if (mergeWithFileCompletion) {
			items.push(...fileData.completionItems)
		}
		return items
	}
	if (findRequirement(doc, fileData, textDocumentPosition.position)) {
		let items: CompletionItem[] = []
		for (const it of fileData.completionItems) {
			if (it.kind == CompletionItemKind.Module)
				items.push(it)
		}
		return items
	}
	return fileData.completionItems
})

connection.onHover(async (textDocumentPosition) => {
	const fileData = await getDocumentDataFast(textDocumentPosition.textDocument.uri)
	if (!fileData)
		return null
	const doc = documents.get(fileData.uri)
	if (!doc)
		return null
	const callChain = findCallChain(doc, fileData, textDocumentPosition.position, /*forAutocompletion*/false)
	if (callChain.length === 0)
		return null
	// console.log(JSON.stringify(callChain))
	const last = callChain[callChain.length - 1]
	const settings = await getDocumentSettings(textDocumentPosition.textDocument.uri)
	let globalCompletion = getGlobalCompletion()
	let res = ''
	let first = true
	for (const tok of last.tokens) {

		if (!first)
			res += '\n'
		first = false
		res += describeToken(tok, fileData.completion, globalCompletion)

		if (settings.hovers.verbose) {

			const func = findFunction(tok.name, tok.mod, fileData.completion, globalCompletion)
			if (func != null) {
				if (func.isGeneric)
					res += `\n\n${funcDetail(func)}`
				if (func.cpp.length > 0)
					res += `\n[::${func.cpp}(...)]`
			}

			if (tok.kind != TokenKind.ExprCall && tok.kind != TokenKind.Func && tok.kind != TokenKind.ExprDebug && tok.kind != TokenKind.ExprAddr && tok.tdk.length > 0) {
				for (const td of fileData.completion.typeDecls) {
					if (td.tdk === tok.tdk) {
						if (!primitiveBaseType(td, fileData.completion, globalCompletion)) {
							const doc = typeDeclDocs(td, fileData.completion, globalCompletion)
							if (doc.length > 0)
								res += `\n\n${doc}`
						}
						break
					}
				}
			}

			if (tok.kind == TokenKind.ExprCall || tok.kind == TokenKind.ExprAddr) {
				for (const fn of fileData.completion.functions) {
					if (fn.name === tok.name && fn.mod === tok.mod) {
						res += `\n\n${funcDocs(fn)}`
						break
					}
				}
				if (globalCompletion) {
					for (const fn of globalCompletion.functions) {
						if (fn.name === tok.name && fn.mod === tok.mod) {
							res += `\n\n${funcDocs(fn)}`
							break
						}
					}
				}
			}
		}
		if (false) {
			res += `\n// ${tok.kind}`
			if (tok.parentTdk.length > 0)
				res += `\n//^ ${tok.parentTdk}`

			if (tok._uri.length > 0)
				res += `\n// ${tok._uri}`
			if (!isRangeZeroEmpty(tok._range))
				res += `\n// ${JSON.stringify(tok._range)}`

			if (tok.declAt._uri.length > 0)
				res += `\n//@ ${tok.declAt._uri}`
			if (!isRangeZeroEmpty(tok.declAt._range))
				res += `\n//@ ${JSON.stringify(tok.declAt._range)}`
		}

	}
	if (res.length == 0) {
		for (const tdk of last.tdks) {
			const td = findTypeDecl(tdk, fileData.completion, globalCompletion)
			if (td) {
				if (!first)
					res += '\n'
				first = false
				res += `${last.obj} : ${typeDeclDetail(td)}`
				if (!primitiveBaseType(td, fileData.completion, globalCompletion)) {
					const doc = typeDeclDocs(td, fileData.completion, globalCompletion)
					res += `\n${doc}`
				}
			}
		}
	}
	// fallback logic, lets try to find module with same name
	if (res.length == 0 && last.obj.length > 0) {
		for (const en of fileData.completionItems) {
			if (en.kind == CompletionItemKind.Module && en.label === last.obj && en.detail) {
				if (!first)
					res += '\n'
				res += `\n${en.detail}`
				break
			}
		}
	}
	if (res.length > 0)
		return {
			contents: { kind: 'markdown', value: '```dascript\n' + res + '\n```' },
			range: last.tokens.length > 0 ? last.tokens[0]._range : Range.create(textDocumentPosition.position, textDocumentPosition.position),
		}
	return null
})

connection.onTypeDefinition(async (typeDefinitionParams) => {
	const doc = documents.get(typeDefinitionParams.textDocument.uri)
	if (!doc)
		return null
	const fileData = await getDocumentDataFast(typeDefinitionParams.textDocument.uri)
	if (!fileData)
		return null
	const callChain = findCallChain(doc, fileData, typeDefinitionParams.position, /*forAutocompletion*/false)
	if (callChain.length === 0)
		return null
	const last = callChain[callChain.length - 1]
	let res: Location[] = []
	const globalCompletion = getGlobalCompletion()

	const resTdks = last.tdks
	for (const resTdk of resTdks) {
		const td = findTypeDecl(resTdk, fileData.completion, globalCompletion)
		if (td) {
			const pos = typeDeclDefinition(td, fileData.completion, globalCompletion)
			addValidLocation(res, pos)
		}
	}

	const resAliases: Array<{ mod: string, name: string }> = last.tokens.length > 0 ? last.tokens.map(it => ({ mod: it.mod, name: it.alias })) : []
	for (const resAlias of resAliases) {
		if (resAlias.name.length === 0)
			continue
		const tf = findTypeDef(resAlias.name, resAlias.mod, fileData.completion, globalCompletion)
		if (tf) {
			addValidLocation(res, tf)
		}
	}

	for (const tok of last.tokens) {
		if (tok.kind == TokenKind.Struct || tok.kind == TokenKind.Handle) {
			// it's fast :)
			const st = findStruct(tok.name, tok.mod, fileData.completion, globalCompletion)
			if (st != null) {
				const st2 = getParentStruct(st, fileData.completion, globalCompletion)
				addValidLocation(res, st2)
			}
		}
	}
	return res.length > 0 ? res : null
})

connection.onReferences(async (referencesParams) => {
	const fileData = await getDocumentDataFast(referencesParams.textDocument.uri)
	if (!fileData)
		return null
	const doc = documents.get(fileData.uri)
	if (!doc)
		return null
	const callChain = findCallChain(doc, fileData, referencesParams.position, false)
	if (callChain.length === 0)
		return null

	let result: Location[] = []
	for (const res of callChain[callChain.length - 1].tokens) {
		let declAt = isValidLocation(res.declAt) && res.kind !== TokenKind.Func
			? res.declAt
			: res
		if (!isValidLocation(declAt)) {
			continue
		}

		addUniqueLocation(result, declAt)

		for (const td of fileData.tokens) {
			if (declAt._uri === td.declAt._uri && isRangeEqual(declAt._range, td.declAt._range)) {
				addUniqueLocation(result, td)
			}
		}
	}

	return result
})

connection.onDefinition(async (declarationParams) => {
	const doc = documents.get(declarationParams.textDocument.uri)
	if (!doc)
		return null
	const fileData = await getDocumentDataFast(declarationParams.textDocument.uri)
	if (!fileData)
		return null
	const callChain = findCallChain(doc, fileData, declarationParams.position, /*forAutocompletion*/false)
	if (callChain.length === 0)
		return null
	let res: Location[] = []
	const last = callChain[callChain.length - 1]
	for (const tok of last.tokens) {
		addValidLocation(res, tok.declAt)
	}
	if (last.tokens.length === 0 && last.delimiter != Delimiter.Pipe && last.delimiter != Delimiter.ColonColon) {
		const prev = callChain.length > 1 ? callChain[callChain.length - 2] : null
		if (prev) {
			let globalCompletion = getGlobalCompletion()
			for (const tdk of prev.tdks) {
				const typeDecl = findTypeDecl(tdk, fileData.completion, globalCompletion)
				if (typeDecl) {
					// TODO: find pos for field tdk.name
					const pos = typeDeclDefinition(typeDecl, fileData.completion, globalCompletion)
					addValidLocation(res, pos)
				}
			}
		}
	}

	return res.length > 0 ? res : null
})

connection.onDocumentSymbol(async (documentSymbolParams) => {
	const fileData = await getDocumentDataFast(documentSymbolParams.textDocument.uri)
	if (!fileData)
		return null
	const globalCompletion = getGlobalCompletion()
	const res: DocumentSymbol[] = []
	for (const glob of fileData.completion.globals) {
		if (glob._uri != documentSymbolParams.textDocument.uri)
			continue
		res.push({
			name: glob.name,
			kind: SymbolKind.Variable,
			detail: globalDetail(glob),
			range: glob._range,
			selectionRange: glob._range,
		})
	}
	for (const st of fileData.completion.structs) {
		if (st.gen)
			continue
		if (st._uri != documentSymbolParams.textDocument.uri)
			continue
		const stRes: DocumentSymbol = {
			name: st.name,
			kind: st.isClass ? SymbolKind.Class : SymbolKind.Struct,
			detail: structDetail(st),
			range: st._range,
			selectionRange: st._range,
			children: [],
		}
		for (const f of st.fields) {
			if (f.gen)
				continue
			if (f._uri != documentSymbolParams.textDocument.uri)
				continue
			stRes.children.push({
				name: f.name,
				kind: SymbolKind.Field,
				range: f._range,
				selectionRange: f._range,
			})
		}
		res.push(stRes)
	}
	for (const en of fileData.completion.enums) {
		if (en._uri != documentSymbolParams.textDocument.uri)
			continue
		const enRes: DocumentSymbol = {
			name: en.name,
			kind: SymbolKind.Enum,
			detail: enumDetail(en),
			range: en._range,
			selectionRange: en._range,
			children: [],
		}
		for (const ev of en.values) {
			if (ev._uri != documentSymbolParams.textDocument.uri)
				continue
			enRes.children.push({
				name: ev.name,
				kind: SymbolKind.EnumMember,
				detail: enumValueDetail(ev),
				range: ev._range,
				selectionRange: ev._range,
			})
		}
		res.push(enRes)
	}
	for (const td of fileData.completion.typeDefs) {
		if (td._uri != documentSymbolParams.textDocument.uri)
			continue
		let tdRes: DocumentSymbol = {
			name: td.name,
			kind: SymbolKind.Interface,
			detail: typedefDetail(td),
			range: td._range,
			selectionRange: td._range,
			children: [],
		}
		res.push(tdRes)
		const ctd = findTypeDecl(td.tdk, fileData.completion, globalCompletion)
		if (ctd != null) {
			for (const f of ctd.fields) {
				tdRes.children.push({
					name: f.name,
					kind: SymbolKind.Field,
					detail: typeDeclFieldDetail(f),
					range: td._range,
					selectionRange: td._range,
				})
			}
		}
	}
	for (const fn of fileData.completion.functions) {
		if (fn.gen)
			continue
		if (fn._uri != documentSymbolParams.textDocument.uri)
			continue
		res.push({
			name: fn.name,
			kind: SymbolKind.Function,
			detail: funcDetail(fn),
			range: fn._range,
			selectionRange: fn._range,
		})
	}
	return res
})

connection.onDocumentFormatting(async (formatParams) => {
	const doc = documents.get(formatParams.textDocument.uri)
	if (!doc)
		return null
	await validateTextDocument(doc, { autoFormat: true })
	const newText = autoFormatResult.get(formatParams.textDocument.uri)
	autoFormatResult.delete(globalCompletionFile.uri)
	if (newText == null)
		return null
	const fixedText = newText.replace(/\r\n/g, '\n')
	const allTextRange = Range.create(Position.create(0, 0), doc.positionAt(doc.getText().length))
	return [{ newText: fixedText, range: allTextRange }]
})

connection.languages.inlayHint.on(async (inlayHintParams) => {
	const doc = documents.get(inlayHintParams.textDocument.uri)
	if (!doc)
		return null
	const fileData = await getDocumentDataFast(inlayHintParams.textDocument.uri)
	if (!fileData || fileData.errors.length > 0 || doc.version != fileData.fileVersion)
		return null
	const res: InlayHint[] = []
	let idx = 0
	const n = fileData.tokens.length
	while (idx < n - 1) {
		const token = fileData.tokens[idx]
		let nextToken = fileData.tokens[idx + 1]
		++idx
		if (isPositionLessOrEqual(inlayHintParams.range.start, token._range.start)
			&& isPositionLess(token._range.end, inlayHintParams.range.end)
			&& token._uri == inlayHintParams.textDocument.uri) {
			// console.log(token)
			if (token.kind == TokenKind.ExprLet || token.kind == TokenKind.ExprFor || token.kind == TokenKind.FuncArg || token.kind == TokenKind.BlockArg) {
				// ignore let generated in dascript generateComprehension(...)
				if (token.name.startsWith('__acomp'))
					continue
				// sometimes typedecl cover whole let expression, ignore it case
				if (nextToken.kind != TokenKind.Typedecl || isRangeLengthZero(nextToken._range) || isPositionLessOrEqual(nextToken._range.start, token._range.start)) {
					const short = tdkName(token.tdk)
					res.push({
						label: `: ${short}`,
						position: token._range.end,
						kind: InlayHintKind.Type,
					})
				}
				continue
			}
			if (token.kind == TokenKind.Func) {
				if (nextToken.kind == TokenKind.FuncArg) {
					var skip = 0
					while (nextToken.kind == TokenKind.FuncArg) {
						skip += 2
						if (idx + skip >= n)
							break
						nextToken = fileData.tokens[idx + skip]
					}
				}
				if (nextToken.kind != TokenKind.Typedecl || isRangeLengthZero(nextToken._range)) {
					const short = tdkName(token.tdk)
					res.push({
						label: `: ${short}`,
						position: closedBracketPos(doc, token._range.end),
						kind: InlayHintKind.Type,
					})
				}
			}

		}
	}
	return res
})

connection.onInitialized(async () => {
	if (hasConfigurationCapability) {
		// Register for all configuration changes.
		connection.client.register(DidChangeConfigurationNotification.type, undefined)
	}
	if (hasWorkspaceFolderCapability) {
		connection.workspace.onDidChangeWorkspaceFolders(_event => {
			if (_event.added.length == 0 && _event.removed.length == 0)
				return
			for (const ws of _event.removed) {
				const idx = workspaceFolders.indexOf(ws)
				if (idx != -1)
					workspaceFolders.splice(idx, 1)
			}
			for (const ws of _event.added) {
				if (workspaceFolders.indexOf(ws) == -1)
					workspaceFolders.push(ws)
			}
			forceUpdateAllDocuments()
		})
	}

	let config = await connection.workspace.getConfiguration({
		scopeUri: 'resource',
		section: 'dascript'
	})

	if (config?.project?.scanWorkspace) {
		validateWorkspaceCommand({});
	}
})

connection.onExecuteCommand(async (params: any) => {
	if (!serverCommandHandlers[params.command]) {
		return;
	}

	serverCommandHandlers[params.command](params?.args);
})

let globalSettings = defaultSettings

connection.onDidChangeConfiguration(change => {
	if (hasConfigurationCapability) {
		// Reset all cached document settings
		documentSettings.clear()
	} else {
		globalSettings = <DasSettings>((change.settings || defaultSettings))
	}

	forceUpdateAllDocuments()
})

function getDocumentSettings(resource: string): Thenable<DasSettings> {
	if (!hasConfigurationCapability) {
		return Promise.resolve(globalSettings)
	}
	let result = documentSettings.get(resource)
	if (!result) {
		result = connection.workspace.getConfiguration({
			scopeUri: resource,
			section: 'dascript'
		})
		documentSettings.set(resource, result)
	}
	return result
}

function stringHashCode(str: string) {
	let hash = 0
	if (str.length === 0) return hash
	for (let i = 0; i < str.length; i++) {
		const chr = str.charCodeAt(i)
		hash = ((hash << 5) - hash) + chr
		hash |= 0 // Convert to 32bit integer
	}
	return hash
}

let validateId = 0

interface ValidatingProcess {
	version: integer
	process: ChildProcessWithoutNullStreams
	promise: Promise<void>
}

const globalCompletionFile = TextDocument.create('$$$completion$$$.das', 'dascript', 1, '// empty')
const validatingProcesses = new Map<string, ValidatingProcess>()

// async function getDocumentDataRaw(uri: string, doc: TextDocument): Promise<FixedValidationResult> {
// 	const data = validatingResults.get(uri)
// 	if (data && data.fileVersion === doc.version)
// 		return data
// 	const loadingData = validatingProcesses.get(uri)
// 	if (loadingData) {
// 		return loadingData.promise.then(() => {
// 			return validatingResults.get(uri)
// 		})
// 	}
// 	return validateTextDocument(doc).then(() => {
// 		return validatingResults.get(uri)
// 	})
// }

function forceUpdateAllDocuments() {
	validatingResults.clear() // workspace was changes, restart all validations
	connection.languages.inlayHint.refresh()
	// documents.all().forEach(doc => getDocumentData(doc.uri))
}

async function updateTextDocumentData(doc: TextDocument) {
	connection.languages.inlayHint.refresh()
	return validateTextDocument(globalCompletionFile).then(() => {
		validateTextDocument(doc, { force: true })
	})
}

async function getDocumentDataFast(uri: string): Promise<FixedValidationResult> {
	const data = validatingResults.get(uri)
	if (data)
		return data
	const req = validatingProcesses.get(uri)
	if (req) {
		return req.promise.then(() => {
			return getDocumentDataFast(uri)
		})
	}
	return null

}
// async function getDocumentData(uri: string): Promise<FixedValidationResult> {
// 	return validateTextDocument(globalCompletionFile).then(() => {
// 		let doc = documents.get(uri)
// 		if (doc) {
// 			return validateTextDocument(doc).then(() => {
// 				return validatingResults.get(uri)
// 			})
// 		}
// 		return null
// 	})
// }

async function validateWorkspaceCommand(args: any = {}): Promise<void> {

	let config = await connection.workspace.getConfiguration({
		scopeUri: 'resource',
		section: 'dascript'
	})

	let params = setWorkspaceValidationParams(config);

	let timerName: string = 'validateWorkspace';

	console.time(timerName);
	console.log('Validation data cache folder', params.cacheFolder);

	let folders = args?.folder ? [<WorkspaceFolder>{ uri: args?.folder, name: '' }] : workspaceFolders

	for (const folder of folders.map(f => URI.parse(f.uri).fsPath)) {
		console.log("Validating workspace folder", folder);
		await validateWorkspaceFolder(folder, params);
	}

	console.timeEnd(timerName);
}

async function clearCachedValidationDataCommand(): Promise<void> {
	let config = await connection.workspace.getConfiguration({
		scopeUri: 'resource',
		section: 'dascript'
	})

	let params = setWorkspaceValidationParams(config);

	for (const folder of workspaceFolders.map(f => URI.parse(f.uri).fsPath)) {
		fs.rmSync(
			path.join(params.cacheFolder, path.basename(folder)),
			{ recursive: true }
		);
	}
}

// function sendDiagnostics(result: ValidationResult, file: string, settings: DasSettings): Map<string, Diagnostic[]> {
// 	const diagnostics: Map<string, Diagnostic[]> = new Map();

// 	for (const error of result.errors) {
// 		error._range = AtToRange(error)
// 		error._uri = AtToUri(error, file, settings, workspaceFolders, result.dasRoot)
// 		if (error._uri.length === 0)
// 			error._uri = file

// 		let msg = error.what.trim()
// 		if (error.extra?.length > 0 || error.fixme?.length > 0) {
// 			var suffix = ''
// 			if (error.extra?.length > 0)
// 				suffix += error.extra.trim()
// 			if (error.fixme?.length > 0)
// 				suffix += (suffix.length > 0 ? '\n' : '') + error.fixme.trim()

// 			msg = `${msg}\n\n${suffix}`
// 		}
// 		const diag: Diagnostic = {
// 			range: error._range,
// 			message: msg,
// 			code: error.cerr,
// 			severity: error.level === 0 ? DiagnosticSeverity.Error : DiagnosticSeverity.Warning,
// 		}
// 		if (!diagnostics.has(error._uri))
// 			diagnostics.set(error._uri, [])
// 		diagnostics.get(error._uri).push(diag)
// 	}

// 	for (const [uri, diags] of diagnostics.entries()) {
// 		connection.sendDiagnostics({ uri: uri, diagnostics: diags })
// 	}

// 	return diagnostics;
// }

async function loadCachedValidationData(validationCacheFile: string): Promise<ValidationResult | null> {
	let fileContent: string;
	let parsedContent: ValidationResult

	try {
		fileContent = await fs.promises.readFile(validationCacheFile, { encoding: 'utf8' });
	}
	catch (e) {
		console.log(`Validation cache file not found, ${validationCacheFile}`, e);
		return null;
	}

	try {
		parsedContent = JSON.parse(fileContent) as ValidationResult;
	}
	catch (e) {
		console.log('Failed to parse', validationCacheFile);
		return null;
	}

	console.log(`Found validation cache file, ${validationCacheFile}`);

	return parsedContent;
}

async function collectWorkspaceFiles(dir: string) {
	let foldersToVisit: string[] = [dir];
	let result: URI[] = [];

	while (foldersToVisit.length !== 0) {
		const files = await promisify(readdir)(foldersToVisit[0]);

		for (const file of files) {
			if (file.startsWith('.')) {
				continue;
			}
			const path = join(foldersToVisit[0], file);

			const stat = await fs.promises.stat(join(foldersToVisit[0], file))

			if (stat.isDirectory()) {
				foldersToVisit.push(path);
			}
			else if (path.endsWith(".das")) {
				result.push(URI.parse(path));
			}
		}

		foldersToVisit.shift();
	}

	return result;
}

async function startQueueValidationJob(dir: string, file: string, params: WorkspaceValidationParams): Promise<void> {
	const cacheFileName: string = `${mangleFileUri(path.relative(dir, file))}.json`;
	const cacheFilePath: string = params.saveCache ? path.join(params.cacheFolder, path.basename(dir), cacheFileName) : null;
	const textDocument = TextDocument.create(
		file,
		'dascript',
		1,
		(await fs.promises.readFile(file)).toString()
	);

	let validationResult = params.saveCache ? await loadCachedValidationData(cacheFilePath) : null;
	if (!validationResult) {
		await validateTextDocument(textDocument);

		if (params.saveCache) {
			await fs.promises.writeFile(cacheFilePath, JSON.stringify(validatingResults.get(file)));
		}
	}
	else {
		const settings = await getDocumentSettings(textDocument.uri);
		storeValidationResult(settings, textDocument, validationResult);
	}
}

async function validateWorkspaceFolder(dir: string, params: WorkspaceValidationParams): Promise<void> {
	return; // TODO: temporary disable scanning
	let validatingQueue: ValidatingQueue = new ValidatingQueue(params?.queueCapacity);
	const token = `validation/${dir}`;

	await connection.sendRequest("window/workDoneProgress/create", { token });
	await connection.sendNotification(
		"$/progress",
		{
			token,
			value: <WorkDoneProgressBegin>{
				kind: "begin",
				title: `Workspace validation`,
				percentage: 0,
			}
		}
	);

	const files = await collectWorkspaceFiles(dir)

	if (params?.saveCache) {
		let cacheFolder = path.join(params.cacheFolder, path.basename(dir));

		if (!await fs.existsSync(cacheFolder)) {
			await fs.promises.mkdir(cacheFolder, { recursive: true });
		}
	}

	let i = 1
	for (const file of files) {
		await connection.sendNotification(
			"$/progress",
			{
				token,
				value: <WorkDoneProgressReport>{
					kind: "report",
					message: `${i}/${files.length} (${path.basename(dir)}/${path.relative(dir, file.fsPath)})`,
					percentage: (i / files.length) * 100,
				}
			}
		);
		await validatingQueue.enqueue(file.fsPath, async () => await startQueueValidationJob(dir, file.fsPath, params));
		i++
	}

	await validatingQueue.waitAll();
	await connection.sendNotification(
		"$/progress",
		{ token, value: <WorkDoneProgressEnd>{ kind: "end" } }
	);
}

async function validateTextDocument(textDocument: TextDocument, extra: { autoFormat?: boolean, force?: boolean } = { autoFormat: false, force: false }): Promise<void> {
	const registerValidatingResult = !extra.autoFormat
	// TODO: limit validating processes
	if (registerValidatingResult) {
		const prevProcess = validatingProcesses.get(textDocument.uri)
		if (prevProcess) {
			if (prevProcess.version === textDocument.version) {
				// TODO: remove
				console.log('document version not changed, waiting for previous process', textDocument.uri)
				return prevProcess.promise
			}
			prevProcess.process?.kill()
			validatingProcesses.delete(textDocument.uri)
			console.log('killed process for', textDocument.uri, 'prev version', prevProcess.version, 'new version', textDocument.version)
		}
		const validResult = validatingResults.get(textDocument.uri)
		if (validResult?.fileVersion === textDocument.version) {
			// TODO: remove
			console.log('document version not changed, ignoring', textDocument.uri)
			return Promise.resolve()
		}
	}

	const vp: ValidatingProcess = { process: null, version: textDocument.version, promise: null }
	var thisResolve: () => void
	var thisReject: (any) => void
	vp.promise = new Promise<void>((resolve, reject) => {
		thisResolve = resolve
		thisReject = reject
	})
	if (registerValidatingResult)
		validatingProcesses.set(textDocument.uri, vp)

	const settings = await getDocumentSettings(textDocument.uri)

	if (registerValidatingResult) {
		var prevProcess = validatingProcesses.get(textDocument.uri)
		if (prevProcess != null && prevProcess.version > textDocument.version) {
			// version was changed while we were waiting for settings
			return prevProcess.promise
		}
		const validResult = validatingResults.get(textDocument.uri)
		if (validResult != null && validResult.fileVersion > textDocument.version) {
			// version was changed while we were waiting for settings
			return Promise.resolve()
		}
	}

	const filePath = URI.parse(textDocument.uri).fsPath
	const tempFilePrefix = `${stringHashCode(textDocument.uri).toString(16)}_${validateId.toString(16)}_${textDocument.version.toString(16)}`
	const tempFileName = `${tempFilePrefix}_${path.basename(filePath)}`
	const resultFileName = `${tempFileName}_res`
	validateId++
	const tempFilePath = path.join(os.tmpdir(), tempFileName)
	const resultFilePath = path.join(os.tmpdir(), resultFileName)
	await fs.promises.writeFile(tempFilePath, textDocument.getText())
	await fs.promises.writeFile(resultFilePath, '')

	const workspaceFolder = URI.parse(workspaceFolders![0].uri).fsPath
	const args = settings.server.args.map(
		p => p.replace('${file}', 'validate_file.das').replace('${workspaceFolder}', workspaceFolder)
	)
	if (args.indexOf('--') < 0)
		args.push('--')
	args.push('--file', tempFilePath)
	args.push('--original-file', filePath)
	args.push('--result', resultFilePath)
	if(settings.compiler) {
		settings.compiler = settings.compiler.replace('${workspaceFolder}', workspaceFolder)
	}
	if (settings.project.file)
	{
		settings.project.file = settings.project.file.replace('${workspaceFolder}', workspaceFolder)
		args.push('--project-file', settings.project.file)
	}	
	if (settings.policies?.ignore_shared_modules)
		args.push('--ignore-shared-modules')
	if (settings.policies?.no_global_variables)
		args.push('--no-global-variables')
	if (settings.policies?.no_unused_block_arguments)
		args.push('--no-unused-block-arguments')
	if (settings.policies?.no_unused_function_arguments)
		args.push('--no-unused-function-arguments')
	if (settings.policies?.fail_on_lack_of_aot_export)
		args.push('--fail-on-lack-of-aot-export')
	if (textDocument == globalCompletionFile)
		args.push('--global-completion')
	for (const rootName in settings.project.fileAccessRoots) {
		const fixedRoot = settings.project.fileAccessRoots[rootName].replace('${workspaceFolder}', workspaceFolder)
		args.push('--file-access-root', `${rootName}:${fixedRoot}`)
	}
	if (extra.autoFormat)
		args.push('--auto-format')

	const scriptPath = process.argv[1]
	const cwd = path.dirname(path.dirname(scriptPath))
	console.log(`> validating ${textDocument.uri} version ${textDocument.version}`)
	console.log('> cwd', cwd)
	console.log('> exec', settings.compiler, args.join(' '))
	const child = spawn(settings.compiler, args, { cwd: cwd })
	vp.process = child

	const diagnostics: Map<string, Diagnostic[]> = new Map()
	diagnostics.set(textDocument.uri, [])
	let output = ''
	child.stdout.on('data', (data: any) => {
		output += data
	})
	child.stderr.on('data', (data: any) => {
		diagnostics.get(textDocument.uri).push({ range: Range.create(0, 0, 0, 0), message: `${data}` })
	})
	child.on('error', (error: any) => {
		diagnostics.get(textDocument.uri).push({ range: Range.create(0, 0, 0, 0), message: `${error}` })
		thisReject(error)
	})
	child.on('close', (exitCode: any) => {
		if (registerValidatingResult) {
			validatingProcesses.delete(textDocument.uri)
		}
		const validateTextResult = fs.readFileSync(resultFilePath, 'utf8')
		// console.log('remove temp files', tempFilePath, resultFilePath)
		try {
			fs.rmSync(tempFilePath)
			fs.rmSync(resultFilePath)
		}
		catch (e) {
			console.log('failed to remove temp files', e)
		}

		if (exitCode === null) {
			console.log('Validation process exited with code', exitCode)
			thisResolve()
			return
		}

		if (vp.version !== textDocument.version) {
			console.log('document version changed, ignore result. Current', vp.version, "got", textDocument.version, textDocument.uri)
			thisResolve()
			return
		}

		let doc = documents.get(textDocument.uri)
		if (doc == null) {
			console.log('document was closed, ignore result', textDocument.uri)
			thisResolve()
			return
		}

		if (extra.autoFormat) {
			autoFormatResult.set(textDocument.uri, validateTextResult)
			thisResolve()
			return
		}

		if (textDocument.uri != globalCompletionFile.uri) {
			let prev = documents.get(textDocument.uri);
			if (prev && prev.version !== textDocument.version) {
				console.log('document version changed, ignore prev result. Current', prev.version, "got", textDocument.version, textDocument.uri)
				thisResolve()
				return
			}
		}

		// console.log(validateTextResult)
		let result: ValidationResult = null
		try {
			if (validateTextResult.length > 0)
				result = JSON.parse(validateTextResult) as ValidationResult
		}
		catch (e) {
			console.log('failed to parse result', e)
			console.log('"""', validateTextResult, '"""')
		}
		if (result != null) {
			for (const error of result.errors) {
				error._range = AtToRange(error)
				error._uri = AtToUri(error, filePath, settings, workspaceFolders, result.dasRoot)
				if (error._uri.length === 0)
					error._uri = textDocument.uri

				let msg = error.what.trim()
				if (error.extra?.length > 0 || error.fixme?.length > 0) {
					var suffix = ''
					if (error.extra?.length > 0)
						suffix += error.extra.trim()
					if (error.fixme?.length > 0)
						suffix += (suffix.length > 0 ? '\n' : '') + error.fixme.trim()

					msg = `${msg}\n\n${suffix}`
				}
				const diag: Diagnostic = {
					range: error._range,
					message: msg,
					code: error.cerr,
					severity: error.level === 0 ? DiagnosticSeverity.Error : DiagnosticSeverity.Warning,
				}
				if (!diagnostics.has(error._uri))
					diagnostics.set(error._uri, [])
				diagnostics.get(error._uri).push(diag)
			}
			console.time('storeValidationResult')
			// console.log("validatingResults uri", textDocument.uri, "version", vp.version, "exitCode", exitCode, "actual version", textDocument.version)
			storeValidationResult(settings, textDocument, result, diagnostics)

			console.timeEnd('storeValidationResult')
		} else { // result == null
			if (!diagnostics.has(textDocument.uri))
				diagnostics.set(textDocument.uri, [])
			diagnostics.get(textDocument.uri).push({ range: Range.create(0, 0, 0, 0), message: `internal error: Validation process exited with code ${exitCode}.` })
			console.log(`internal error: Validation process exited with code ${exitCode}. But no errors were reported. Please report this issue.`)
			console.log('"""', output, '"""')
			console.log('"""', args, '"""')
		}
		for (const [uri, diags] of diagnostics.entries()) {
			connection.sendDiagnostics({ uri: uri, diagnostics: diags })
		}
		thisResolve()
	})
	return vp.promise
}

function addCompletionItem(map: Array<Map<string, CompletionItem>>, item: CompletionItem) {
	while (map.length <= item.kind)
		map.push(new Map())
	const items = map[item.kind]
	const it = items.get(item.label)
	if (it != null) {
		if (item.documentation) {
			let wasDoc = it.documentation
			let newDoc = item.documentation
			if (typeof wasDoc != 'string') {
				wasDoc = wasDoc.value
			}
			if (typeof newDoc != 'string') {
				newDoc = newDoc.value
			}
			if (wasDoc != newDoc) {
				if (typeof item.documentation != 'string') {
					let prefixLen = '```dascript\n'.length
					newDoc = newDoc.substring(prefixLen, newDoc.length - 3)
				}
				if (typeof it.documentation == 'string') {
					it.documentation += '\n\n' + newDoc
				}
				else {
					let mk = it.documentation as MarkupContent
					mk.value = mk.value.substring(0, mk.value.length - 3) + '\n\n' + newDoc + '\n```'
				}
			}
		}
		if (item.insertText)
			it.insertText = item.insertText
		return
	}
	items.set(item.label, item)
}

function baseTypeToCompletionItemKind(baseType: string) {
	if (baseType === 'block' || baseType === 'lambda' || baseType === 'function')
		return CompletionItemKind.Function

	return CompletionItemKind.Struct
}

function storeValidationResult(settings: DasSettings, doc: TextDocument, res: ValidationResult, diagnostics: Map<string, Diagnostic[]> = new Map()) {
	const uri = doc.uri
	const fileVersion = doc.version
	const filePath = URI.parse(doc.uri).fsPath
	console.log('storeValidationResult', uri, 'version', fileVersion)
	const fixedResults: FixedValidationResult = { ...res, uri: uri, completionItems: [], fileVersion: fileVersion, filesCache: new Map(), diagnostics }
	if (res.errors.length > 0 && validatingResults.has(uri)) {
		// keep previous completion items
		const prev = validatingResults.get(uri)

		fixedResults.completion = prev.completion
		fixedResults.completionItems = prev.completionItems
		fixedResults.tokens = prev.tokens
	}
	else {
		let globalCompletionRes = uri != globalCompletionFile.uri ? validatingResults.get(globalCompletionFile.uri) : null
		let globalCompletion = globalCompletionRes ? globalCompletionRes.completion : null
		let isGlobalCompletion = uri == globalCompletionFile.uri

		let modules = new Set<string>()
		let usedModules: Set<string> = new Set()
		function addUsedModule(mod: string) {
			if (mod.length > 0)
				usedModules.add(mod)
		}
		const completionMap = new Array<Map<string, CompletionItem>>()
		for (const name of fixedResults.builtinMods) {
			modules.add(name)
			if (isGlobalCompletion)
				addCompletionItem(completionMap, {
					label: name,
					kind: CompletionItemKind.Module,
					detail: `module ${name}`,
					documentation: `(builtin module)`,
					sortText: MODULE_SORT,
				})
		}
		function addMod(name: string, at: CompletionAt) {
			if (name?.length == 0)
				return
			if (!modules.has(name)) {
				modules.add(name)
				addCompletionItem(completionMap, {
					label: name,
					kind: CompletionItemKind.Module,
					detail: `module ${name}`,
					documentation: `module ${name}\n${at.file}`,
					sortText: MODULE_SORT,
				})

			}
			if (at._uri == uri)
				addUsedModule(name)
		}
		for (const e of res.completion.enums) {
			e._range = AtToRange(e)
			e._uri = AtToUri(e, filePath, settings, workspaceFolders, res.dasRoot, fixedResults.filesCache)
			addCompletionItem(completionMap, {
				label: e.name,
				kind: CompletionItemKind.Enum,
				detail: enumDetail(e),
				documentation: enumDocs(e),
				sortText: MODULE_SORT,
			})
			addMod(e.mod, e)
			for (const ev of e.values) {
				ev._range = AtToRange(ev)
				ev._uri = AtToUri(ev, filePath, settings, workspaceFolders, res.dasRoot, fixedResults.filesCache)
				addCompletionItem(completionMap, {
					label: ev.name,
					kind: CompletionItemKind.EnumMember,
					detail: enumValueDetail(ev),
					documentation: enumValueDocs(ev, e),
					sortText: MODULE_SORT,
				})
			}
		}
		for (const s of res.completion.structs) {
			s.column++ // magic number to fix column
			s._range = AtToRange(s)
			s._uri = AtToUri(s, filePath, settings, workspaceFolders, res.dasRoot, fixedResults.filesCache)
			addCompletionItem(completionMap, {
				label: s.name,
				kind: s.isClass ? CompletionItemKind.Class : CompletionItemKind.Struct,
				detail: structDetail(s),
				documentation: structDocs(s),
				sortText: MODULE_SORT,
			})
			addMod(s.mod, s)
			if (s._uri == uri && s.parentMod.length > 0)
				addMod(s.parentMod, s)
			for (const sf of s.fields) {
				// TODO: search for the field end using textDocument.getText()
				// sf.columnEnd += sf.tdk.length + 1 // 1 char for ':'
				sf._range = AtToRange(sf)
				sf._uri = AtToUri(sf, filePath, settings, workspaceFolders, res.dasRoot, fixedResults.filesCache)
				addCompletionItem(completionMap, {
					label: sf.name,
					kind: CompletionItemKind.Field,
					detail: structFieldDetail(sf),
					documentation: structFieldDocs(sf, s),
					sortText: MODULE_SORT,
				})
			}
		}
		for (const t of res.completion.typeDecls) {
			t._range = AtToRange(t)
			t._uri = AtToUri(t, filePath, settings, workspaceFolders, res.dasRoot, fixedResults.filesCache)
			//SKIP it, a lot of data, to reduce completion items
			// addCompletionItem(completionMap, {
			// 	label: t.tdk,
			// 	kind: baseTypeToCompletionItemKind(t.baseType),
			// 	detail: typeDeclDetail(t),
			// 	documentation: typeDeclDocs(t, res.completion, globalCompletion),
			// 	sortText: MODULE_SORT,
			// })
			// for (const tf of t.fields) {
			// 	addCompletionItem(completionMap, {
			// 		label: tf.name,
			// 		kind: CompletionItemKind.Field,
			// 		detail: typeDeclFieldDetail(tf),
			// 		documentation: typeDeclFieldDocs(tf, t),
			// 		sortText: MODULE_SORT,
			// 	})
			// }
			if (t._uri == uri)
				addUsedModule(t.mod)
		}
		for (const t of res.completion.typeDefs) {
			t._range = AtToRange(t)
			t._uri = AtToUri(t, filePath, settings, workspaceFolders, res.dasRoot, fixedResults.filesCache)
			const valueType = findTypeDecl(t.tdk, res.completion, globalCompletion)
			addCompletionItem(completionMap, {
				label: t.name,
				kind: valueType != null ? baseTypeToCompletionItemKind(valueType.baseType) : CompletionItemKind.Struct,
				detail: typedefDetail(t),
				documentation: typedefDocs(t),
				sortText: MODULE_SORT,
			})
			addMod(t.mod, t)
		}
		for (const g of res.completion.globals) {
			g._range = AtToRange(g)
			g._uri = AtToUri(g, filePath, settings, workspaceFolders, res.dasRoot, fixedResults.filesCache)
			addCompletionItem(completionMap, {
				label: g.name,
				kind: CompletionItemKind.Variable,
				detail: globalDetail(g),
				documentation: globalDocs(g),
				sortText: MODULE_SORT,
			})
			addMod(g.mod, g)
		}
		for (const f of res.completion.functions) {
			f._range = AtToRange(f)
			f._uri = AtToUri(f, filePath, settings, workspaceFolders, res.dasRoot, fixedResults.filesCache)
			f.decl._range = AtToRange(f.decl)
			f.decl._uri = AtToUri(f.decl, filePath, settings, workspaceFolders, res.dasRoot, fixedResults.filesCache)
			if ((f.args.length == 1 || f.args.length == 2) && f.name.startsWith(PROPERTY_PREFIX)) {
				// let found = false
				for (const argTdk of f.args[0].tdk) {
					const td = findTypeDecl(argTdk, res.completion, globalCompletion)
					if (td) {
						typeDeclIter(td, res.completion, globalCompletion, (td, st, en) => {
							if (st) {
								// found = true
								let name = f.name.substring(2)
								let writeProp = false
								if (name.endsWith('`clone')) {
									name = name.substring(0, name.length - 6)
									writeProp = true
								}
								let prop = st.fields.find(f => f.name === name && f._property)
								if (prop) {
									if (writeProp)
										prop._writeFn = f
									else
										prop._readFn = f
								} else {
									st.fields.push({
										name: name,
										tdk: f.tdk,
										offset: -1,
										isPrivate: false,
										_range: f._range,
										_uri: f._uri,
										gen: f.gen,
										file: f.file,
										line: f.line,
										lineEnd: f.lineEnd,
										column: f.column,
										columnEnd: f.columnEnd,
										_originalText: f._originalText,
										_property: true,
										_readFn: !writeProp ? f : null,
										_writeFn: writeProp ? f : null,
									})
								}
							}
						})
					}
					// if (found)
					// 	continue
				}
			}
			addMod(f.mod, f)
			if (f._uri == uri)
				addUsedModule(f.origMod)
			for (const arg of f.args) {
				arg._range = AtToRange(arg)
				arg._uri = AtToUri(arg, filePath, settings, workspaceFolders, res.dasRoot, fixedResults.filesCache)
				addCompletionItem(completionMap, {
					label: arg.name,
					kind: CompletionItemKind.Variable,
					detail: funcArgDetail(arg),
					documentation: funcArgDocs(arg),
					sortText: MODULE_SORT,
				})
			}
		}

		const tokens = fixedResults.tokens
		fixedResults.tokens = []

		var tokenIdx = -1
		var prevToken: DasToken = null
		for (const token of tokens) {
			tokenIdx++
			token._uri = AtToUri(token, filePath, settings, workspaceFolders, res.dasRoot, fixedResults.filesCache)
			if (token._uri != uri) // filter out tokens from other files
				continue
			addUsedModule(token.mod)
			token._range = AtToRange(token)
			token._originalText = doc.getText(token._range)
			if (token.kind == TokenKind.Struct) {
				token._range.start.character++ // magic number to fix column
				token._originalText = doc.getText(token._range)
			}
			else if (token.kind == TokenKind.ExprField) {
				if (token._originalText == '->') {
					// convert foo|->|bar to foo->|bar|
					token._range.start.character += 2
					token._range.end.character += token.name.length
					token._originalText = doc.getText(token._range)
				}
				else if (token.name == 'self' && rangeLength(token._range) < token.name.length) {
					// skip autogenerated self-s
					token.declAt.line = -1
				}
			}
			else if (token.kind == TokenKind.Typedecl) {
				if (prevToken && isRangeEqual(prevToken._range, token._range)) {
					// skip autogenerated typedecl-s with the same range
					token._range.start.character = token._range.end.character
					token._originalText = ''
					token.declAt.line = -1
				}
			}
			// declAt is negative for tokens that are not declared in the source code
			if (token.declAt.line >= 0) {
				token.declAt._range = AtToRange(token.declAt)
				token.declAt._uri = AtToUri(token.declAt, filePath, settings, workspaceFolders, res.dasRoot, fixedResults.filesCache)
			}
			else {
				token.declAt._range = Range.create(0, 0, 0, 0)
				token.declAt._uri = ''
			}
			fixedResults.tokens.push(token)
			// TODO: add completion items for tokens
			if (token.kind == TokenKind.ExprLet) {
				addCompletionItem(completionMap, {
					label: token.name,
					kind: CompletionItemKind.Variable,
					detail: token.name,
					documentation: describeToken(token, res.completion, globalCompletion),
					sortText: MODULE_SORT,
				})
			}

			if (isRangeZeroEmpty(token.declAt._range)) {
				// moved to validate_file.das
				// if (token.kind == TokenKind.ExprAddr) {
				// 	// function call
				// 	const func = mergedCompletion.functions.find(f => f.name === token.name && f.mod === token.mod)
				// 	if (func) {
				// 		token.declAt = func
				// 		addUsedModule(token.mod)
				// 	}
				// }
				if (token.kind == TokenKind.Typedecl) {
					if (!isRangeLengthZero(token._range)) {
						const td = findTypeDecl(token.tdk, res.completion, globalCompletion)
						if (td != null) {
							addUsedModule(td.mod)
							const td2 = typeDeclDefinition(td, res.completion, globalCompletion)
							if (td2 != null) {
								token.declAt = td2
							}
						}
						if (isRangeZeroEmpty(token.declAt._range) && token.alias.length > 0) {
							const td = findTypeDef(token.alias, token.mod, res.completion, globalCompletion)
							if (td != null) {
								addUsedModule(td.mod)
								token.declAt = td
							}
						}
					}
				}
				else if (token.kind == TokenKind.Struct || token.kind == TokenKind.Handle) {
					const st = findStruct(token.name, token.mod, res.completion, globalCompletion)
					if (st) {
						addUsedModule(st.mod)
						token.declAt = st
					}
				}
				else if (token.kind == TokenKind.ExprConstEnumeration) {
					const td = findTypeDecl(token.tdk, res.completion, globalCompletion)
					if (td && td.enumName.length > 0) {
						const en = findEnum(td.enumName, td.mod, res.completion, globalCompletion)
						if (en) {
							addUsedModule(en.mod)
							for (const ev of en.values) {
								const reqName = `${en.name} ${ev.name}`
								if (token.name == reqName) {
									token.declAt = ev
									break
								}
							}
							if (isRangeZeroEmpty(token.declAt._range))
								token.declAt = en
						}
					}
				}
			}
			// if (isRangeZeroEmpty(token.declAt._range) && token.kind != TokenKind.Func && token.kind != TokenKind.ExprDebug) {
			// 	if (token.tdk.length > 0) {
			// 		const td = mergedCompletion.typeDecls.find(td => td.tdk === token.tdk)
			// 		token.declAt = td
			// 	}
			// }
			if (isRangeZeroEmpty(token.declAt._range) && token.parentTdk.length > 0) {
				const parentTypeDecl = findTypeDecl(token.parentTdk, res.completion, globalCompletion)
				if (parentTypeDecl) {
					typeDeclIter(parentTypeDecl, res.completion, globalCompletion, (td, st, en, tf) => {
						if (st) {
							addUsedModule(st.mod)
							const field = st.fields.find(f => f.name === token.name)
							if (field) {
								token.declAt = field._property ? (field._writeFn.decl ?? field._readFn.decl) : field
							}
						}
						if (en) {
							addUsedModule(en.mod)
							const value = en.values.find(v => v.name === token.name)
							if (value) {
								token.declAt = value
							}
						}
					})

				}
				if (isRangeZeroEmpty(token.declAt._range))
					token.declAt = typeDeclDefinition(parentTypeDecl, res.completion, globalCompletion)
			}

			for (const token of fixedResults.tokens) {
				if (token._uri != uri)
					continue
				if (!isRangeZeroEmpty(token.declAt._range))
					continue

				if (token.kind == TokenKind.ExprGoto) {
					const label = token.name
					if (label.length > 0) {
						let found = false
						let prevIdx = tokenIdx - 1
						while (prevIdx >= 0) {
							const prevToken = tokens[prevIdx]
							if (prevToken.kind == TokenKind.ExprLabel && prevToken.name == label && prevToken._uri == uri) {
								token.declAt = prevToken
								found = true
								break
							}
							if (prevToken.kind == TokenKind.Func)
								break
							prevIdx--
						}
						if (!found) {
							var nextIdx = tokenIdx + 1
							while (nextIdx < tokens.length) {
								const nextToken = tokens[nextIdx]
								if (nextToken.kind == TokenKind.ExprLabel && nextToken.name == label && nextToken._uri == uri) {
									token.declAt = nextToken
									found = true
									break
								}
								if (nextToken.kind == TokenKind.Func)
									break
								nextIdx++
							}
						}
					}
				}
			}

			// mod usages data
			if (token.kind == TokenKind.ExprCall || token.kind == TokenKind.ExprAddr) {
				for (const fn of fixedResults.completion.functions) {
					if (fn.name === token.name && fn.mod === token.mod) {
						addUsedModule(fn.mod)
						addUsedModule(fn.origMod)
					}
				}
				if (globalCompletion) {
					for (const fn of globalCompletion.functions) {
						if (fn.name === token.name && fn.mod === token.mod) {
							addUsedModule(fn.mod)
							addUsedModule(fn.origMod)
						}
					}
				}
			}
			//fallback to type decl
			if (token.kind != TokenKind.Func && token.kind != TokenKind.ExprDebug && token.kind != TokenKind.ExprAddr && token.tdk.length > 0) {
				const td = findTypeDecl(token.tdk, res.completion, globalCompletion)
				if (td) {
					addUsedModule(td.mod)
				}
			}

			prevToken = token
		}

		for (const f of fixedResults.completion.functions) {
			if (f.name.startsWith('builtin`')) {
				f.name = f.name.substring(8)
			}
			else {
				let prefixIdx = f.name.indexOf('`')
				if (prefixIdx >= 0) {
					if (prefixIdx == 0 || usedModules.has(f.name.substring(0, prefixIdx)))
						f.name = f.name.substring(prefixIdx + 1)
				}
			}

			addCompletionItem(completionMap, {
				label: f.name,
				kind: CompletionItemKind.Function,
				detail: funcDetail(f),
				documentation: funcDocs(f),
				sortText: MODULE_SORT,
			})
		}

		var allReq = new Map<string, { origin: ModuleRequirement, depth: number }>()

		for (const mod of fixedResults.requirements) {
			mod._uri = uri
			mod._range = AtToRange(mod)
			if (mod.req.length > 0)
				mod._range.start.character -= "require ".length
			let fileName = AtToUri(mod, filePath, settings, workspaceFolders, res.dasRoot, fixedResults.filesCache)
			if (!fs.existsSync(URI.parse(fileName).fsPath))
				fileName = ''
			const declAt: CompletionAt = {
				_range: Range.create(0, 0, 0, fileName.length > 0 ? 1 : 0),
				_uri: fileName,
				file: '',
				line: 0,
				column: 0,
				lineEnd: 0,
				columnEnd: 0,
				_originalText: '',
			}
			fixedResults.tokens.unshift({
				kind: TokenKind.Require,
				name: mod.req,
				mod: mod.mod,
				_range: mod._range,
				_uri: mod._uri,
				_originalText: doc.getText(mod._range),
				declAt: declAt,
				alias: '',
				value: '',
				tdk: '',
				parentTdk: '',
				isUnused: false,
				isConst: false,
				file: mod.file,
				line: mod.line,
				column: mod.column,
				lineEnd: mod.lineEnd,
				columnEnd: mod.columnEnd,
			})
			allReq.set(mod.mod, { origin: mod, depth: 0 })
			for (const req of mod.dependencies) {
				let sub = allReq.get(req.mod)
				if (sub != null) {
					if (req.depth < sub.depth) {
						sub.depth = req.depth
						sub.origin = mod
					}
				} else {
					allReq.set(req.mod, { origin: mod, depth: req.depth })
				}
			}
		}

		for (const req of usedModules.keys()) {
			const mod = allReq.get(req)
			if (mod != null) {
				mod.origin._used = true
			}
		}

		for (const req of fixedResults.requirements) {
			if (!req._used && !req.isPublic) {
				if (!diagnostics.has(uri))
					diagnostics.set(uri, [])
				const data: DiagnosticsAction = {
					type: DiagnosticsActionType.UnusedReq,
					data: req.req,
				}
				diagnostics.get(uri).push({
					range: req._range,
					message: `unused module ${req.req}`,
					severity: DiagnosticSeverity.Hint,
					tags: [DiagnosticTag.Unnecessary],
					data: data,
				})
			}
		}

		if (uri == globalCompletionFile.uri) {
			// add global completion items
			// src\parser\ds_parser.ypp:143
			const keywords = ["struct", "class", "let", "def", "while", "if", "static_if", "else", "for", "recover", "true", "false", "new",
				"typeinfo", "type", "in", "is", "as", "elif", "static_elif", "array", "return", "null", "break", "try", "options",
				"table", "expect", "const", "require", "operator", "enum", "finally", "delete", "deref", "typedef", "with", "aka", "assume",
				"cast", "override", "abstract", "upcast", "iterator", "var", "addr", "continue", "where", "pass", "reinterpret",
				"module", "public", "label", "goto", "implicit", "explicit", "shared", "private", "smart_ptr", "unsafe", "inscope", "static"]

			for (const kw of keywords) {
				addCompletionItem(completionMap, {
					label: kw,
					kind: CompletionItemKind.Keyword,
					// detail: kw,
					documentation: `keyword ${kw}`,
					sortText: MODULE_SORT,
				})
			}

			const basicTypes = ["bool", "void", "string", "auto", "int", "int2", "int3", "int4", "uint", "bitfield", "uint2",
				"uint3", "uint4", "float", "float2", "float3", "float4", "range", "urange", "range64", "urange64", "block", "int64",
				"uint64", "double", "function", "lambda", "int8", "uint8", "int16", "uint16", "tuple", "variant", "generator", "yield", "sealed"]

			for (const bt of basicTypes) {
				addCompletionItem(completionMap, {
					label: bt,
					kind: CompletionItemKind.Class,
					documentation: `type ${bt}`,
					sortText: MODULE_SORT,
				})
			}

			const metaCalls: Map<string, string[]> = new Map()
			metaCalls.set('assert', ["def assert(cond)", "def assert(cond, string)"])
			metaCalls.set('verify', ["def verify(cond)", "def verify(cond, string)"])
			metaCalls.set('static_assert', ["def static_assert(cond)", "def static_assert(cond, string)"])
			metaCalls.set('concept_assert', ["def concept_assert(cond)", "def concept_assert(cond, string)"])
			metaCalls.set('debug', ["def debug(value : auto(T)) : T", "def debug(value : auto(T), string) : T"])
			metaCalls.set('memzero', ["def memzero(ref)"])

			for (const [name, descriptions] of metaCalls.entries()) {
				for (const desc of descriptions) {
					addCompletionItem(completionMap, {
						label: name,
						kind: CompletionItemKind.Function,
						documentation: desc,
						sortText: MODULE_SORT,
					})
				}
			}

			// src\ast\ast_lint.cpp:759
			const options = ["lint", "only_fast_aot", "aot_order_side_effects", "no_global_heap", "no_global_variables",
				"no_global_variables_at_all", "no_unused_function_arguments", "no_unused_block_arguments", "no_deprecated", "no_aliasing",
				"strict_smart_pointers", "no_init", "no_local_class_members", "report_invisible_functions", "report_private_functions", "stack",
				"intern_strings", "multiple_contexts", "persistent_heap", "heap_size_hint", "heap_size_limit", "string_heap_size_hint",
				"string_heap_size_limit", "gc", "solid_context", "no_aot", "aot_prologue", "log", "log_optimization_passes", "log_optimization",
				"log_stack", "log_init", "log_symbol_use", "log_var_scope", "log_nodes", "log_nodes_aot_hash", "log_mem", "log_debug_mem", "log_cpp",
				"log_aot", "log_infer_passes", "log_require", "log_compile_time", "log_total_compile_time", "log_generics", "log_mn_hash",
				"log_gmn_hash", "log_ad_hash", "log_aliasing", "print_ref", "print_var_access", "print_c_style", "print_func_use", "rtti",
				"optimize", "fusion", "remove_unused_symbols", "always_export_initializer", "infer_time_folding", "disable_run", "max_infer_passes",
				"indenting", "debugger", "profiler", "skip_lock_checks", "skip_module_lock_checks", "threadlock_context"]

			for (const opt of options) {
				addCompletionItem(completionMap, {
					label: `options ${opt}`,
					kind: CompletionItemKind.Keyword,
					documentation: `options ${opt}`,
					sortText: MODULE_SORT,
				})
			}
		}

		if (globalCompletionRes) {
			// very stupid way to merge global completion with local completion
			// but only this way we can keep all the items in the correct order
			for (const it of globalCompletionRes.completionItems) {
				addCompletionItem(completionMap, it)
			}
		}

		for (const [_, items] of completionMap.entries()) {
			fixedResults.completionItems.push(...items.values())
		}
		for (const item of fixedResults.completionItems) {
			if (typeof item.documentation === 'string') {
				item.documentation = {
					kind: 'markdown',
					value: '```dascript\n' + item.documentation + '\n```'
				}
			}
		}
	}

	const prev = validatingResults.get(uri)
	if (prev != null) {
		// collect prev errors and update errors for the same uri-s (to remove transitive errors)
		for (const [errorsUri, _] of prev.diagnostics) {
			if (uri == errorsUri || fixedResults.diagnostics.has(errorsUri))
				continue;

			diagnostics.set(errorsUri, collectDiagnostics(errorsUri, uri));
		}
	}

	validatingResults.set(uri, fixedResults);

	connection.languages.inlayHint.refresh()
}

function collectDiagnostics(uri: string, ignoreUri: string): Array<Diagnostic> {
	let res = new Array<Diagnostic>();
	for (const [errUri, it] of validatingResults) {
		if (ignoreUri == errUri)
			continue;
		let errors = it.diagnostics.get(uri);
		if (errors)
			res.push(...errors);
	}
	return res;
}

connection.listen()
