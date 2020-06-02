import * as net from 'net'
import * as path from 'path'
import {
	workspace as Workspace, window as Window, ExtensionContext, TextDocument, OutputChannel, WorkspaceFolder, Uri
} from 'vscode'

import {
	LanguageClient, LanguageClientOptions, TransportKind, ServerOptions, Executable, StreamInfo
} from 'vscode-languageclient'

let defaultClient: LanguageClient
const clients: Map<string, LanguageClient> = new Map()

let _sortedWorkspaceFolders: string[] | undefined
function sortedWorkspaceFolders(): string[] {
	if (_sortedWorkspaceFolders === void 0) {
		_sortedWorkspaceFolders = Workspace.workspaceFolders
			? Workspace.workspaceFolders.map(folder => {
				let result = folder.uri.toString()
				if (result.charAt(result.length - 1) !== '/') {
					result = result + '/'
				}
				return result
			}).sort((a, b) => { return a.length - b.length })
			: []
	}
	return _sortedWorkspaceFolders
}
Workspace.onDidChangeWorkspaceFolders(() => _sortedWorkspaceFolders = undefined)

function getOuterMostWorkspaceFolder(folder: WorkspaceFolder): WorkspaceFolder {
	const sorted = sortedWorkspaceFolders()
	for (const element of sorted) {
		let uri = folder.uri.toString()
		if (uri.charAt(uri.length - 1) !== '/')
			uri = uri + '/'
		if (uri.startsWith(element))
			return Workspace.getWorkspaceFolder(Uri.parse(element))!
	}
	return folder
}

export function activate(context: ExtensionContext) {

	const cmd = "d:\\dev\\cpp\\daScript\\cmake_temp\\RelWithDebInfo\\daScript.exe" //context.asAbsolutePath(path.join('server', 'out', 'server.js'))
	const outputChannel: OutputChannel = Window.createOutputChannel('daScript')

	function didOpenTextDocument(document: TextDocument): void {
		if (document.languageId !== 'dascript' || (document.uri.scheme !== 'file' && document.uri.scheme !== 'untitled')) {
			return
		}

		const uri = document.uri
		if (uri.scheme === 'untitled' && !defaultClient) {
			const debugOptions = { execArgv: ["--nolazy", `--inspect=${6011 + clients.size}`] }
			const args = ["D:\\dev\\vscode\\daScriptLangPlugin\\server\\das\\server.das"]
			const connectionInfo = {
				port: 9000
			}
			const serverOptions = () => {
				// Connect to language server via socket
				const socket = net.connect(connectionInfo)
				const result: StreamInfo = {
					writer: socket,
					reader: socket
				}
				return Promise.resolve(result)
			}
			const clientOptions: LanguageClientOptions = {
				documentSelector: [{ scheme: 'untitled', language: 'dascript' }],
				diagnosticCollectionName: 'dascript',
				outputChannel: outputChannel
			}
			defaultClient = new LanguageClient('dascript', serverOptions, clientOptions)
			defaultClient.start()
			return
		}
		let folder = Workspace.getWorkspaceFolder(uri)
		if (!folder)
			return
		folder = getOuterMostWorkspaceFolder(folder)

		if (!clients.has(folder.uri.toString())) {
			const debugOptions = { execArgv: ["--nolazy", `--inspect=${6011 + clients.size}`] }
			const args = ["D:\\dev\\vscode\\daScriptLangPlugin\\server\\das\\server.das"]
			const connectionInfo = {
				port: 9000
			}
			const serverOptions = () => {
				// Connect to language server via socket
				const socket = net.connect(connectionInfo)
				const result: StreamInfo = {
					writer: socket,
					reader: socket
				}
				return Promise.resolve(result)
			}
			const clientOptions: LanguageClientOptions = {
				documentSelector: [
					{ scheme: 'file', language: 'dascript', pattern: `${folder.uri.fsPath}/**/*` }
				],
				diagnosticCollectionName: 'dascript',
				workspaceFolder: folder,
				outputChannel: outputChannel
			}
			const client = new LanguageClient('dascript', serverOptions, clientOptions)
			client.start()
			clients.set(folder.uri.toString(), client)
		}
	}

	Workspace.onDidOpenTextDocument(didOpenTextDocument)
	Workspace.textDocuments.forEach(didOpenTextDocument)
	Workspace.onDidChangeWorkspaceFolders((event) => {
		for (const folder of event.removed) {
			const client = clients.get(folder.uri.toString())
			if (client) {
				clients.delete(folder.uri.toString())
				client.stop()
			}
		}
	})
}

export function deactivate(): Thenable<void> {
	const promises: Thenable<void>[] = []
	if (defaultClient)
		promises.push(defaultClient.stop())
	for (const client of clients.values())
		promises.push(client.stop())
	return Promise.all(promises).then(() => undefined)
}