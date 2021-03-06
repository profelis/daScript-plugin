import * as net from 'net'
import * as path from 'path'
import * as cp from 'child_process'
import {
	workspace as Workspace, window as Window, ExtensionContext, TextDocument, OutputChannel, WorkspaceFolder, Uri
} from 'vscode'

import {
	LanguageClient, LanguageClientOptions, StreamInfo
} from 'vscode-languageclient'

let SPAWN_SERVER = true
let DEFAULT_PORT = 7999 + Math.round(Math.random() * 3000)
let defaultClient: LanguageClient
const clients: Map<string, LanguageClient> = new Map()
const sockets: Map<string, net.Socket> = new Map()
const childProcesses: Map<string, cp.ChildProcess> = new Map()

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

function createServerWithSocket(folder_uri: string, port: number, cmd: string, args: string[], cwd: string, out: OutputChannel) {
	return new Promise<[cp.ChildProcess, net.Socket]>(resolve => {
		const log = function (data: string) {
			console.log(data)
			if (!sockets.has(folder_uri))
				out.appendLine(data)
		}
		log(`> spawn server ${cmd} ${args.join(' ')} - '${folder_uri}' cwd: ${cwd}`)
		const child: cp.ChildProcess = SPAWN_SERVER ? cp.spawn(cmd, args, { cwd: cwd }) : null

		if (child) {
			const waitTime = Date.now()
			while (!child.connected && Date.now() - waitTime < 4000) {
				// log("waiting child...")
			}
		}

		if (child)
			child.on('error', (err) => {
				log(`Failed to spawn server ${err.message}`)
			})

		const socket = net.connect({ port: port }, () => {
			socket.setNoDelay()
			log(`> ${port} connected - '${folder_uri}'`)
			childProcesses.set(folder_uri, child)
			sockets.set(folder_uri, socket)
			resolve([child, socket])
		})

		if (child) {
			child.stdout.on('data', (data) => {
				log(`stdout: ${data}`)
			})

			child.stderr.on('data', (data) => {
				log(`stderr: ${data}`)
			})
			child.on('close', (code) => {
				log(`child process exited with code ${code} - '${folder_uri}'`)
				childProcesses.delete(folder_uri)
				if (sockets.has(folder_uri))
					sockets.delete(folder_uri)
				else
					resolve([child, socket])
			})
		}

		// socket.on('data', (data) => {
		// 	const msg = data.toString()
		// 	console.log(msg.length > 1000 ? msg.substr(0, 1000) + "..." : msg)
		// })
		socket.on('error', (err) => {
			console.log(`socket error: ${err.message}`)
			if (err.stack != null)
				console.log(err.stack ?? "")
		})
		socket.on('end', () => {
			console.log(`socked closed - '${folder_uri}'`)
			if (child && !child.killed)
				child.kill()
			childProcesses.delete(folder_uri)
			sockets.delete(folder_uri)
		})
	})
}

function setArg(args: string[], pattern: string, value: string): string[] {
	const res = new Array<string>()
	for (const it of args) {
		res.push(it.replace(pattern, value))
	}
	return res
}

export function activate(context: ExtensionContext) {

	const settings = Workspace.getConfiguration()

	const cmd = settings.get<string>("dascript.compiler")
	let args = settings.get<string[]>("dascript.server.args")
	const port: number = settings.get("dascript.debug.port", -1)
	if (port != 0) {
		SPAWN_SERVER = port > 0
		DEFAULT_PORT = Math.abs(port)
	}
	const cwd = context.asAbsolutePath(path.join('server', 'das'))
	const serverFilePath = context.asAbsolutePath(path.join('server', 'das', 'server.das'))
	args = setArg(args, "${file}", serverFilePath)
	const outputChannel: OutputChannel = Window.createOutputChannel('daScript')

	function didOpenTextDocument(document: TextDocument): void {
		if (document.languageId !== 'dascript' || (document.uri.scheme !== 'file' && document.uri.scheme !== 'untitled')) {
			return
		}

		const uri = document.uri
		if (uri.scheme === 'untitled' && !defaultClient) {
			const serverOptions = async () => {
				const port = DEFAULT_PORT
				const [_, socket] = await createServerWithSocket("____untitled____", port, cmd, setArg(args, "${port}", port.toPrecision()), cwd, outputChannel)
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

		const folderUri = folder.uri.toString()
		if (!clients.has(folderUri)) {
			const serverOptions = async () => {
				const port = DEFAULT_PORT + 1 + clients.size
				const [_, socket] = await createServerWithSocket(folderUri, port, cmd, setArg(args, "${port}", port.toPrecision()), cwd, outputChannel)
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
			const uri = folder.uri.toString()
			const client = clients.get(uri)
			if (client) {
				clients.delete(uri)
				client.stop()
			}
			const child = childProcesses.get(uri)
			if (child) {
				childProcesses.delete(uri)
				child.kill()
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