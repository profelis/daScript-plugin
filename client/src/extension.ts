import * as net from 'net'
import * as path from 'path'
import * as cp from 'child_process'
import {
	workspace as Workspace, window as Window, ExtensionContext, TextDocument, OutputChannel, WorkspaceFolder, Uri, DebugConfigurationProvider, DebugConfiguration, ProviderResult, CancellationToken
} from 'vscode'

import * as vscode from 'vscode'

import {
	LanguageClient, LanguageClientOptions, StreamInfo
} from 'vscode-languageclient/node'

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
			const settings = Workspace.getConfiguration()
			const timeout = settings.get<number>("dascript.server.connectTimeout", 2)
			const waitTime = Date.now()
			while (Date.now() - waitTime < timeout * 1000) {
				// log("waiting child... " + timeout)
			}

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
			child.on('error', (err) => {
				log(`Failed to spawn server ${err.message}`)
			})
		}

		const socket = net.connect({ port: port }, () => {
			socket.setNoDelay()
			log(`> ${port} connected - '${folder_uri}'`)
			childProcesses.set(folder_uri, child)
			sockets.set(folder_uri, socket)
			resolve([child, socket])
		})

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

	const provider = new DascriptLaunchConfigurationProvider()
	context.subscriptions.push(vscode.debug.registerDebugConfigurationProvider('dascript', provider))
	context.subscriptions.push(vscode.debug.registerDebugAdapterDescriptorFactory('dascript', new DascriptLaunchDebugAdapterFactory()))
}

export function deactivate(): Thenable<void> {
	const promises: Thenable<void>[] = []
	if (defaultClient)
		promises.push(defaultClient.stop())
	for (const client of clients.values())
		promises.push(client.stop())
	return Promise.all(promises).then(() => undefined)
}


class DascriptLaunchConfigurationProvider implements DebugConfigurationProvider {

	resolveDebugConfiguration(folder: WorkspaceFolder | undefined, config: DebugConfiguration, token?: CancellationToken): ProviderResult<DebugConfiguration> {

		// if launch.json is missing or empty
		if (!config.type && !config.request && !config.name) {
			const editor = vscode.window.activeTextEditor
			if (editor?.document?.languageId === 'dascript') {
				config.type = 'dascript'
				config.name = 'dascript'
				config.request = 'launch'
				config.program = '${config:dascript.compiler} ${file}'
			}
		}

		// if (!config.program) {
		// 	return vscode.window.showInformationMessage("Cannot find a program to debug").then(_ => {
		// 		return undefined;	// abort launch
		// 	});
		// }

		return config
	}
}

class DascriptLaunchDebugAdapterFactory implements vscode.DebugAdapterDescriptorFactory {

	child: cp.ChildProcess
	outputChannel: OutputChannel
	createDebugAdapterDescriptor(_session: vscode.DebugSession): ProviderResult<vscode.DebugAdapterDescriptor> {

		const port = "port" in _session.configuration ? _session.configuration.port : 9000
		if (_session.configuration.request != "launch")
			return new vscode.DebugAdapterServer(port)

		if (this.outputChannel)
			this.outputChannel.dispose() // always recreate output
		const outputChannel: OutputChannel = Window.createOutputChannel("daScript debug output")
		outputChannel.show(true)
		this.outputChannel = outputChannel

		const log = function (data: string) {
			console.log(data)
			outputChannel.append(data)
		}

		if (this.child) {
			console.log(`da kill prev child`)
			this.child.kill()
		}
		const cmdAndArgs: string[] = _session.configuration.program.split(" ")
		const cmd = cmdAndArgs.shift()
		const extraArgs = ["--das-debug-port", `${port}`]
		const args = cmdAndArgs.concat(cmdAndArgs.indexOf("--") >= 0 ? extraArgs : ["--", ...extraArgs])
		const cwd = _session.configuration.cwd || _session.workspaceFolder.uri.fsPath
		const externalConsole = _session.configuration.console == "externalTerminal"

		console.log(`> spawn da server ${cmd} ${args.join(' ')} cwd: ${cwd}`)
		this.child = cp.spawn(cmd, args, { cwd: cwd, detached: externalConsole, shell: externalConsole })

		if (this.child) {
			// this.child.on('spawn', () => {
			// 	console.log(`da spawned`)
			// })
			this.child.on('error', (err) => {
				log(`da: server error ${err.message}`)
				this.child.kill()
				this.child = null
			})
			this.child.on('close', (code) => {
				log(`\nda: child process exited with code ${code}`)
				this.child = null
			})
			this.child.stdout.on('data', (data) => {
				log(`${data}`)
			})
			this.child.stderr.on('data', (data) => {
				log(`[stderr] ${data}`)
			})
		}

		if (this.child) {
			const connectTimeout = "connectTimeout" in _session.configuration ? _session.configuration.connectTimeout : 1
			const waitTime = Date.now()
			while (Date.now() - waitTime < connectTimeout * 1000) {
				// log("waiting child...")
			}
		}

		return new vscode.DebugAdapterServer(port)
	}
}
