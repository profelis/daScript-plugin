import { Disposable, ExtensionContext, WorkspaceFolder, WorkspaceFolderPickOptions, commands, window } from 'vscode'
import { ExecuteCommandRequest, LanguageClient, RequestType } from 'vscode-languageclient/node'


export function registerClientCommands(context: ExtensionContext, client: LanguageClient): void {
    context.subscriptions.push(
        ...registerValidateCommand(client)
    )
}

async function validateWorkspaceCommand(client: LanguageClient): Promise<void> {
    const workspaceFolder: WorkspaceFolder = await window.showWorkspaceFolderPick({
        placeHolder: 'Select folder',
    })

    if (!workspaceFolder) {
        return
    }

    await client.sendRequest(
        new RequestType(ExecuteCommandRequest.method),
        { command: 'validateWorkspace', args: { folder: workspaceFolder.uri } }
    )
}

function registerValidateCommand(client: LanguageClient): Disposable[] {
    return [
        commands.registerCommand(
            'dascript.validateWorkspace',
            async () => await validateWorkspaceCommand(client)
        ),
        commands.registerCommand(
            'dascript.clearValidationCache',
            async () => await client.sendRequest(
                new RequestType(ExecuteCommandRequest.method),
                { command: 'clearValidationCache' }
            )
        )
    ]
}
