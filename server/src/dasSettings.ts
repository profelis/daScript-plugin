import { integer } from 'vscode-languageserver/node';

export interface DasSettings {
	compiler: string;
	server: {
		args: string[];
		unity?: boolean;
	};
	project?: {
		file?: string;
		scanWorkspace?: boolean;
		roots?: string[];
		fileAccessRoots?: { [key: string]: string; };
	};
	hovers?: {
		verbose?: boolean;
	};
	policies?: {
		ignore_shared_modules?: boolean;
		no_global_variables?: boolean;
		no_unused_block_arguments?: boolean;
		no_unused_function_arguments?: boolean;
		fail_on_lack_of_aot_export?: boolean;
	};
	debug?: {
		port: integer;
	};
	experimental?: boolean;
	colorPreviewFormat?: string; // ARGB (default) or RGBA
}

export const defaultSettings: DasSettings = {
	compiler: "daScript", server: { args: ["${file}"] },
	policies: { ignore_shared_modules: true, }
}

// Cache the settings of all open documents
export const documentSettings: Map<string, Thenable<DasSettings>> = new Map()
