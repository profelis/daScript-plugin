import { integer } from 'vscode-languageserver/node'

export interface DasSettings {
	compiler: string;
	server: {
		args: string[];
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
		no_global_variables?: boolean;
		no_unused_block_arguments?: boolean;
		no_unused_function_arguments?: boolean;
		fail_on_lack_of_aot_export?: boolean;
		version_2_syntax?: boolean;
		gen2_make_syntax?: boolean;
		always_report_candidates_threshold?: integer;
	};
	debug?: {
		port: integer;
	};
	experimental?: boolean;
	colorPreviewFormat?: string; // ARGB (default) or RGBA
	validationConcurrency?: integer; // Max number of concurrent validation processes (default 10)
}

export const defaultSettings: DasSettings = {
	compiler: "daScript", server: { args: ["${file}"] },
}

// Cache the settings of all open documents
export const documentSettings: Map<string, Thenable<DasSettings>> = new Map()
