import { Range } from 'vscode-languageserver'
import { fixPath, DascriptSettings } from './server'
import { isRangeZero, rangeToString, fixRange } from './lspUtil'

export interface CursorData {
	cursor: Cursor
	function: FunctionData | null
	functions?: FunctionData[]
	call: CallData | null
	calls?: CallData[]
	variable: VariableData | null
	variables?: VariableData[]
}

export interface Cursor {
	uri: string
	range: Range
	tab: number
}
export interface FuncData {
	uri: string
	range: Range
	name: string
}

export interface FunctionData extends FuncData {
	generic: FuncData | null
}

export interface CallData {
	uri: string
	range: Range
	name: string
	function: FunctionData | null
}

export interface VariableData {
	uri: string
	range: Range
	name: string
	type?: string
	category?: string
	index?: number
	function?: FunctionData
}

export namespace VariableCategory {
	export const LOCAL = "local"
	export const GLOBAL = "global"
	export const BLOCK_ARGUMENT = "block argument"
	export const FUNCTION_ARGUMENT = "function argument"
	export const STRUCTURE_FIELD = "structure field"
	export const UNINFERED_FIELD_LOOKUP = "uninfered field lookup"
	export const LAMBDA_OR_GENERATOR_CAPTURE = "lambda or generator capture"
	export const ANNOTATION_FIELD_OR_PROPERTY = "annotation field or property"
	export const TUPLE_FIELD = "tuple field"
	export const VARIANT_FIELD = "variant field"
	export const VECTOR_FIELD_LOOKUP_OR_SWIZZLE = "vector field lookup or swizzle"
	export const UNKNOWN_FIELD_LOOKUP = "unknown field lookup"
}

export function functionToString(func: FuncData, settings: DascriptSettings, verbose = false): string {
	let res = `def ${func.name}`
	if (!verbose)
		return res
	if ((func.uri?.length ?? 0) > 0)
		res += `\n// ${fixPath(func.uri, settings)}`
	if (func.range && !isRangeZero(func.range))
		res += ((func.uri?.length ?? 0) == 0 ? "\n// " : " ") + rangeToString(func.range)
	return res
}

export function callToString(call: CallData, settings: DascriptSettings, verbose = false): string {
	let res = call.name
	if (!verbose)
		return res
	if ((call.uri?.length ?? 0) > 0)
		res += `\n// ${fixPath(call.uri, settings)}`
	if (call.range && !isRangeZero(call.range))
		res += (call.uri.length == 0 ? "\n// " : " ") + rangeToString(call.range)
	return res
}

export function variableToString(variable: VariableData, settings: DascriptSettings, verbose = false, showCall = false): string {
	let res = `${variable.name}`
	if (variable.type)
		res += `: ${variable.type}`
	if (variable.category)
		res += ` // ${variable.category}`
	else
		res += ` // variable`
	if (verbose) {
		if (variable.index)
			res += ` @${variable.index}`
		if ((variable.uri?.length ?? 0) > 0)
			res += `\n// ${fixPath(variable.uri, settings)}`
		if (variable.range && !isRangeZero(variable.range))
			res += ((variable.uri?.length ?? 0) == 0 ? "\n// " : " ") + rangeToString(variable.range)
	}
	if (showCall && variable.function)
		res += `\n${variable.function.name} // call`
	return res
}
