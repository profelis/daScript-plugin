// @ts-check
import { Range } from 'vscode-languageserver'
import { fixPath, DascriptSettings } from './server'
import { isRangeZero, rangeToString, fixRange } from './lspUtil'

export interface CursorData {
	path: string
	range: Range
	func?: FunctionData
	call?: CallData
	variable?: VariableData
}

export interface FuncData {
	path: string
	range: Range
	name: string
}

export interface FunctionData extends FuncData {
	generic?: FuncData
}

export interface CallData {
	path: string
	range: Range
	name: string
	func?: FunctionData
}

export interface VariableData {
	path: string
	range: Range
	name: string
	type?: string
	category?: string
	index?: number
}

export namespace VariableCategory {
	export const LOCAL = "local"
	export const GLOBAL = "global"
	export const BLOCK_ARGUMENT = "block argument"
	export const FUNCTION_ARGUMENT = "function argument"
	export const STRUCTURE_FIELD = "structure field"
}

export function parseCursor(data: any, settings: DascriptSettings): CursorData | null {
	if (!data)
		return null
	const cursor = data["cursor"] ?? {}
	const path = fixPath(cursor["uri"] || "", settings)
	return {
		path: path,
		range: fixRange(cursor["range"]),
		func: parseFunctionData(data["function"], settings),
		call: parseCallData(data["call"], settings),
		variable: parseVariableData(data["variable"], settings),
	}
}

export function funcToString(func: FuncData, verbose = false): string {
	let res = `def ${func.name}`
	if (!verbose)
		return res
	if (func.path.length > 0)
		res += `\n// ${func.path}`
	if (!isRangeZero(func.range))
		res += (func.path.length == 0 ? "\n// " : " ") + rangeToString(func.range)
	return res
}

export function callToString(call: CallData, verbose = false): string {
	let res = `${call.name} // call`
	if (!verbose)
		return res
	if (call.path.length > 0)
		res += `\n// ${call.path}`
	if (!isRangeZero(call.range))
		res += (call.path.length == 0 ? "\n// " : " ") + rangeToString(call.range)
	return res
}

export function variableToString(variable: VariableData, verbose = false): string {
	let res = `${variable.name}`
	if (variable.type)
		res += `: ${variable.type}`
	if (variable.category)
		res += ` // ${variable.category}`
	else
		res += ` // variable`
	if (!verbose)
		return res
	if (variable.index)
		res += ` @${variable.index}`
	if (variable.path.length > 0)
		res += `\n// ${variable.path}`
	if (!isRangeZero(variable.range))
		res += (variable.path.length == 0 ? "\n// " : " ") + rangeToString(variable.range)
	return res
}

function parseFunctionData(data: any, settings: DascriptSettings): FunctionData | null {
	if (!data)
		return null
	const funcData = parseFuncData(data, settings)
	const genericData = parseFuncData(data["generic"], settings)
	if (funcData) {
		if (genericData)
			return { generic: genericData, name: funcData.name, path: funcData.path, range: funcData.range }
		return funcData
	}
	if (genericData)
		return { generic: genericData, name: genericData.name, path: genericData.path, range: genericData.range }
	return null
}

function parseFuncData(data: any, settings: DascriptSettings): FuncData | null {
	if (!data)
		return null
	const name = data["name"] ?? ""
	const localPath = fixPath(data["uri"] || "", settings)
	const range = fixRange(data["range"])
	return { name: name, path: localPath, range: range }
}

function parseCallData(data: any, settings: DascriptSettings): CallData | null {
	if (!data)
		return null
	const name = data["name"] ?? ""
	const localPath = fixPath(data["uri"] || "", settings)
	const range = fixRange(data["range"])
	const call = parseFuncData(data["function"], settings)
	return { name: name, path: localPath, range: range, func: call }
}

function parseVariableData(data: any, settings: DascriptSettings): VariableData | null {
	if (!data)
		return null
	const name = data["name"] ?? ""
	const localPath = fixPath(data["uri"] || "", settings)
	const range = fixRange(data["range"])
	return { name: name, path: localPath, range: range, type: data["type"], category: data["category"], index: data["index"] }
}
