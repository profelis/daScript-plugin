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
	const LOCAL = "local"
	const GLOBAL = "global"
	const BLOCK_ARGUMENT = "block argument"
	const FUNCTION_ARGUMENT = "function argument"
	const STRUCTURE_FIELD = "structure field"
}

export function parseCursor(data: any, path: string, settings: DascriptSettings): CursorData | null {
	if (!data)
		return null
	const cursor = data["cursor"] ?? {}
	path = fixPath(cursor["uri"] || path, settings)
	return {
		path: path,
		range: fixRange(cursor["range"]),
		func: parseFunctionData(data["function"], path, settings),
		call: parseCallData(data["call"], path, settings),
		variable: parseVariableData(data["variable"], path, settings),
	}
}

export function funcToString(func: FuncData): string {
	let res = `def ${func.name}`
	if (func.path.length > 0)
		res += `\n// ${func.path}`
	if (!isRangeZero(func.range))
		res += `\n// ${rangeToString(func.range)}`
	return res
}

export function callToString(call: CallData): string {
	let res = `${call.name} // call`
	if (call.path.length > 0)
		res += `\n// ${call.path}`
	if (!isRangeZero(call.range))
		res += `\n// ${rangeToString(call.range)}`
	return res
}

export function variableToString(variable: VariableData): string {
	let res = `${variable.name}`
	if (variable.type)
		res += `: ${variable.type}`
	if (variable.category)
		res += ` // ${variable.category}`
	if (variable.index)
		res += ` @${variable.index}`
	if (variable.path.length > 0)
		res += `\n// ${variable.path}`
	if (!isRangeZero(variable.range))
		res += `\n// ${rangeToString(variable.range)}`
	return res
}

function parseFunctionData(data: any, path: string, settings: DascriptSettings): FunctionData | null {
	if (!data)
		return null
	const funcData = parseFuncData(data, path, settings)
	const genericData = parseFuncData(data["generic"], path, settings)
	if (funcData) {
		if (genericData)
			return { generic: genericData, name: funcData.name, path: funcData.path, range: funcData.range }
		return funcData
	}
	if (genericData)
		return { generic: genericData, name: genericData.name, path: genericData.path, range: genericData.range }
	return null
}

function parseFuncData(data: any, path: string, settings: DascriptSettings): FuncData | null {
	if (!data)
		return null
	const name = data["name"] ?? ""
	const localPath = fixPath(data["uri"] || path, settings)
	const range = fixRange(data["range"])
	return { name: name, path: localPath, range: range }
}

function parseCallData(data: any, path: string, settings: DascriptSettings): CallData | null {
	if (!data)
		return null
	const name = data["name"] ?? ""
	const localPath = fixPath(data["uri"] || path, settings)
	const range = fixRange(data["range"])
	const call = parseFuncData(data["function"], path, settings)
	return { name: name, path: localPath, range: range, func: call }
}

function parseVariableData(data: any, path: string, settings: DascriptSettings): VariableData | null {
	if (!data)
		return null
	const name = data["name"] ?? ""
	const localPath = fixPath(data["uri"] || path, settings)
	const range = fixRange(data["range"])
	return { name: name, path: localPath, range: range, type: data["type"], category: data["category"], index: data["index"] }
}
