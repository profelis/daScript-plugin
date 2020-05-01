// @ts-check
import { Position, Range } from "vscode-languageserver"

export function uriToFile(uri: string): string {
	uri = decodeURIComponent(uri);
	if (!uri.startsWith("file://"))
		return uri;
	uri = uri.substr(7); // "file://".length
	if (/\/\w\:/.test(uri.substr(0, 3)))
		return uri.substr(1);
	return uri;
}

export function rangeToString(range: Range): string {
	return `${range.start.line}:${range.start.character}-${range.end.line}:${range.end.character}`
}

export function isRangeZero(range: Range): boolean {
	return range.start.character == 0 && range.start.line == 0 && range.end.character == 0 && range.end.line == 0
}

export function isRangeEmpty(range: Range): boolean {
	return range.start.line == range.end.line && range.start.character == range.end.character
}

export function fixPosition(pos: Position | null, lineOffset = -1, endOffset = 0): Position {
	pos = pos ?? Position.create(0, 0)
	pos.line = Math.max(0, (pos?.line ?? 0) + lineOffset)
	pos.character = Math.max(0, (pos?.character ?? 0) + endOffset)
	return pos
}

export function fixRange(range: Range | null, lineOffset = -1, endOffset = 0): Range {
	range = range ?? Range.create(0, 0, 0, 0)
	range.start = fixPosition(range.start, lineOffset)
	range.end = fixPosition(range.end, lineOffset, endOffset)
	range.end.line = Math.max(range.start.line, range.end.line - 1)
	if (range.start.line == range.end.line)
		range.end.character = Math.max(range.start.character, range.end.character)
	return range
}
