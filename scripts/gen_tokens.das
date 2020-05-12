require fio

let
	dasRoot = ""

[init]
def init
	// dumpTokens()
	dumpTypes()

def dumpTokens
	dump("/* keywords */", "/* basic types */")

def dumpTypes
	dump("/* basic types */", "/* operators */")

def dump(from, to : string)
	let path = dasRoot + "src/parser/ds_parser.ypp"
	let f = fio::fopen(path, "rb")
	if f == null
		debug("error: can't open {path}")
		return false
	let data = fio::fread(f)
	let start = find(data, from) + length(from)
	let end = find(data, to)
	if start == -1 | end == -1
		debug("error: can't find bounds")
		return false
	let sub = slice(data, start, end)
	let tokens <- split(sub, "\r\n")
	for it in tokens
		if it |> strip() |> length() == 0
			continue
		let b = find(it, "\"") + 1
		print(slice(it, b, find(it, "\"", b)) + "\n")
	return true
finally
	if f != null
		fclose(f)
