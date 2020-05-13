require fio

let
	dasRoot = ""

typedef
	Tokens = variant< status:bool; tokens:array<string> >

[init]
def init
	// let data <- getKeywords()
	let data <- getBasicTypes()
	if data is status
		debug("get tokens status: {data}")
		return
	let res = buildString(data as tokens, "[[string ", "]]", "\"", "; ")
	// let res = buildString(data as tokens, "", "", "", " | ", 80)
	print(res)

def buildString(data : array<string>; prefix, postfix, decorator, delimeter : string; maxLen = 120)
	if length(data) == 0
		return prefix + postfix
	else
		var res = ""
		var line = prefix
		for i in range(0, length(data))
			let it = data[i]
			if length(line) + length(delimeter) + 2 * length(decorator) >= maxLen
				res += "\n{line}"
				line = repeat(" ", prefix |> length())
			line += "{decorator}{it}{decorator}"
			if i < length(data) - 1
				line += delimeter
		if length(line) > 0
			res += "\n{line}"
		res += postfix
		return res

def getKeywords()
	return <- getTokens("/* keywords */", "/* basic types */")

def getBasicTypes()
	return <- getTokens("/* basic types */", "/* operators */")

def getTokens(from, to : string) : Tokens
	let path = dasRoot + "src/parser/ds_parser.ypp"
	var res : array<string>
	let f = fio::fopen(path, "rb")
	if f == null
		debug("error: can't open {path}")
		return <- [[Tokens status = false]]
	let data = fio::fread(f)
	let start = find(data, from) + length(from)
	let end = find(data, to)
	if start == -1 | end == -1
		debug("error: can't find bounds")
		return <- [[Tokens status = false]]
	let sub = slice(data, start, end)
	let tokens <- split(sub, "\r\n")
	for it in tokens
		if it |> strip() |> length() == 0
			continue
		let b = find(it, "\"") + 1
		res |> push(slice(it, b, find(it, "\"", b)))
	return <- [[Tokens tokens <- res]]
finally
	if f != null
		fclose(f)
