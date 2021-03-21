require fio
require strings
require daslib/strings_boost

variant Tokens
    status : bool
    tokens : array<string>

[export]
def main
    // let data <- getKeywords()
    let data <- getOperators()
    // let data <- getBasicTypes()
    if data is status
        debug("get tokens status: {data}")
        return
    let res = buildString(data as tokens, "[[string ", "]]", "\"", "; ") // das
    // let res = buildString(data as tokens, "", "", "", " | ", 60) // tm syntax
    print(res)

def buildString(data : array<string>; prefix, postfix, decorator, delimeter : string; maxLen = 120)
    if length(data) == 0
        return prefix + postfix
    else
        var res = ""
        var line = prefix
        for i in iter_range(data)
            let it = data[i]
            if length(line) + length(delimeter) + 2 * length(decorator) >= maxLen
                res += (length(res) > 0 ? "\n" : "") + line
                line = repeat(" ", prefix |> length())
            line += "{decorator}{it}{decorator}"
            if i < length(data) - 1
                line += delimeter
        if length(line) > 0
            res += "\n{line}"
        res += postfix
        return res

def getKeywords()
    return <- getTokens("/* keywords */")

def getBasicTypes()
    return <- getTokens("/* basic types */")

def getOperators()
    return <- getTokens("DAS_OPERATOR '!'")

def getTokens(from : string) : Tokens
    let path = get_das_root() + "/src/parser/ds_parser.ypp"
    var res <- [[Tokens status = false]]
    fio::fopen(path, "rb") <| $(f)
        if f == null
            debug("error: can't open {path}")
            return
        let data = fio::fread(f)
        let start = find(data, from) + length(from) + 2
        if start == -1
            debug("error: can't find bounds")
            return
        let sub = slice(data, start, length(data) - 1)
        let tokens <- split(split(sub, "\r\n\r\n")[0], "\r\n") // workaround split
        var list : array<string>
        for it in tokens
            if it |> strip() |> length() == 0 || find(it, "\"") == -1
                break
            let b = find(it, "\"") + 1
            list |> emplace(slice(it, b, find(it, "\"", b)))
        res <- [[Tokens tokens <- list]]
    return <- res
