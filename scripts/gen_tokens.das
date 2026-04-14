options gen2
options indenting = 4
require daslib/fio
require daslib/strings
require daslib/strings_boost

variant Tokens {
    status : bool
    tokens : array<string>
}

[export]
def main {
    let cbs = [@@getKeywords, @@getBasicTypes, @@getOperators]
    let prefixes = [ "const keywords = [", "const basicTypes = [", "const OPERATORS = ["]
    for (cb, prefix in cbs, prefixes) {
        let data = cb()
        if (data is status) {
            error("get tokens status: {data}")
            continue
        }
        let res = buildString(data as tokens, prefix, "]", "'", ", ") // das
        // let res = buildString(data as tokens, "", "", "", " | ", 60) // tm syntax
        print(res)
    }
}

def buildString(data : array<string>; prefix, postfix, decorator, delimeter : string; maxLen = 120) {
    if (length(data) == 0) {
        return prefix + postfix
    } else {
        let padStr = repeat(" ", length(prefix))
        return build_string() $(var writer) {
            var lineLen = length(prefix)
            var isFirst = true
            write(writer, prefix)
            for (i in iter_range(data)) {
                let it = data[i]
                if (lineLen + length(delimeter) + 2 * length(decorator) >= maxLen) {
                    if (!isFirst) {
                        write(writer, "\n")
                    }
                    isFirst = false
                    write(writer, padStr)
                    lineLen = length(padStr)
                }
                write(writer, decorator)
                write(writer, it)
                write(writer, decorator)
                lineLen += 2 * length(decorator) + length(it)
                if (i < length(data) - 1) {
                    write(writer, delimeter)
                    lineLen += length(delimeter)
                }
            }
            write(writer, postfix)
            write(writer, "\n")
        }
    }
}

def getKeywords() {
    return <- getTokens("/* keywords */")
}

def getBasicTypes() {
    return <- getTokens("/* basic types */")
}

def getOperators() {
    return <- getTokens("DAS_OPERATOR '!'")
}

def getTokens(from : string) : Tokens {
    let path = get_das_root() + "/src/parser/ds_parser.ypp"
    var res <- Tokens(status = false)
    fio::fopen(path, "rb") $(f) {
        if (f == null) {
            debug("error: can't open {path}")
            return
        }
        let data = fio::fread(f)
        let start = find(data, from) + length(from) + 2
        if (start == -1) {
            debug("error: can't find bounds")
            return
        }
        let sub = slice(data, start, length(data) - 1)
        let tokens <- split(sub, "\r\n\r\n")[0] |> replace("\n\r", "\n") |> split("\n") // workaround split
        var list : array<string>
        for (it in tokens) {
            if (it |> strip() |> length() == 0 || find(it, "\"") == -1) {
                break
            }
            let b = find(it, "\"") + 1
            list |> push(slice(it, b, find(it, "\"", b)))
        }
        res <- Tokens(tokens <- list)
    }
    return <- res
}
