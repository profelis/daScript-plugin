options indenting = 4
options no_aot = true

require strings
require daslib/strings_boost

def strip_left(str : string; part : string)
    return starts_with(str, part) ? slice(str, length(part)) : str

def get_int_arg(args:array<string>; name : string; def_val : int) : int
    let idx = find_index(args, name)
    if idx >= 0 && idx + 1 < length(args)
        return int(args[idx + 1])
    return def_val

def get_str_arg(args:array<string>; name : string; def_val : string) : string
    let idx = find_index(args, name)
    if idx >= 0 && idx + 1 < length(args)
        return args[idx + 1]
    return def_val

def bytes_hr(value : uint64)
    if value > uint64(1024 * 1024)
        return "{float(value) / (1024f * 1024f)}mb ({int(value)})"
    if value > uint64(1024)
        return "{float(value) / 1024f}kb ({int(value)})"
    return "{int(value)} b"
