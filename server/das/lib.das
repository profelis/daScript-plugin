options indenting = 4
options no_aot = true

require strings
require daslib/strings_boost

[export]
def move ( var a )
    return <- a

[generic]
def strip_left(str : string; part : string)
    return starts_with(str, part) ? slice(str, length(part)) : str

def get_int_arg(args:array<string>; name : string; def_val : int) : int
    for i in range(0, length(args))
        if args[i] == name
            if i == length(args) - 1
                return def_val
            return int(args[i+1])
    return def_val

def get_str_arg(args:array<string>; name : string; def_val : string) : string
    for i in range(0, length(args))
        if args[i] == name
            if i == length(args) - 1
                return def_val
            return args[i+1]
    return def_val

def bytes_hr(value : uint64)
    if value > uint64(1024 * 1024)
        return "{float(value) / (1024f * 1024f)}mb ({int(value)})"
    if value > uint64(1024)
        return "{float(value) / 1024f}kb ({int(value)})"
    return "{int(value)} b"

def ends_with_separator(str : string)
    for c in "\\/"
        if ends_with(str, string(c))
            return true
    return false

def starts_with_separator(str : string)
    for c in "\\/"
        if starts_with(str, string(c))
            return true
    return false

def join_path(a, b : string; sep : string = "/")
    return build_string() <| $(builder)
        builder |> write(a)
        let ends = ends_with_separator(a)
        let starts = starts_with_separator(b)
        if ends && starts
            builder |> write(slice(b, 1))
        elif ! ends && !starts
            builder |> write(sep)
            builder |> write(b)
