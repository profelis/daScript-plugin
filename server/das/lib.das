options indenting = 4

require strings

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

def join_path(a, b : string; sep : string = "/")
    return build_string() <| $(builder)
        builder |> write(a)
        if ! ends_with(a, "/") // ! ends_with(a, "\\")
            builder |> write(sep)
        builder |> write(b)
