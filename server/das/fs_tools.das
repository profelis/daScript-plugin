module fs_tools shared

options indenting = 4
options no_aot = true

require fio
require uriparser
require strings

// def urlDecode(str : string) : string
//     return join(split(str, "%3A"), ":")

// def urlEncode(str : string) : string
//     return join(split(str, ":"), "%3A")

// def uri_to_path(str : string)
//     let path = urlDecode(str)
//     unsafe
//         return path |> character_at(9) == ':' ? chop(path, 8, length(path) - 8) : chop(path, 7, length(path) - 7)

// def path_to_uri(str : string)
//     let prefix = str |> character_at(1) == ':' ? "file:///" : "file://"
//     return "{prefix}{urlEncode(str)}"

def uri_to_path(str : string)
    return str |> normalize_uri() |> uri_to_file_name()

def path_to_uri(str : string)
    return file_name_to_uri(str) |> normalize_uri()


def private ends_with_separator(str : string)
    for c in "\\/"
        if ends_with(str, to_char(c))
            return true
    return false


def private starts_with_separator(str : string)
    for c in "\\/"
        if starts_with(str, to_char(c))
            return true
    return false

def join_path(a, b : string)
    var res = build_string() <| $(builder)
        builder |> write(a)
        let ends = ends_with_separator(a)
        let starts = starts_with_separator(b)
        if ends && starts
            builder |> write(slice(b, 1))
        elif ! ends && !starts
            builder |> write("/")
            builder |> write(b)
        else
            builder |> write(b)
    return fix_path(res)

def fix_path(path : string)
    return path |> path_to_uri() |> normalize_uri() |> uri_to_path()

def scan_dir(path : string; var res : table<string; void?>; suffix = ".das")
    fio::dir(path) <| $(n)
        if n == "." || n == ".."
            return
        let f = path |> join_path(n)
        let fStat = stat(f)
        if !fStat.is_valid
            return
        if fStat.is_dir
            f |> scan_dir(res)
        elif fStat.is_reg && f |> ends_with(suffix) && !res |> key_exists(f)
            res[f] = null
