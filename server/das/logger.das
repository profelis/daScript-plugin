options indenting = 4
options no_aot = true

require rtti_tools

enum LogLevel
    Error = 1
    Warning = 2
    Info = 3
    Log = 4

var
    onLog : lambda<(msg : string; level : LogLevel) : void>

def error(data; li : rtti::LineInfo)
    var msg : string
    static_if typeinfo(is_string data)
        msg = "{data} {to_string(li)}\n"
    else
        msg = "{sprint(data, print_flags_debugger)} {to_string(li)}\n"
    if onLog != null
        invoke(onLog, msg, LogLevel Error)
    print("[error] ")
    print(msg)

def warning(data; li : rtti::LineInfo)
    var msg : string
    static_if typeinfo(is_string data)
        msg = "{data} {to_string(li)}\n"
    else
        msg = "{sprint(data, print_flags_debugger)} {to_string(li)}\n"
    if onLog != null
        invoke(onLog, msg, LogLevel Warning)
    print("[warn] ")
    print(msg)

def info(data; li : rtti::LineInfo)
    var msg : string
    static_if typeinfo(is_string data)
        msg = "{data} {to_string(li)}\n"
    else
        msg = "{sprint(data, print_flags_debugger)} {to_string(li)}\n"
    if onLog != null
        invoke(onLog, msg, LogLevel Info)
    print("[info] ")
    print(msg)

def log(data; li : rtti::LineInfo)
    var msg : string
    static_if typeinfo(is_string data)
        msg = "{data} {to_string(li)}\n"
    else
        msg = "{sprint(data, print_flags_debugger)} {to_string(li)}\n"
    if onLog != null
        invoke(onLog, msg, LogLevel Log)
    print("[log] ")
    print(msg)
