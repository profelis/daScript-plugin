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


def log(data; li : LineInfo; level : LogLevel)
    var msg : string
    static_if typeinfo(is_string data)
        msg = "{data} {to_string(li)}\n"
    else
        msg = "{sprint(data, print_flags_debugger)} {to_string(li)}\n"
    if onLog != null
        invoke(onLog, msg, level)
    print("[{level}] {msg}")

def error(data; li : LineInfo)
    log(data, li, LogLevel Error)

def warning(data; li : LineInfo)
    log(data, li, LogLevel Warning)

def info(data; li : LineInfo)
    log(data, li, LogLevel Info)

def log(data; li : LineInfo)
    log(data, li, LogLevel Log)
