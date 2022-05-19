options indenting = 4
options no_aot = true

require rtti_tools
require rtti

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

def error(data)
    log(data, get_line_info(1), LogLevel Error)

def warning(data)
    log(data, get_line_info(1), LogLevel Warning)

def info(data)
    log(data, get_line_info(1), LogLevel Info)

def log(data)
    log(data, get_line_info(1), LogLevel Log)
