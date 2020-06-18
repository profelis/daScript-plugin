enum LogLevel
	Error = 1
	Warning = 2
	Info = 3
	Log = 4

var
	onLog : lambda<(msg : string; level : LogLevel) : void>

def error(data)
	var msg : string
	static_if typeinfo(is_string data)
		msg = data
	else
		msg = sprint(data, print_flags_debugger)
	if onLog != null
		invoke(onLog, msg, LogLevel Error)
	print("[error] ")
	print(msg)

def warning(data)
	var msg : string
	static_if typeinfo(is_string data)
		msg = data
	else
		msg = sprint(data, print_flags_debugger)
	if onLog != null
		invoke(onLog, msg, LogLevel Warning)
	print("[warn] ")
	print(msg)

def info(data)
	var msg : string
	static_if typeinfo(is_string data)
		msg = data
	else
		msg = sprint(data, print_flags_debugger)
	if onLog != null
		invoke(onLog, msg, LogLevel Info)
	print("[info] ")
	print(msg)

def log(data)
	var msg : string
	static_if typeinfo(is_string data)
		msg = data
	else
		msg = sprint(data, print_flags_debugger)
	if onLog != null
		invoke(onLog, msg, LogLevel Log)
	print("[log] ")
	print(msg)
