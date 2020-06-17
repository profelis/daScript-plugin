// options heap_page = 0
// options string_heap_page = 0

require daslib/json

def joj(val : JsonValue?; id : string) : JsonValue?
    unsafe
		return val != null ? (find(val.value as _object, id) ?? null) : null

def jon(val : JsonValue?; id : string; defVal = 0lf) : double
    return jonn(val, id) ?? defVal

def jonn(val : JsonValue?; id : string) : double?
    var res : double?
	if val == null
		return res
	find(val.value as _object, id) <| $(val)
		if val != null
			unsafe
				res = (*val)?.value?._number
	return res

def jos(val : JsonValue?; id : string; defVal = "") : string
    return josn(val, id) ?? defVal

def j_s(val : JsonValue?; defVal = "") : string
    return val.value ?as _string ?? defVal

def josn(val : JsonValue?; id : string) : string?
    var res : string?
	if val == null
		return res
	find(val.value as _object, id) <| $(val)
		if val != null
			unsafe
				res = (*val)?.value?._string
	return res

def job(val : JsonValue?; id : string; defVal = false) : bool
    return jobn(val, id) ?? defVal

def jobn(val : JsonValue?; id : string) : bool?
    var res : bool?
	if val == null
		return res
	find(val.value as _object, id) <| $(val)
		if val != null
			unsafe
				res = (*val)?.value?._bool
	return res
