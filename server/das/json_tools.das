require daslib/json


[unsafe]
def jojn(val : JsonValue?; id : string) : JsonValue? const?
    return find(val.value as _object, id)

[unsafe]
def joj(val : JsonValue?; id : string) : JsonValue?
    return find(val.value as _object, id) ?? JV(null)

def jon(val : JsonValue?; id : string; defVal = 0lf) : double
    return jonn(val, id) ?? defVal

[unsafe]
def jonn(val : JsonValue?; id : string) : double?
    var res : double?
	find(val.value as _object, id) <| $(val)
		if val != null
			res = (*val)?.value?._number
	return res

[unsafe]
def jos(val : JsonValue?; id : string; defVal = "") : string
    return josn(val, id) ?? defVal

[unsafe]
def josn(val : JsonValue?; id : string) : string?
    var res : string?
	find(val.value as _object, id) <| $(val)
		if val != null
			res = (*val)?.value?._string
	return res