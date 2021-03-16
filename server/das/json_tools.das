options indenting = 4
options no_aot = true

require daslib/json
require daslib/json_boost

def clone(var a: JsonValue?&; b: JsonValue?)
    unsafe
        delete a
    if b == null
        a = JV(null)
    else
        a = new [[JsonValue value := b.value]]

def joj(val : JsonValue?; id : string) : JsonValue?
    var res : JsonValue? = null
    if val == null || !(val is _object)
        return res
    find_if_exists(val as _object, id) <| $(v)
        res = *v
    return res

def jon(val : JsonValue?; id : string; defVal = 0lf) : double
    var res = defVal
    if val == null || !(val is _object)
        return res
    find_if_exists(val.value as _object, id) <| $(v)
        unsafe
            res = (*v)?.value?._number ?? defVal
    return res

def j_s(val : JsonValue?; defVal = "") : string
    return val?.value ?as _string ?? defVal

def jos(val : JsonValue?; id : string; defVal = "") : string
    var res = defVal
    if val == null || !(val is _object)
        return res
    find_if_exists(val.value as _object, id) <| $(v)
        unsafe
            res = (*v)?.value?._string ?? defVal
    return res

def job(val : JsonValue?; id : string; defVal = false) : bool
    var res = defVal
    if val == null || !(val is _object)
        return res
    find_if_exists(val.value as _object, id) <| $(v)
        unsafe
            res = (*v)?.value?._bool ?? defVal
    return res
