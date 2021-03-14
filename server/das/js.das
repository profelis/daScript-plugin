options indenting = 2
options remove_unused_symbols = false
module js shared


require daslib/apply
require daslib/json public
require daslib/json_boost public


def js(val: bool): JsonValue?
  return new [[JsonValue value <- [[JsValue _bool = val]]]]

def js(val: double): JsonValue?
  return new [[JsonValue value <- [[JsValue _number = val]]]]

def js(val: string): JsonValue?
  return new [[JsonValue value <- [[JsValue _string = val]]]]

def js(var val: table<string; JsonValue?>): JsonValue?
  return new [[JsonValue value <- [[JsValue _object <- val]]]]

def js(var val: array<JsonValue?>): JsonValue?
  return new [[JsonValue value <- [[JsValue _array <- val]]]]

def js(): JsonValue?
  return new [[JsonValue value <- [[JsValue _null = null]]]]

def js(val: int8): JsonValue?
  return new [[JsonValue value <- [[JsValue _number = double(val)]]]]

def js(val: uint8): JsonValue?
  return new [[JsonValue value <- [[JsValue _number = double(val)]]]]

def js(val: int16): JsonValue?
  return new [[JsonValue value <- [[JsValue _number = double(val)]]]]

def js(val: uint16): JsonValue?
  return new [[JsonValue value <- [[JsValue _number = double(val)]]]]

def js(val: int): JsonValue?
  return new [[JsonValue value <- [[JsValue _number = double(val)]]]]

def js(val: uint): JsonValue?
  return new [[JsonValue value <- [[JsValue _number = double(val)]]]]

def js(val: int64): JsonValue?
  return new [[JsonValue value <- [[JsValue _number = double(val)]]]]

def js(val: uint64): JsonValue?
  return new [[JsonValue value <- [[JsValue _number = double(val)]]]]

def js(val: float): JsonValue?
  return new [[JsonValue value <- [[JsValue _number = double(val)]]]]


def js(value): JsonValue?
  static_if typeinfo(typename value) == "json::JsonValue?" || typeinfo(typename value) == "json::JsonValue? const"
    return value
  elif typeinfo(is_pointer value)
    return value == null ? null : _::js(*value)
  elif typeinfo(is_string value)
    return _::js(value)
  elif typeinfo(is_struct value)
    var map: table<string; JsonValue?>
    apply(value) <| $(name: string; field)
      static_if typeinfo(is_function field)
        pass // skip functions
      elif typeinfo(is_void_pointer field)
        pass // skip void?
      elif typeinfo(is_pointer field)
        if field != null
          map[name] = _::js(field)
      else
        map[name] = _::js(field)
    return _::js(map)
  elif typeinfo(is_table value)
    var map: table<string; JsonValue?>
    for k, v in keys(value), values(value)
      map[k] = _::js(v)
    return _::js(map)
  elif typeinfo(is_iterable value)
    var arr: array<JsonValue?>
    for x in value
      arr |> push <| _::js(x)
    return _::js(arr)
  else
    print("unsupported type {typeinfo(typename value)}\n")
    concept_assert(false, "unknown js()")
    return null


def js(val1; val2): JsonValue?
  return _::js([{auto[] _::js(val1); _::js(val2)}])

def js(val1; val2; val3): JsonValue?
  return _::js([{auto[] _::js(val1); _::js(val2); _::js(val3)}])

def js(val1; val2; val3; val4): JsonValue?
  return _::js([{auto[] _::js(val1); _::js(val2); _::js(val3); _::js(val4)}])

def js(val1; val2; val3; val4; val5): JsonValue?
  return _::js([{auto[] _::js(val1); _::js(val2); _::js(val3); _::js(val4); _::js(val5)}])

def js(val1; val2; val3; val4; val5; val6): JsonValue?
  return _::js([{auto[] _::js(val1); _::js(val2); _::js(val3); _::js(val4); _::js(val5); _::js(val6)}])

def js(val1; val2; val3; val4; val5; val6; val7): JsonValue?
  return _::js([{auto[] _::js(val1); _::js(val2); _::js(val3); _::js(val4); _::js(val5); _::js(val6); _::js(val7)}])

def js(val1; val2; val3; val4; val5; val6; val7; val8): JsonValue?
  return _::js([{auto[] _::js(val1); _::js(val2); _::js(val3); _::js(val4); _::js(val5); _::js(val6); _::js(val7); _::js(val8)}])

def js(val1; val2; val3; val4; val5; val6; val7; val8; val9): JsonValue?
  return _::js([{auto[] _::js(val1); _::js(val2); _::js(val3); _::js(val4); _::js(val5); _::js(val6); _::js(val7); _::js(val8); _::js(val9)}])

def js(val1; val2; val3; val4; val5; val6; val7; val8; val9; val10): JsonValue?
  return _::js([{auto[] _::js(val1); _::js(val2); _::js(val3); _::js(val4); _::js(val5); _::js(val6); _::js(val7); _::js(val8); _::js(val9); _::js(val10)}])
