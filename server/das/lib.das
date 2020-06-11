require daslib/strings

[export]
def move ( var a )
    return <- a

[generic]
def strip_left(str : string; part : string)
	return starts_with(str, part) ? slice(str, length(part)) : str

// [generic]
// def replace(str, from, to : string)
// 	var in scope parts <- split(str, from)
// 	return join(parts, to)