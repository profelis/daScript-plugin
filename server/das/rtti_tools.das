require rtti
require lsp_types
require math

def line_info_to_range(val : rtti::LineInfo)
    var res <- Range(
        max(0, int(val.line) - 1), int(val.column),
        max(0, int(val.last_line) - 1), int(val.last_column)
    )
    if (res.end.character > 0 &&
        (res.start.line > 0 || res.start.character > 0 || res.end.line > 0)
    )
        res.end.character ++
    return <- res

def to_string(val : rtti::LineInfo)
    return "{val.fileInfo.name}:{int(val.line)}"