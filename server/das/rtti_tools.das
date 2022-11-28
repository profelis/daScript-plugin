module rtti_tools shared

options indenting = 4
options no_aot = true

require rtti
require lsp_types
require math

def line_info_to_range(val : rtti::LineInfo)
    var res <- Range(
        max(0, int(val.line) - 1), int(val.column),
        max(0, int(val.last_line) - 1), int(val.last_column)
    )
    if res.end.character > 0 && range_oneline(res)
        res.end.character += 1
    return <- res
