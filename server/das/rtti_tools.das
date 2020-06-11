require rtti
require lsp_types

def line_info_to_range(val : rtti::LineInfo)
    return [[Range
        start = [[Position line = int(val.line) - 1, character = int(val.column)]],
        end = [[Position line = int(val.last_line) - 1, character = int(val.last_column) + 1]]
    ]]

def line_info_in(val : LineInfo; line, character : uint)
    if line < val.line | (line == val.line & character < val.column)
        return false
    if line > val.last_line | (line == val.last_line & character > val.last_column)
        return false
    return true
