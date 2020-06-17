require rtti
require lsp_types
require math

def line_info_to_range(val : rtti::LineInfo)
    return [[Range
        start = [[Position line = max(0, int(val.line) - 1), character = int(val.column)]],
        end = [[Position line = max(0, int(val.last_line) - 1), character = int(val.last_column) + 1]]
    ]]
