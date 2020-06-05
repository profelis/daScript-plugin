require rtti
require lsp_types

def lineInfoToRange(val : rtti::LineInfo)
    return [[Range start = [[Position line = val.line - 1u, character = val.column]], end = [[Position line = val.last_line - 1u, character = val.last_column + 1u]] ]]
