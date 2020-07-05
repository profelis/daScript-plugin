
require ast
require math
require rtti_tools
require strings
require lsp_types
require logger
require completion

bitfield AstTokenWarn
    unused
    deprecated

enum AstTokenKind
    File = 1
    Module = 2
    Namespace = 3
    Package = 4
    Class = 5
    Method = 6
    Property = 7
    Field = 8
    Constructor = 9
    Enum = 10
    Interface = 11
    Function = 12
    Variable = 13
    Constant = 14
    String = 15
    Number = 16
    Boolean = 17
    Array = 18
    Object = 19
    Key = 20
    Null = 21
    EnumMember = 22
    Struct = 23
    Event = 24
    Operator = 25
    TypeParameter = 26

struct AstToken
    topLevel : bool
    _range : Range
    path : string

    name : string
    desc : string
    kind : AstTokenKind
    _type : string
    warn : AstTokenWarn
    children : array<AstToken?>

    defRange : Range
    defPath : string

    typeDefRange : Range
    typeDefPath : string

typedef
    AstTokens = array<AstToken?>


def to_hover(val : AstToken?; verbose : bool)
    if !verbose
        return "```dascript\n{val.desc}\n```"
    var res = "```dascript\n{val.desc}\nname: {val.name}\ntype: {val._type}\nkind: {val.kind}\n{val.path} @ {to_string(val._range)}\n{val.defPath} @ {to_string(val.defRange)}\n{val.typeDefPath} @ {to_string(val.typeDefRange)}\n```"
    return <- res


def collect_diag(val : AstToken?; path : string; var diag : array<Diagnostic>)
    if val.path == path && val.warn != bitfield(0)
        var d <- [[Diagnostic _range = val._range ]]
        if val.warn.unused
            d.message = "[Unused] {val.desc}"
            d.severity = DiagnosticSeverity Information
        else
            d.message = "[Deprecated] {val.desc}"
            d.severity = DiagnosticSeverity Warning
        diag |> emplace(d)
    for it in val.children
        collect_diag(it, path, diag)

def collect_diag(val : AstTokens; path : string; var diag : array<Diagnostic>)
    for it in val
        collect_diag(it, path, diag)

def collect_token(var val : AstToken?; blk : block<(var token:AstToken?):void>)
    invoke(blk, val)
    for it in val.children
        collect_token(it, blk)

def collect_token(var val : AstTokens; blk : block<(var token:AstToken?):void>)
    for it in val
        collect_token(it, blk)

def collect_token(val : AstToken?; path : string; blk : block<(token:AstToken?):void>)
    if val.path == path
        invoke(blk, val)
    for it in val.children
        collect_token(it, path, blk)

def collect_token(val : AstTokens; path : string; blk : block<(token:AstToken?):void>)
    for it in val
        collect_token(it, path, blk)

def collect_token(val : AstToken?; path : string; pos : Position; blk : block<(token:AstToken?):void>)
    if val.path == path && pos |> in_range(val._range)
        invoke(blk, val)
    for it in val.children
        collect_token(it, path, pos, blk)

def collect_token(val : AstTokens; path : string; pos : Position; blk : block<(token:AstToken?):void>)
    for it in val
        collect_token(it, path, pos, blk)

def collect_ref_token(val : AstToken?; path : string; _range : Range; blk : block<(token:AstToken?):void>)
    if val.path != "" && val.defPath == path && val.defRange |> in_range(_range) && ! val._range |> in_range(_range) &&  ! val._range |> in_range(val.defRange)
        invoke(blk, val)
    for it in val.children
        collect_ref_token(it, path, _range, blk)

def collect_ref_token(val : AstTokens; path : string; _range : Range; blk : block<(token:AstToken?):void>)
    for it in val
        collect_ref_token(it, path, _range, blk)

def module_name(mod)
    if mod == null
        return ""
    let name = "{mod.name}"
    return length(name) == 0 ? "" : "{name}::"

def set_token_path(var token : AstToken?; at)
    token._range = line_info_to_range(at)
    token.path = at.fileInfo != null ? "{at.fileInfo.name}" : ""

def set_token_def(var token : AstToken?; at)
    token.defRange = line_info_to_range(at)
    token.defPath = at.fileInfo != null ? "{at.fileInfo.name}" : ""

def set_token_type_def(var token : AstToken?; typ)
    if typ != null
        token.typeDefRange = line_info_to_range(typ.at)
        token.typeDefPath = typ.at.fileInfo != null ? "{typ.at.fileInfo.name}" : ""

def var_to_token(arg; blk : block<(var token:AstToken?):void>)
    if arg == null || arg.flags.generated
        return false
    let typ = arg._type != null ? "{module_name(arg._module)}{describe([decl=arg._type,extra=true])}" : ""
    var desc = "{arg.name} : {typ}"
    if !arg._type.flags.constant
        desc = "var " + desc
    if arg.init != null
        desc += " = {describe(arg.init)}"
    var token <- new [[AstToken
        name = "{arg.name}",
        desc = desc,
        kind = AstTokenKind Variable,
        _type = typ,
        warn = arg.isAccessUnused ? AstTokenWarn unused : bitfield(0)
    ]]
    set_token_path(token, arg.at)
    set_token_def(token, arg.at)
    if arg._type != null && arg._type.structType != null
        set_token_type_def(token, arg._type.structType)
    invoke(blk, token)
    return true


def field_to_token(expr; blk : block<(var token:AstToken?):void>)
    if expr == null
        return false
    let typ = expr._type != null ? "{describe([decl=expr._type,extra=true])}" : ""
    var name = "{expr.name}"
    var desc = "{name} : {typ}"
    if expr.field != null && expr.field.init != null
        desc = "{desc} = {describe(expr.field.init)}"
    var res <- new [[AstToken
        name = name,
        desc = desc,
        kind = AstTokenKind Field,
        _type = typ
    ]]
    set_token_path(res, expr.atField)
    if expr.field != null
        set_token_def(res, expr.field.at)
        if expr.field._type != null && expr.field._type.structType != null
            set_token_type_def(res, expr.field._type.structType)
    if range_oneline(res._range) && typ |> starts_with("function<")
        res.kind = AstTokenKind Method
        let selfLen = 4
        res._range.start.character = res._range.start.character > selfLen ? res._range.start.character - selfLen : res._range.start.character
        res._range.end.character += length(res.name)
    invoke(blk, res)
    return true


def like_call_to_token(expr; blk : block<(var token:AstToken?):void>)
    if expr == null
        return false
    // info(expr._type)
    var typ = "def {expr.name}"
    var firstArg : string
    if expr.arguments.length > 0
        typ += "("
        var idx = 1
        for arg in expr.arguments
            if idx == 1
                firstArg = describe(arg)
            else
                typ += ", "
            // if arg is ExprMakeBlock
                // var blk & = reinterpret<smart_ptr<ExprMakeBlock>>(arg)
                // info(blk)
                // typ += "__blk__"
                // continue
            if arg._type != null && !arg._type.flags.constant
                typ += "var "
            typ += "arg{idx} : {arg._type != null ? describe([decl=arg._type,extra=true]) : "" }"
            idx++
        typ += ")"
    // if firstArg |> starts_with("self.")
    //     return false
    if expr._type != null
        typ += " : {describe([decl=expr._type,extra=true])}"
    var res <- new [[AstToken
        name = "{expr.name}",
        desc = typ,
        kind = AstTokenKind Function,
        _type = typ
    ]]
    set_token_path(res, expr.at)
    if range_oneline(res._range) && firstArg |> starts_with("self.")
        let selfLen = 4
        res._range.start.character = res._range.start.character > selfLen ? res._range.start.character - selfLen : res._range.start.character
        res._range.end.character += length(firstArg) - selfLen - 1
    invoke(blk, res)
    return true

def call_to_token(expr; blk : block<(var token:AstToken?):void>)
    if expr == null
        return false
    // info(expr._type)
    var typ = "def {expr.name}"
    var firstArg : string
    if expr.arguments.length > 0
        typ += "("
        var idx = 1
        for arg in expr.arguments
            if idx == 1
                firstArg = describe(arg)
            else
                typ += ", "
            // if arg is ExprMakeBlock
                // var blk & = reinterpret<smart_ptr<ExprMakeBlock>>(arg)
                // info(blk)
                // typ += "__blk__"
                // continue
            if arg._type != null && !arg._type.flags.constant
                typ += "var "
            typ += "arg{idx} : {arg._type != null ? describe([decl=arg._type,extra=true]) : "" }"
            idx++
        typ += ")"
    // if firstArg |> starts_with("self.")
    //     return false
    if expr._type != null
        typ += " : {describe([decl=expr._type,extra=true])}"
    var res <- new [[AstToken
        name = "{expr.name}",
        desc = typ,
        kind = AstTokenKind Function,
        _type = typ
    ]]
    set_token_path(res, expr.at)
    if expr.func != null
        set_token_def(res, expr.func.atDecl)
    if range_oneline(res._range) && firstArg |> starts_with("self.")
        let selfLen = 4
        res._range.start.character = res._range.start.character > selfLen ? res._range.start.character - selfLen : res._range.start.character
        res._range.end.character += length(firstArg) - selfLen - 1
    invoke(blk, res)
    return true

def type_to_token(typ; blk : block<(var token:AstToken?):void>)
    if typ == null
        return false
    let typeName = describe([decl=typ,extra=true])
    var tok <- new [[AstToken
        name = typeName,
        desc = typeName,
        kind = AstTokenKind Struct,
        _type = typeName
    ]]
    set_token_path(tok, typ.at)
    if typ.structType != null
        set_token_def(tok, typ.structType.at)
    invoke(blk, tok)
    return true

[private]
def addCompletionSwizzle(main_type : string; num : int; typ : string; var res : array<Completion>)
    let arr = [[auto "x"; "y"; "z"; "w"]]
    for i in range(0, num)
        let desc = "{main_type}.{arr[i]} : {typ}"
        var comp <- [[Completion
            name = arr[i],
            fullName = "{main_type}.{desc}",
            desc = desc,
            kind = CompletionItemKind Field
        ]]
        res |> emplace(comp)

def parse(t; var res : AstCompletionData)
    if t == null
        return
    // info("@@@@@@@@@@@@@", get_line_info())
    // info(t, get_line_info())
    // info("@@@@@@@@@@@@@", get_line_info())
    if t.annotation != null
        res.found = true
        unsafe
            let baseAnnotation & = reinterpret<rtti::Annotation?> t.annotation
            var items: Completions
            parseAnnotation(*baseAnnotation, module_name(t._module), items, false /*include struct name*/)
            for it in values(items)
                res.res |> emplace(it)
            delete items

    elif t.structType != null
        res.found = true
        let structType = t.structType
        let structTyp = describe([decl=t,extra=false,contracts=false])
        for decl in structType.fields
            let typ = decl._type != null ? "{describe([decl=decl._type,extra=true])}" : ""
            let fieldName = "{decl.name}"
            let fullName = "{structTyp}.{fieldName}"
            var fieldDesc = "{fullName} : {typ}"
            if decl.init != null
                fieldDesc += " = {describe(decl.init)}"
            if decl.offset != -1
                fieldDesc += " [@ {decl.offset}]"
            var comp <- [[Completion
                name = fieldName,
                fullName = fullName,
                desc = fieldDesc,
                kind = CompletionItemKind Field
            ]]
            // info(comp, get_line_info())
            res.res |> emplace(comp)

    if t.argTypes.length > 0 || t.argNames.length > 0
        res.found = true
        for i in range(0, max(t.argTypes.length, t.argNames.length))
            let name = t.argNames.length > i ? "{t.argNames[i]}" : "_{i}"
            var desc = name
            if t.argTypes.length > i && t.argTypes[i] != null
                desc += " : {describe(t.argTypes[i])}"
            if t.baseType == Type tBitfield
                desc += " // bitfield"
            var comp <- [[Completion
                name = name,
                fullName = "{describe([decl=t,extra=false,contracts=false])}.{name}",
                desc = desc,
                kind = CompletionItemKind Field
            ]]
            res.res |> emplace(comp)

    if t.baseType == Type tFloat2
        addCompletionSwizzle(describe([decl=t,extra=false,contracts=false]), 2, "float", res.res)
    elif t.baseType == Type tFloat3
        addCompletionSwizzle(describe([decl=t,extra=false,contracts=false]), 3, "float", res.res)
    elif t.baseType == Type tFloat4
        addCompletionSwizzle(describe([decl=t,extra=false,contracts=false]), 4, "float", res.res)
    elif t.baseType == Type tInt2
        addCompletionSwizzle(describe([decl=t,extra=false,contracts=false]), 2, "int", res.res)
    elif t.baseType == Type tInt3
        addCompletionSwizzle(describe([decl=t,extra=false,contracts=false]), 3, "int", res.res)
    elif t.baseType == Type tInt4
        addCompletionSwizzle(describe([decl=t,extra=false,contracts=false]), 4, "int", res.res)
    elif t.baseType == Type tUInt2
        addCompletionSwizzle(describe([decl=t,extra=false,contracts=false]), 2, "uint", res.res)
    elif t.baseType == Type tUInt3
        addCompletionSwizzle(describe([decl=t,extra=false,contracts=false]), 3, "uint", res.res)
    elif t.baseType == Type tUInt4
        addCompletionSwizzle(describe([decl=t,extra=false,contracts=false]), 4, "uint", res.res)

    if t.firstType != null
        parse(t.firstType, res)

class AstCompletionData : AstVisitor
    path : string
    pos : Position
    found : bool = false
    res : array<Completion>

// var
    def override preVisitExprVar(expr:smart_ptr<ExprVar>) : void
        if found || expr == null
            return
        if string(expr.at.fileInfo.name) != path
            return
        let _range = line_info_to_range(expr.at)
        if !pos |> in_range(_range)
            return
        // info(_range, get_line_info())
        parse(expr._type, self)

// field
    def override preVisitExprField(expr:smart_ptr<ExprField>) : void
        if found || expr == null
            return
        if string(expr.at.fileInfo.name) != path
            return
        let _range = line_info_to_range(expr.atField)
        if !pos |> in_range(_range)
            return
        // info(_range, get_line_info())
        // info(expr, get_line_info())
        parse(expr._type, self)

// safe field
    def override preVisitExprSafeField(expr:smart_ptr<ExprSafeField>) : void
        if found || expr == null
            return
        if string(expr.at.fileInfo.name) != path
            return
        let _range = line_info_to_range(expr.atField)
        if !pos |> in_range(_range)
            return
        // info(_range, get_line_info())
        parse(expr._type, self)
// looks like call (debug,assert,verify,erase,find,key_exists,keys,values,invoke,memzero etc)
    def override preVisitExprLooksLikeCall(expr:smart_ptr<ExprLooksLikeCall>): void
        if found || expr == null
            return
        if string(expr.at.fileInfo.name) != path
            return
        let _range = line_info_to_range(expr.at)
        if !pos |> in_range(_range)
            return
        // info(_range, get_line_info())
        parse(expr._type, self)
// call
    def override preVisitExprCall(expr:smart_ptr<ExprCall>): void
        if found || expr == null
            return
        if string(expr.at.fileInfo.name) != path
            return
        let _range = line_info_to_range(expr.at)
        if !pos |> in_range(_range)
            return
        // info(_range, get_line_info())
        parse(expr._type, self)
// ptr to ref
    def override preVisitExprPtr2Ref(expr:smart_ptr<ExprPtr2Ref>) : void
        if found || expr == null
            return
        if string(expr.at.fileInfo.name) != path
            return
        let _range = line_info_to_range(expr.at)
        if !pos |> in_range(_range)
            return
        // info(_range, get_line_info())
        parse(expr._type, self)

class AstData : AstVisitor
    tokens : AstTokens?
    skipExprs : bool = false
// program
    // def override preVisitProgram(prog:ProgramPtr) : void
    // def override visitProgram(porg:ProgramPtr) : void
    // def override preVisitProgramBody(prog:ProgramPtr) : void
// type
    def override preVisitTypeDecl(typ:TypeDeclPtr) : void
        if typ == null
            return
        type_to_token(typ) <| $(var token : AstToken?)
            *tokens |> emplace(token)
    // def override visitTypeDecl(typ:TypeDeclPtr) : TypeDeclPtr
// type alias
    def override preVisitAlias(typ:TypeDeclPtr;name:das_string) : void
        if typ == null
            return
        type_to_token(typ) <| $(var token : AstToken?)
            *tokens |> emplace(token)
    // def override visitAlias(typ:TypeDeclPtr;name:das_string) : TypeDeclPtr
// enumeration
    def override preVisitEnumeration(enu:EnumerationPtr) : void
        if enu == null
            return
        var name = "{module_name(enu._module)}{enu.name}"
        var token <- new [[AstToken
            topLevel = true,
            name = name,
            desc = "enum {name} : {get_das_type_name(enu.baseType)}",
            kind = AstTokenKind Enum,
            _type = name
        ]]
        set_token_path(token, enu.at)
        for it in enu.list
            var typ = it.value._type != null ? describe(it.value._type) : ""
            var desc = "{enu.name} {it.name}"
            if it.value != null
                desc += " = {describe(it.value)}"
            var tok <- new [[AstToken
                topLevel = true,
                name = "{it.name}",
                desc = desc,
                kind = AstTokenKind EnumMember,
                _type = typ
            ]]
            set_token_path(tok, it.at)
            token.children |> emplace(tok)
        *tokens |> emplace(token)

    // def override preVisitEnumerationValue(enu:EnumerationPtr;name:das_string;value:ExpressionPtr;last:bool) : void
    // def override visitEnumerationValue(enu:EnumerationPtr;name:das_string;value:ExpressionPtr;last:bool) : ExpressionPtr
    // def override visitEnumeration(enu:EnumerationPtr) : EnumerationPtr
// // structure
    def override preVisitStructure(str:StructurePtr) : void
        if str == null
            return
        if str.flags.generated || str.flags.isLambda
            return
        var name = "{module_name(str._module)}{str.name}"
        var desc = "struct {name}"
        if str.parent != null
            desc += " : {module_name(str.parent._module)}{str.parent.name}"
        var token <- new [[AstToken
            topLevel = true,
            name = name,
            desc = desc,
            kind = str.flags.isClass ? AstTokenKind Class : AstTokenKind Struct,
            _type = name
        ]]
        set_token_path(token, str.at)
        for decl in str.fields
            if decl.flags.generated
                continue
            let typ = decl._type != null ? "{describe([decl=decl._type,extra=true])}" : ""
            var fieldName = "{decl.name}"
            var fieldDesc = "{fieldName} : {typ}"
            if decl.init != null
                fieldDesc += " = {describe(decl.init)}"
            var tok <- new [[AstToken
                topLevel = true,
                name = fieldName,
                desc = fieldDesc,
                kind = AstTokenKind Field,
                _type = typ
            ]]
            set_token_path(tok, decl.at)
            token.children |> emplace(tok)
        *tokens |> emplace(token)
    // def override preVisitStructureField(str:StructurePtr;decl:FieldDeclaration;last:bool) : void
    // def override visitStructureField(str:StructurePtr;decl:FieldDeclaration;last:bool) : void
    // def override visitStructure(str:StructurePtr) : StructurePtr
// function
    def override preVisitFunction(fun:FunctionPtr) : void
        if fun == null
            return
        if fun.flags.generated && !fun.flags._generator && !fun.flags._lambda
            skipExprs = true
            return
        if fun.flags._lambda
            return
        var desc = "def {module_name(fun._module)}{fun.name}"
        if fun.arguments.length >= 0
            desc += "("
            var first = true
            for arg in fun.arguments
                if !first
                    desc += "; "
                first = false
                if !arg._type.flags.constant
                    desc += "var "
                desc += "{arg.name} : {describe([decl=arg._type,extra=true])}"
                if arg.init != null
                    desc += " = {describe(arg.init)}"
            desc += ")"
        if fun.result != null
            desc += " : {describe([decl=fun.result,extra=true])}"
        var token <- new [[AstToken
            // topLevel = fun.body != null,
            name = "{fun.name}",
            desc = desc,
            kind = fun.flags.isClassMethod ? AstTokenKind Method : AstTokenKind Function,
            _type = desc
        ]]
        set_token_path(token, fun.at)
        set_token_def(token, fun.atDecl)
        token.topLevel = true //!fun.flags.isClassMethod //!fun.flags.generated && token._range |> in_range(token.defRange)
        for it in fun.arguments
            var_to_token(it) <| $(var arg : AstToken?)
                token.children |> emplace(arg)
        *tokens |> emplace(token)
    def override visitFunction(fun:FunctionPtr) : FunctionPtr
        skipExprs = false
        return fun
    // def override preVisitFunctionArgument(fun:FunctionPtr;arg:VariablePtr;lastArg:bool) : void
    // def override visitFunctionArgument(fun:FunctionPtr;arg:VariablePtr;lastArg:bool) : VariablePtr
    // def override preVisitFunctionArgumentInit(fun:FunctionPtr;arg:VariablePtr;value:ExpressionPtr) : void
    // def override visitFunctionArgumentInit(fun:FunctionPtr;arg:VariablePtr;value:ExpressionPtr) : ExpressionPtr
    // def override preVisitFunctionBody(fun:FunctionPtr) : void
    // def override visitFunctionBody(fun:FunctionPtr) : void
// any expression there is
    // def override preVisitExpression(expr:ExpressionPtr) : void
    // def override visitExpression(expr:ExpressionPtr) : ExpressionPtr
// block
    // def override preVisitExprBlock(fun:smart_ptr<ExprBlock>) : void
    // def override visitExprBlock(blk:smart_ptr<ExprBlock>) : ExpressionPtr
    def override preVisitExprBlockArgument(blk:smart_ptr<ExprBlock>;arg:VariablePtr;lastArg:bool): void
        if skipExprs || blk == null
            return
        var_to_token(arg) <| $(var token : AstToken?)
            *tokens |> emplace(token)
    // def override visitExprBlockArgument(blk:smart_ptr<ExprBlock>;arg:VariablePtr;lastArg:bool) : VariablePtr
    // def override preVisitExprBlockArgumentInit(blk:smart_ptr<ExprBlock>;arg:VariablePtr;expr:ExpressionPtr): void
    // def override visitExprBlockArgumentInit(blk:smart_ptr<ExprBlock>;arg:VariablePtr;expr:ExpressionPtr) : ExpressionPtr
    // def override preVisitExprBlockExpression(blk:smart_ptr<ExprBlock>;expr:ExpressionPtr): void
    // def override visitExprBlockExpression(blk:smart_ptr<ExprBlock>;expr:ExpressionPtr) : ExpressionPtr
    // def override preVisitExprBlockFinal(blk:smart_ptr<ExprBlock>): void
    // def override visitExprBlockFinal(blk:smart_ptr<ExprBlock>): void
    // def override preVisitExprBlockFinalExpression(blk:smart_ptr<ExprBlock>;expr:ExpressionPtr): void
    // def override visitExprBlockFinalExpression(blk:smart_ptr<ExprBlock>;expr:ExpressionPtr) : ExpressionPtr
// let
    def override preVisitExprLet(expr:smart_ptr<ExprLet>) : void
        if skipExprs || expr == null
            return
        for it in expr.variables
            var_to_token(it) <| $(var token : AstToken?)
                *tokens |> emplace(token)
    // def override visitExprLet(expr:smart_ptr<ExprLet>) : ExpressionPtr
    // def override preVisitExprLetVariable(expr:smart_ptr<ExprLet>;arg:VariablePtr;lastArg:bool) : void
    // def override visitExprLetVariable(expr:smart_ptr<ExprLet>;arg:VariablePtr;lastArg:bool) : VariablePtr
    // def override preVisitExprLetVariableInit(blk:smart_ptr<ExprLet>;arg:VariablePtr;expr:ExpressionPtr): void
    // def override visitExprLetVariableInit(blk:smart_ptr<ExprLet>;arg:VariablePtr;expr:ExpressionPtr) : ExpressionPtr
// global let
    // def override preVisitGlobalLet(prog:ProgramPtr) : void
    // def override visitGlobalLet(prog:ProgramPtr) : void
    def override preVisitGlobalLetVariable(arg:VariablePtr;lastArg:bool) : void
        if skipExprs || arg == null
            return
        var_to_token(arg) <| $(var token : AstToken?)
            token.topLevel = true
            *tokens |> emplace(token)
    // def override visitGlobalLetVariable(arg:VariablePtr;lastArg:bool) : VariablePtr
    // def override preVisitGlobalLetVariableInit(arg:VariablePtr;expr:ExpressionPtr): void
    // def override visitGlobalLetVariableInit(arg:VariablePtr;expr:ExpressionPtr) : ExpressionPtr
// string builder
    // def override preVisitExprStringBuilder(expr:smart_ptr<ExprStringBuilder>): void
    // def override visitExprStringBuilder(expr:smart_ptr<ExprStringBuilder>) : ExpressionPtr
    // def override preVisitExprStringBuilderElement(expr:smart_ptr<ExprStringBuilder>;elem:ExpressionPtr;last:bool): void
    // def override visitExprStringBuilderElement(expr:smart_ptr<ExprStringBuilder>;elem:ExpressionPtr;last:bool) : ExpressionPtr
// new
    // def override preVisitExprNew(expr:smart_ptr<ExprNew>): void
    // def override visitExprNew(expr:smart_ptr<ExprNew>) : ExpressionPtr
    // def override preVisitExprNewArgument(expr:smart_ptr<ExprNew>;arg:ExpressionPtr;last:bool): void
    // def override visitExprNewArgument(expr:smart_ptr<ExprNew>;arg:ExpressionPtr;last:bool) : ExpressionPtr
// named call (this one only exists before or during the infer)
    // def override preVisitExprNamedCall(expr:smart_ptr<ExprNamedCall>): void
    // def override visitExprNamedCall(expr:smart_ptr<ExprNamedCall>) : ExpressionPtr
    // def override preVisitExprNamedCallArgument(expr:smart_ptr<ExprNamedCall>;arg:MakeFieldDeclPtr;last:bool): void
    // def override visitExprNamedCallArgument(expr:smart_ptr<ExprNamedCall>;arg:MakeFieldDeclPtr;last:bool) : MakeFieldDeclPtr
// looks like call (debug,assert,verify,erase,find,key_exists,keys,values,invoke,memzero etc)
    def override preVisitExprLooksLikeCall(expr:smart_ptr<ExprLooksLikeCall>): void
        if skipExprs || expr == null
            return
        like_call_to_token(expr) <| $(var token : AstToken?)
            *tokens |> emplace(token)
    // def override visitExprLooksLikeCall(expr:smart_ptr<ExprLooksLikeCall>) : ExpressionPtr
    // def override preVisitExprLooksLikeCallArgument(expr:smart_ptr<ExprLooksLikeCall>;arg:ExpressionPtr;last:bool): void
    // def override visitExprLooksLikeCallArgument(expr:smart_ptr<ExprLooksLikeCall>;arg:ExpressionPtr;last:bool) : ExpressionPtr
// call
    def override preVisitExprCall(expr:smart_ptr<ExprCall>): void
        if skipExprs || expr == null
            return
        call_to_token(expr) <| $(var token : AstToken?)
            *tokens |> emplace(token)
    // def override visitExprCall(expr:smart_ptr<ExprCall>) : ExpressionPtr
    // def override preVisitExprCallArgument(expr:smart_ptr<ExprCall>;arg:ExpressionPtr;last:bool): void
    // def override visitExprCallArgument(expr:smart_ptr<ExprCall>;arg:ExpressionPtr;last:bool) : ExpressionPtr
// null coaelescing
    // def override preVisitExprNullCoalescing(expr:smart_ptr<ExprNullCoalescing>): void
    // def override visitExprNullCoalescing(expr:smart_ptr<ExprNullCoalescing>) : ExpressionPtr
    // def override preVisitExprNullCoalescingDefault(expr:smart_ptr<ExprNullCoalescing>;defval:ExpressionPtr): void
// at
    // def override preVisitExprAt(expr:smart_ptr<ExprAt>): void
    // def override visitExprAt(expr:smart_ptr<ExprAt>) : ExpressionPtr
    // def override preVisitExprAtIndex(expr:smart_ptr<ExprAt>;index:ExpressionPtr): void
// // safe at
    // def override preVisitExprSafeAt(expr:smart_ptr<ExprSafeAt>): void
    // def override visitExprSafeAt(expr:smart_ptr<ExprSafeAt>) : ExpressionPtr
    // def override preVisitExprSafeAtIndex(expr:smart_ptr<ExprAt>;index:ExpressionPtr): void
// is
    // def override preVisitExprIs(expr:smart_ptr<ExprIs>): void
    // def override visitExprIs(expr:smart_ptr<ExprIs>) : ExpressionPtr
    // def override preVisitExprIsType(expr:smart_ptr<ExprAt>;typeDecl:TypeDeclPtr): void
// op2
    def override preVisitExprOp2(expr:smart_ptr<ExprOp2>): void
        if skipExprs || expr == null
            return
        unsafe
            var name = "{expr.op}"
            var desc = name
            var call & = reinterpret<smart_ptr<ExprCallFunc>> expr
            desc = call != null && call.func != null ? describe(reinterpret<smart_ptr<Function>> call.func) : ""
            var token <- new [[AstToken
                name = name,
                desc = desc,
                kind = AstTokenKind Operator,
                _type = desc
            ]]
            set_token_path(token, expr.at)
            if call.func != null
                set_token_def(token, call.func.atDecl)
            *tokens |> emplace(token)
    // def override visitExprOp2(expr:smart_ptr<ExprOp2>) : ExpressionPtr
    // def override preVisitExprOp2Right(expr:smart_ptr<ExprOp2>;right:ExpressionPtr): void
// op3
    // def override preVisitExprOp3(expr:smart_ptr<ExprOp3>): void
    // def override visitExprOp3(expr:smart_ptr<ExprOp3>) : ExpressionPtr
    // def override preVisitExprOp3Left(expr:smart_ptr<ExprOp3>;left:ExpressionPtr): void
    // def override preVisitExprOp3Right(expr:smart_ptr<ExprOp3>;right:ExpressionPtr): void
// copy
    // def override preVisitExprCopy(expr:smart_ptr<ExprCopy>): void
    // def override visitExprCopy(expr:smart_ptr<ExprCopy>) : ExpressionPtr
    // def override preVisitExprCopyRight(expr:smart_ptr<ExprCopy>;right:ExpressionPtr): void
// move
    // def override preVisitExprMove(expr:smart_ptr<ExprMove>): void
    // def override visitExprMove(expr:smart_ptr<ExprMove>) : ExpressionPtr
    // def override preVisitExprMoveRight(expr:smart_ptr<ExprMove>;right:ExpressionPtr) : void
// clone
    // def override preVisitExprClone(expr:smart_ptr<ExprClone>) : void
    // def override visitExprClone(expr:smart_ptr<ExprClone>) : ExpressionPtr
    // def override preVisitExprCloneRight(expr:smart_ptr<ExprClone>;right:ExpressionPtr) : void
// with
    // def override preVisitExprWith(expr:smart_ptr<ExprWith>) : void
    // def override visitExprWith(expr:smart_ptr<ExprWith>) : ExpressionPtr
    // def override preVisitExprWithBody(expr:smart_ptr<ExprWith>;right:ExpressionPtr) : void
// while
    // def override preVisitExprWhile(expr:smart_ptr<ExprWhile>) : void
    // def override visitExprWhile(expr:smart_ptr<ExprWhile>) : ExpressionPtr
    // def override preVisitExprWhileBody(expr:smart_ptr<ExprWhile>;right:ExpressionPtr) : void
// try-catch
    // def override preVisitExprTryCatch(expr:smart_ptr<ExprTryCatch>) : void
    // def override visitExprTryCatch(expr:smart_ptr<ExprTryCatch>) : ExpressionPtr
    // def override preVisitExprTryCatchCatch(expr:smart_ptr<ExprTryCatch>;right:ExpressionPtr) : void
// if-then-else
    // def override preVisitExprIfThenElse(expr:smart_ptr<ExprIfThenElse>) : void
    // def override visitExprIfThenElse(expr:smart_ptr<ExprIfThenElse>) : ExpressionPtr
    // def override preVisitExprIfThenElseIfBlock(expr:smart_ptr<ExprIfThenElse>;ifBlock:ExpressionPtr) : void
    // def override preVisitExprIfThenElseElseBlock(expr:smart_ptr<ExprIfThenElse>;elseBlock:ExpressionPtr) : void
// for
    // def override preVisitExprFor(expr:smart_ptr<ExprFor>) : void
    // def override visitExprFor(expr:smart_ptr<ExprFor>) : ExpressionPtr
    def override preVisitExprForVariable(expr:smart_ptr<ExprFor>;svar:VariablePtr;last:bool) : void
        if skipExprs || expr == null
            return
        var_to_token(svar) <| $(var token : AstToken?)
            *tokens |> emplace(token)
    // def override visitExprForVariable(expr:smart_ptr<ExprFor>;svar:VariablePtr;last:bool) : VariablePtr
    // def override preVisitExprForSource(expr:smart_ptr<ExprFor>;source:ExpressionPtr;last:bool) : void
    // def override visitExprForSource(expr:smart_ptr<ExprFor>;source:ExpressionPtr;last:bool) : ExpressionPtr
    // def override preVisitExprForStack(expr:smart_ptr<ExprFor>) : void
    // def override preVisitExprForBody(expr:smart_ptr<ExprFor>) : void
// make variant
    // def override preVisitExprMakeVariant(expr:smart_ptr<ExprMakeVariant>) : void
    // def override visitExprMakeVariant(expr:smart_ptr<ExprMakeVariant>) : ExpressionPtr
    // def override preVisitExprMakeVariantField(expr:smart_ptr<ExprMakeVariant>;index:int;decl:MakeFieldDeclPtr;last:bool) : void
    // def override visitExprMakeVariantField(expr:smart_ptr<ExprMakeVariant>;index:int;decl:MakeFieldDeclPtr;last:bool) : MakeFieldDeclPtr
// make structure
    // def override preVisitExprMakeStruct(expr:smart_ptr<ExprMakeStruct>) : void
    // def override visitExprMakeStruct(expr:smart_ptr<ExprMakeStruct>) : ExpressionPtr
    // def override preVisitExprMakeStructIndex(expr:smart_ptr<ExprMakeStruct>;index:int;last:bool) : void
    // def override visitExprMakeStructIndex(expr:smart_ptr<ExprMakeStruct>;index:int;last:bool) : void
    def override preVisitExprMakeStructField(expr:smart_ptr<ExprMakeStruct>;index:int;decl:MakeFieldDeclPtr;last:bool) : void
        if skipExprs || expr == null || decl == null || expr.genFlags.generated
            return
        var typ = decl.value != null && decl.value._type != null ? describe(decl.value._type) : ""
        var name = "{decl.name}"
        if expr._type != null
            name = "{describe(expr._type)}.{name}"
        var token <- new [[AstToken
            name = name,
            desc = typ != "" ? "{name} : {typ}" : name,
            kind = AstTokenKind Field,
            _type = typ
        ]]
        set_token_path(token, decl.at)
        if expr._type != null
            if expr._type.structType != null
                set_token_def(token, expr._type.structType.at)
        // if find(name, "finalize") >= 0
            // info(token, get_line_info())
            // info(decl.flags, get_line_info())
            // info(expr.flags, get_line_info())
            // info(expr.genFlags, get_line_info())
        *tokens |> emplace(token)
    // def override visitExprMakeStructField(expr:smart_ptr<ExprMakeStruct>;index:int;decl:MakeFieldDeclPtr;last:bool) : MakeFieldDeclPtr
// make array
    // def override preVisitExprMakeArray(expr:smart_ptr<ExprMakeArray>) : void
    // def override visitExprMakeArray(expr:smart_ptr<ExprMakeArray>) : ExpressionPtr
    // def override preVisitExprMakeArrayIndex(expr:smart_ptr<ExprMakeArray>;index:int;init:ExpressionPtr;last:bool) : void
    // def override visitExprMakeArrayIndex(expr:smart_ptr<ExprMakeArray>;index:int;init:ExpressionPtr;last:bool) : ExpressionPtr
// make tuple
    // def override preVisitExprMakeTuple(expr:smart_ptr<ExprMakeTuple>) : void
    // def override visitExprMakeTuple(expr:smart_ptr<ExprMakeTuple>) : ExpressionPtr
    // def override preVisitExprMakeTupleIndex(expr:smart_ptr<ExprMakeTuple>;index:int;init:ExpressionPtr;last:bool) : void
    // def override visitExprMakeTupleIndex(expr:smart_ptr<ExprMakeTuple>;index:int;init:ExpressionPtr;last:bool) : ExpressionPtr
// array comprehension
    // def override preVisitExprArrayComprehension(expr:smart_ptr<ExprArrayComprehension>) : void
    // def override visitExprArrayComprehension(expr:smart_ptr<ExprArrayComprehension>) : ExpressionPtr
    // def override preVisitExprArrayComprehensionSubexpr(expr:smart_ptr<ExprArrayComprehension>;subexrp:ExpressionPtr) : void
    // def override preVisitExprArrayComprehensionWhere(expr:smart_ptr<ExprArrayComprehension>;filter:ExpressionPtr) : void
// type info
    // def override preVisitExprTypeInfo(expr:smart_ptr<ExprTypeInfo>) : void
    // def override visitExprTypeInfo(expr:smart_ptr<ExprTypeInfo>) : ExpressionPtr
// ptr to ref
    def override preVisitExprPtr2Ref(expr:smart_ptr<ExprPtr2Ref>) : void
        if skipExprs || expr == null
            return
        var name = "deref"
        var desc = "{name}("
        var typ = expr._type != null ? describe(expr._type) : ""
        if expr.subexpr != null && expr.subexpr._type != null
            desc += "arg1 : {describe(expr.subexpr._type)}"
        desc += ")"
        if typ != ""
            desc += " : {typ}"
        var token <- new [[AstToken
            name = name,
            desc = desc,
            kind = AstTokenKind Operator,
            _type = typ
        ]]
        set_token_path(token, expr.at)
        if expr._type != null
            if expr._type.structType != null
                set_token_def(token, expr._type.structType.at)
        *tokens |> emplace(token)
    // def override visitExprPtr2Ref(expr:smart_ptr<ExprPtr2Ref>) : ExpressionPtr
// label
    // def override preVisitExprLabel(expr:smart_ptr<ExprLabel>) : void
    // def override visitExprLabel(expr:smart_ptr<ExprLabel>) : ExpressionPtr
// goto
    // def override preVisitExprGoto(expr:smart_ptr<ExprGoto>) : void
    // def override visitExprGoto(expr:smart_ptr<ExprGoto>) : ExpressionPtr
// ref to value
    // def override preVisitExprRef2Value(expr:smart_ptr<ExprRef2Value>) : void
    // def override visitExprRef2Value(expr:smart_ptr<ExprRef2Value>) : ExpressionPtr
// ref to ptr
    // def override preVisitExprRef2Ptr(expr:smart_ptr<ExprRef2Ptr>) : void
    // def override visitExprRef2Ptr(expr:smart_ptr<ExprRef2Ptr>) : ExpressionPtr
// @@
    // def override preVisitExprAddr(expr:smart_ptr<ExprAddr>) : void
    // def override visitExprAddr(expr:smart_ptr<ExprAddr>) : ExpressionPtr
// assert / verify
    // def override preVisitExprAssert(expr:smart_ptr<ExprAssert>) : void
    // def override visitExprAssert(expr:smart_ptr<ExprAssert>) : ExpressionPtr
// static_assert
    // def override preVisitExprStaticAssert(expr:smart_ptr<ExprStaticAssert>) : void
    // def override visitExprStaticAssert(expr:smart_ptr<ExprStaticAssert>) : ExpressionPtr
// debug
    // def override preVisitExprDebug(expr:smart_ptr<ExprDebug>) : void
    // def override visitExprDebug(expr:smart_ptr<ExprDebug>) : ExpressionPtr
// invoke
    // def override preVisitExprInvoke(expr:smart_ptr<ExprInvoke>) : void
    // def override visitExprInvoke(expr:smart_ptr<ExprInvoke>) : ExpressionPtr
// erase
    // def override preVisitExprErase(expr:smart_ptr<ExprErase>) : void
    // def override visitExprErase(expr:smart_ptr<ExprErase>) : ExpressionPtr
// find
    // def override preVisitExprFind(expr:smart_ptr<ExprFind>) : void
    // def override visitExprFind(expr:smart_ptr<ExprFind>) : ExpressionPtr
// key exists
    // def override preVisitExprKeyExists(expr:smart_ptr<ExprKeyExists>) : void
    // def override visitExprKeyExists(expr:smart_ptr<ExprKeyExists>) : ExpressionPtr
// ascend
    // def override preVisitExprAscend(expr:smart_ptr<ExprAscend>) : void
    // def override visitExprAscend(expr:smart_ptr<ExprAscend>) : ExpressionPtr
// cast
    // def override preVisitExprCast(expr:smart_ptr<ExprCast>) : void
    // def override visitExprCast(expr:smart_ptr<ExprCast>) : ExpressionPtr
// delete
    // def override preVisitExprDelete(expr:smart_ptr<ExprDelete>) : void
    // def override visitExprDelete(expr:smart_ptr<ExprDelete>) : ExpressionPtr
// var
    def override preVisitExprVar(expr:smart_ptr<ExprVar>) : void
        if skipExprs || expr == null
            return
        var_to_token(expr.variable) <| $(var token)
            token.defPath = token.path
            token.defRange = token._range
            set_token_path(token, expr.at)
            *tokens |> emplace(token)
    // def override visitExprVar(expr:smart_ptr<ExprVar>) : ExpressionPtr
// field
    def override preVisitExprField(expr:smart_ptr<ExprField>) : void
        if skipExprs || expr == null
            return
        field_to_token(expr) <| $(var token)
            *tokens |> emplace(token)
    // def override visitExprField(expr:smart_ptr<ExprField>) : ExpressionPtr
// safe field
    def override preVisitExprSafeField(expr:smart_ptr<ExprSafeField>) : void
        if skipExprs
            return
        field_to_token(expr) <| $(var token)
            *tokens |> emplace(token)
    // def override visitExprSafeField(expr:smart_ptr<ExprSafeField>) : ExpressionPtr
// swizzle
    def override preVisitExprSwizzle(expr:smart_ptr<ExprSwizzle>) : void
        if skipExprs || expr == null
            return
        var swizzle = ""
        let f2name = [[string "x";"y";"z";"w"]]
        for fch in expr.fields
            let f = int(fch)
            swizzle += "{f>=0 && f<=3 ? f2name[f] : "?"}"
        let typ = "{describe([decl=expr.value._type,extra=true])}.{swizzle}"
        let name = "{describe(expr.value)}.{swizzle}"
        var token <- new [[AstToken
            name = name,
            desc = "{name} : {typ}",
            kind = AstTokenKind Variable,
            _type = "{typ}"
        ]]
        set_token_path(token, expr.value != null ? expr.value.at : expr.at)
        token._range.start.character = token._range.end.character + 1
        token._range.end.character += length(swizzle) + 1
        *tokens |> emplace(token)
    // def override visitExprSwizzle(expr:smart_ptr<ExprSwizzle>) : ExpressionPtr
// is variant
    // def override preVisitExprIsVariant(expr:smart_ptr<ExprIsVariant>) : void
    // def override visitExprIsVariant(expr:smart_ptr<ExprIsVariant>) : ExpressionPtr
// as variant
    // def override preVisitExprAsVariant(expr:smart_ptr<ExprAsVariant>) : void
    // def override visitExprAsVariant(expr:smart_ptr<ExprAsVariant>) : ExpressionPtr
// safe as variant
    // def override preVisitExprSafeAsVariant(expr:smart_ptr<ExprSafeAsVariant>) : void
    // def override visitExprSafeAsVariant(expr:smart_ptr<ExprSafeAsVariant>) : ExpressionPtr
// op1
    // def override preVisitExprOp1(expr:smart_ptr<ExprOp1>) : void
    // def override visitExprOp1(expr:smart_ptr<ExprOp1>) : ExpressionPtr
// return
    // def override preVisitExprReturn(expr:smart_ptr<ExprReturn>) : void
    // def override visitExprReturn(expr:smart_ptr<ExprReturn>) : ExpressionPtr
// yield
    // def override preVisitExprYield(expr:smart_ptr<ExprYield>) : void
    // def override visitExprYield(expr:smart_ptr<ExprYield>) : ExpressionPtr
// break
    // def override preVisitExprBreak(expr:smart_ptr<ExprBreak>) : void
    // def override visitExprBreak(expr:smart_ptr<ExprBreak>) : ExpressionPtr
// continue
    // def override preVisitExprContinue(expr:smart_ptr<ExprContinue>) : void
    // def override visitExprContinue(expr:smart_ptr<ExprContinue>) : ExpressionPtr
// make block
    // def override preVisitExprMakeBlock(expr:smart_ptr<ExprMakeBlock>) : void
    // def override visitExprMakeBlock(expr:smart_ptr<ExprMakeBlock>) : ExpressionPtr
// make generator
    // def override preVisitExprMakeGenerator(expr:smart_ptr<ExprMakeGenerator>) : void
    // def override visitExprMakeGenerator(expr:smart_ptr<ExprMakeGenerator>) : ExpressionPtr
// memzero
    // def override preVisitExprMemZero(expr:smart_ptr<ExprMemZero>) : void
    // def override visitExprMemZero(expr:smart_ptr<ExprMemZero>) : ExpressionPtr
// const
    def override preVisitExprConst(expr:smart_ptr<ExprConst>) : void
        if skipExprs || expr == null
            return
        if expr.at.line == 0u && expr.at.column == 0u
            return
        let typ = expr._type != null ? describe([decl=expr._type,extra=true]) : ""
        var name = describe(ExpressionPtr(expr))
        var token <- new [[AstToken
            name = name,
            desc = "constant : {typ} = {name}",
            kind = AstTokenKind Constant,
            _type = "{typ}"
        ]]
        set_token_path(token, expr.at)
        *tokens |> emplace(token)
    // def override visitExprConst(expr:smart_ptr<ExprConst>) : ExpressionPtr
// const ptr
    // def override preVisitExprConstPtr(expr:smart_ptr<ExprConstPtr>) : void
    // def override visitExprConstPtr(expr:smart_ptr<ExprConstPtr>) : ExpressionPtr
// const enumeration
    def override preVisitExprConstEnumeration(expr:smart_ptr<ExprConstEnumeration>) : void
        if skipExprs || expr == null
            return
        let typ = expr._type != null ? describe(expr._type, false, false, false) : ""
        var name = "{expr.value}"
        var token <- new [[AstToken
            name = name,
            desc = "{typ} {name}",
            kind = AstTokenKind EnumMember,
            _type = "{typ}"
        ]]
        set_token_path(token, expr.at)
        *tokens |> emplace(token)

    // def override visitExprConstEnumeration(expr:smart_ptr<ExprConstEnumeration>) : ExpressionPtr
// cont bitfield
    // def override preVisitExprConstBitfield(expr:smart_ptr<ExprConstBitfield>) : void
    // def override visitExprConstBitfield(expr:smart_ptr<ExprConstBitfield>) : ExpressionPtr
// const int 8
    // def override preVisitExprConstInt8(expr:smart_ptr<ExprConstInt8>) : void
    // def override visitExprConstInt8(expr:smart_ptr<ExprConstInt8>) : ExpressionPtr
// const int 16
    // def override preVisitExprConstInt16(expr:smart_ptr<ExprConstInt16>) : void
    // def override visitExprConstInt16(expr:smart_ptr<ExprConstInt16>) : ExpressionPtr
// const int 64
    // def override preVisitExprConstInt64(expr:smart_ptr<ExprConstInt64>) : void
    // def override visitExprConstInt64(expr:smart_ptr<ExprConstInt64>) : ExpressionPtr
// const int
    // def override preVisitExprConstInt(expr:smart_ptr<ExprConstInt>) : void
    // def override visitExprConstInt(expr:smart_ptr<ExprConstInt>) : ExpressionPtr
// const int2
    // def override preVisitExprConstInt2(expr:smart_ptr<ExprConstInt2>) : void
    // def override visitExprConstInt2(expr:smart_ptr<ExprConstInt2>) : ExpressionPtr
// const int3
    // def override preVisitExprConstInt3(expr:smart_ptr<ExprConstInt3>) : void
    // def override visitExprConstInt3(expr:smart_ptr<ExprConstInt3>) : ExpressionPtr
// const int4
    // def override preVisitExprConstInt4(expr:smart_ptr<ExprConstInt4>) : void
    // def override visitExprConstInt4(expr:smart_ptr<ExprConstInt4>) : ExpressionPtr
// const uint 8
    // def override preVisitExprConstUInt8(expr:smart_ptr<ExprConstUInt8>) : void
    // def override visitExprConstUInt8(expr:smart_ptr<ExprConstUInt8>) : ExpressionPtr
// const uint 16
    // def override preVisitExprConstUInt16(expr:smart_ptr<ExprConstUInt16>) : void
    // def override visitExprConstUInt16(expr:smart_ptr<ExprConstUInt16>) : ExpressionPtr
// const uint 64
    // def override preVisitExprConstUInt64(expr:smart_ptr<ExprConstUInt64>) : void
    // def override visitExprConstUInt64(expr:smart_ptr<ExprConstUInt64>) : ExpressionPtr
// const uint
    // def override preVisitExprConstUInt(expr:smart_ptr<ExprConstUInt>) : void
    // def override visitExprConstUInt(expr:smart_ptr<ExprConstUInt>) : ExpressionPtr
// const uint2
    // def override preVisitExprConstUInt2(expr:smart_ptr<ExprConstUInt2>) : void
    // def override visitExprConstUInt2(expr:smart_ptr<ExprConstUInt2>) : ExpressionPtr
// const uint3
    // def override preVisitExprConstUInt3(expr:smart_ptr<ExprConstUInt3>) : void
    // def override visitExprConstUInt3(expr:smart_ptr<ExprConstUInt3>) : ExpressionPtr
// const uint4
    // def override preVisitExprConstUInt4(expr:smart_ptr<ExprConstUInt4>) : void
    // def override visitExprConstUInt4(expr:smart_ptr<ExprConstUInt4>) : ExpressionPtr
// const range
    // def override preVisitExprConstRange(expr:smart_ptr<ExprConstRange>) : void
    // def override visitExprConstRange(expr:smart_ptr<ExprConstRange>) : ExpressionPtr
// const urnage
    // def override preVisitExprConstURange(expr:smart_ptr<ExprConstURange>) : void
    // def override visitExprConstURange(expr:smart_ptr<ExprConstURange>) : ExpressionPtr
// const bool
    // def override preVisitExprConstBool(expr:smart_ptr<ExprConstBool>) : void
    // def override visitExprConstBool(expr:smart_ptr<ExprConstBool>) : ExpressionPtr
// const float
    // def override preVisitExprConstFloat(expr:smart_ptr<ExprConstFloat>) : void
    // def override visitExprConstFloat(expr:smart_ptr<ExprConstFloat>) : ExpressionPtr
// const float2
    // def override preVisitExprConstFloat2(expr:smart_ptr<ExprConstFloat2>) : void
    // def override visitExprConstFloat2(expr:smart_ptr<ExprConstFloat2>) : ExpressionPtr
// const float3
    // def override preVisitExprConstFloat3(expr:smart_ptr<ExprConstFloat3>) : void
    // def override visitExprConstFloat3(expr:smart_ptr<ExprConstFloat3>) : ExpressionPtr
// const float4
    // def override preVisitExprConstFloat4(expr:smart_ptr<ExprConstFloat4>) : void
    // def override visitExprConstFloat4(expr:smart_ptr<ExprConstFloat4>) : ExpressionPtr
// const string
    // def override preVisitExprConstString(expr:smart_ptr<ExprConstString>) : void
    // def override visitExprConstString(expr:smart_ptr<ExprConstString>) : ExpressionPtr
// const double
    // def override preVisitExprConstDouble(expr:smart_ptr<ExprConstDouble>) : void
    // def override visitExprConstDouble(expr:smart_ptr<ExprConstDouble>) : ExpressionPtr
// fake context
    // def override preVisitExprFakeContext(expr:smart_ptr<ExprFakeContext>) : void
    // def override visitExprFakeContext(expr:smart_ptr<ExprFakeContext>) : ExpressionPtr
// fake line info
    // def override preVisitExprFakeLineInfo(expr:smart_ptr<ExprFakeLineInfo>) : void
    // def override visitExprFakeLineInfo(expr:smart_ptr<ExprFakeLineInfo>) : ExpressionPtr
