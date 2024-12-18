options indenting = 4
options no_global_variables
options no_unused_function_arguments
options no_unused_block_arguments
options strict_smart_pointers
options no_aot

require ast
require rtti
require math
require strings
require daslib/strings_boost


struct CompletionAt
    file : string
    line : int
    column : int
    lineEnd : int
    columnEnd : int


struct CompletionResult
    enums : array<CompletionEnum>
    structs : array<CompletionStruct>
    typeDecls : array<CompletionTypeDecl>
    typeDefs : array<CompletionTypeDef> // aliases, typedef Foo = int
    globals : array<CompletionGlobal>
    functions : array<CompletionFunction>


struct CompletionContext
    typeDecls : table<string; int>
    result : CompletionResult

def is_builtin(mod : rtti::Module?) : bool
    return mod.moduleFlags.builtIn && !mod.moduleFlags.promoted

def to_json(val)
    return val |> sprint_json(/*readable*/true)
    // return JV(val) |> write_json()

def fill_at(var res; at : LineInfo?)
    if at != null
        fill_at(res, at)

def fill_at(var res : CompletionAt&; at : LineInfo)
    res.file = at.fileInfo != null ? "{at.fileInfo.name}" : ""
    res.line = at.line |> int()
    res.column = at.column |> int()
    res.lineEnd = at.last_line |> int()
    res.columnEnd = at.last_column |> int()


def BaseTypeString(t : rtti::Type) : string
    return "none" if t == Type none
    return "auto" if t == Type autoinfer
    return "alias" if t == Type alias
    return "option" if t == Type option
    return "<context>" if t == Type fakeContext
    return "<line info>" if t == Type fakeLineInfo
    return "any" if t == Type anyArgument
    return "void" if t == Type tVoid
    return "bool" if t == Type tBool
    return "int8" if t == Type tInt8
    return "int16" if t == Type tInt16
    return "uint8" if t == Type tUInt8
    return "uint16" if t == Type tUInt16
    return "int64" if t == Type tInt64
    return "uint64" if t == Type tUInt64
    return "int" if t == Type tInt
    return "int2" if t == Type tInt2
    return "int3" if t == Type tInt3
    return "int4" if t == Type tInt4
    return "uint" if t == Type tUInt
    return "uint2" if t == Type tUInt2
    return "uint3" if t == Type tUInt3
    return "uint4" if t == Type tUInt4
    return "float" if t == Type tFloat
    return "float2" if t == Type tFloat2
    return "float3" if t == Type tFloat3
    return "float4" if t == Type tFloat4
    return "double" if t == Type tDouble
    return "range" if t == Type tRange
    return "urange" if t == Type tURange
    return "range64" if t == Type tRange64
    return "urange64" if t == Type tURange64
    return "string" if t == Type tString
    return "structure" if t == Type tStructure
    return "handle" if t == Type tHandle
    return "enum" if t == Type tEnumeration
    return "enum8" if t == Type tEnumeration8
    return "enum16" if t == Type tEnumeration16
    return "enum64" if t == Type tEnumeration64
    return "bitfield" if t == Type tBitfield
    return "pointer" if t == Type tPointer
    return "function" if t == Type tFunction
    return "lambda" if t == Type tLambda
    return "iterator" if t == Type tIterator
    return "array" if t == Type tArray
    return "table" if t == Type tTable
    return "block" if t == Type tBlock
    return "tuple" if t == Type tTuple
    return "variant" if t == Type tVariant
    panic("unknown type: {t}")
    return "{t}"

/// enum

struct CompletionEnumValue : CompletionAt
    name : string
    value : string

struct CompletionEnum : CompletionAt
    name : string
    mod : string
    cpp : string
    tdk : string
    baseType : string
    values : array<CompletionEnumValue>

def parse_enum(var res : CompletionContext; e : smart_ptr<Enumeration>&)
    var en <- [[CompletionEnum() name="{e.name}", cpp="{e.cppName}", baseType=BaseTypeString(e.baseType)]]
    en |> fill_at(e.at)
    if e._module != null && length(e._module.name) > 0
        en.mod = e._module.name |> string()
        en.tdk = "{en.mod}::{en.name}"
    else
        en.tdk = "{en.name}"
    for ef in e.list
        var ev <- [[CompletionEnumValue() name="{ef.name}"]]
        ev |> fill_at(ef.at)
        ev.value = ef.value |> describe()
        en.values |> emplace(ev)
    res.result.enums |> emplace(en)

/// global

struct CompletionGlobal : CompletionAt
    name : string
    tdk : string
    value : string
    mod : string
    gen : bool
    isUnused : bool

def parse_global(var res : CompletionContext; g : smart_ptr<ast::Variable>&)
    var gl <- [[CompletionGlobal() name="{g.name}"]]
    gl |> fill_at(g.at)
    if g._module != null
        gl.mod = g._module.name |> string()
    gl.tdk = TypeDeclKey(g._type)
    res |> parse_typedecl(gl.tdk, g._type, /*is_generic*/false)
    gl.gen = g.flags.generated
    if g.init != null
        gl.value = g.init |> describe()
    gl.isUnused = !g.flags.marked_used && g.isAccessUnused
    res.result.globals |> emplace(gl)

/// struct

struct CompletionStructField : CompletionAt
    name : string
    // mn : string
    tdk : string
    offset : int
    isPrivate : bool
    gen : bool

struct CompletionStruct : CompletionAt
    name : string
    parentName : string
    parentMod : string
    mod : string
    fields : array<CompletionStructField>
    isClass : bool
    isLambda : bool
    isMacro : bool
    isGenerator : bool
    gen : bool

def resolve_str(var s : smart_ptr<Structure>&) : Structure?
    return s |> get_ptr()

def resolve_str(var s : Structure?) : Structure?
    return s

def parse_struct(var res : CompletionContext; var s : smart_ptr<Structure>& | Structure?)
    var st <- [[CompletionStruct() name="{s.name}"]]
    st |> fill_at(s.at)
    if s._module != null
        st.mod = s._module.name |> string()
    st.isClass = s.flags.isClass
    st.isLambda = s.flags.isLambda
    st.isMacro = s.flags.macroInterface
    st.isGenerator = s.flags._generator
    st.gen = s.flags.generated
    if s.parent != null
        parse_struct(res, s.parent)
        st.parentName = s.parent.name |> string()
        st.parentMod = s.parent._module != null ? "{s.parent._module.name}" : ""
    var sptr = s |> resolve_str()
    for f in s.fields
        var field <- [[CompletionStructField() name="{f.name}"]]
        field |> fill_at(f.at)
        field.isPrivate = f.flags.privateField
        field.gen = f.flags.generated
        field.offset = f.offset
        field.tdk = TypeDeclKey(f._type)
        res |> parse_typedecl(field.tdk, f._type, /*is_generic*/false)
        // field.mn = f._type |> get_mangled_name()
        st.fields |> emplace(field)
    res.result.structs |> emplace(st)

/// type decl
// TODO: support optional type (split by | )
// TODO: keep full type name as separate field (for preview)
def private cleanupTDK(str : string) : string
    var parts <- str |> split("const?")

    for res in parts
        res = replace_multiple(res, [{auto "&" => ""; " ==const" => ""; " const" => ""; " implicit" => "" }])

    return parts |> join("const?")

def TypeDeclKey(t : smart_ptr<ast::TypeDecl>&) : string
    return t |> describe_typedecl(false, false, true) |> cleanupTDK()

struct CompletionTypeDeclField
    name : string
    tdk : string

struct CompletionTypeDecl : CompletionAt
    baseType : string
    tdk : string
    fields : array<CompletionTypeDeclField>
    dim : array<int>
    alias : string
    sizeOf : int
    alignOf : int
    enumName : string
    structName : string
    mod : string // enum name or struct name
    tdk1 : string
    tdk2 : string
    canCopy : bool
    canMove : bool
    canClone : bool

let skipTypeDescSize = fixed_array(Type none, Type.autoinfer, Type alias, Type.fakeLineInfo,
    Type fakeContext, Type alias, Type.typeDecl, Type.typeMacro, Type.option)

def parse_typedecl(var res : CompletionContext; tdk : string; t : smart_ptr<TypeDecl>&; is_generic : bool) : void
    var foundIdx = res.typeDecls?[tdk] ?? -1
    if foundIdx >= 0
        return
    foundIdx = res.result.typeDecls |> length()
    res.typeDecls |> insert(tdk, foundIdx)
    // if t.argNames |> length() > 0
    var td <- [[CompletionTypeDecl() tdk=tdk, baseType=BaseTypeString(t.baseType)]]
    td.canCopy = t.canCopy
    td.canMove = t.canMove
    td.canClone = t.canClone
    let totalArgs = min(length(t.argNames), length(t.argTypes))
    for idx, argN, argT in count(), t.argNames, t.argTypes
        var tf <- [[CompletionTypeDeclField() name=length(argN) > 0 ? "{argN}" : "_{idx}"]]
        tf.tdk = TypeDeclKey(argT)
        res |> parse_typedecl(tf.tdk, argT, is_generic)
        td.fields |> emplace(tf)
    for idx in totalArgs..length(t.argNames)
        assume argN = t.argNames[idx]
        var tf <- [[CompletionTypeDeclField() name=length(argN) > 0 ? "{argN}" : "_{idx}"]]
        td.fields |> emplace(tf)
    for idx in totalArgs..length(t.argTypes)
        assume argT = t.argTypes[idx]
        var tf <- [[CompletionTypeDeclField() name="_{idx}"]]
        tf.tdk = TypeDeclKey(argT)
        res |> parse_typedecl(tf.tdk, argT, is_generic)
        td.fields |> emplace(tf)
    td |> fill_at(t.at)
    td.alias = t.alias |> string()
    for i in t.dim
        td.dim |> push(i)
    if t._module != null
        td.mod = t._module.name |> string()

    if !is_generic && skipTypeDescSize |> find_index(t.baseType) == -1
        td.sizeOf = t.sizeOf
        td.alignOf = t.alignOf
    if t.enumType != null
        td.enumName = t.enumType.name |> string()
        td.mod = t.enumType._module != null ? t.enumType._module.name |> string() : ""
    if t.structType != null
        td.structName = t.structType.name |> string()
        td.mod = t.structType._module != null ? t.structType._module.name |> string() : ""
    if t.annotation != null
        td.structName = t.annotation.name |> string()
        td.mod = t.annotation._module != null ? t.annotation._module.name |> string() : ""
    if t.firstType != null
        td.tdk1 = TypeDeclKey(t.firstType)
        res |> parse_typedecl(td.tdk1, t.firstType, is_generic)
    elif length(t.dim) > 0
        var inscope sub <- t |> clone_type()
        sub.dim |> pop()
        parse_typedecl(res, TypeDeclKey(sub), sub, is_generic)
        td.tdk1 = TypeDeclKey(sub)
    if t.secondType != null
        td.tdk2 = TypeDeclKey(t.secondType)
        res |> parse_typedecl(td.tdk2, t.secondType, is_generic)
    res.result.typeDecls |> emplace(td)
    // return foundIdx

/// type def

struct CompletionTypeDef : CompletionAt
    name : string
    mod : string
    tdk : string

def parse_typedef(var res : CompletionContext; mod : Module?; n : string#; t : smart_ptr<TypeDecl>&)
    var td <- [[CompletionTypeDef() name="{n}"]]
    td |> fill_at(t.at)
    td.mod = mod.name |> string()
    td.tdk = TypeDeclKey(t)
    res |> parse_typedecl(td.tdk, t, /*is_generic*/false)
    res.result.typeDefs |> emplace(td)

/// function

struct CompletionFuncArg : CompletionAt
    name : string
    alias : string
    variable : bool
    tdk : array<string>
    value : string

struct CompletionFunction : CompletionAt
    name : string
    mod : string
    origMod : string
    cpp : string
    tdk : string
    decl : CompletionAt
    args : array<CompletionFuncArg>
    gen : bool
    isClassMethod : bool
    isGeneric : bool

[nodiscard]
def fix_fn_name(name : string) : string
    if name |> starts_with("__::")
        return name |> slice(4)
    return name

def parse_function(var res : CompletionContext; f : smart_ptr<Function>&; cpp : string; is_generic : bool)
    var fn <- [[CompletionFunction() name="{f.name}", cpp="{cpp}"]]
    fn.name = fix_fn_name(fn.name)
    fn.isGeneric = is_generic
    fn |> fill_at(f.at)
    if f._module != null
        fn.mod = f._module.name |> string()
    var origin = f.origin
    if origin != null && origin._module != null
        fn.origMod = origin._module.name |> string()
    if f.result != null
        fn.tdk = TypeDeclKey(f.result)
        res |> parse_typedecl(fn.tdk, f.result, is_generic)
    fn.decl |> fill_at(f.atDecl)
    fn.gen = f.flags.generated
    fn.isClassMethod = f.flags.isClassMethod
    for arg in f.arguments
        var fa <- [[CompletionFuncArg() name="{arg.name}"]]
        fa.variable = arg._type.flags.removeConstant
        fa |> fill_at(arg.at)
        if arg._type.baseType == Type option
            for argType in arg._type.argTypes
                fa.tdk.push(TypeDeclKey(argType))
                res |> parse_typedecl(fa.tdk.back(), argType, is_generic)
        else
            fa.tdk.push(TypeDeclKey(arg._type))
            res |> parse_typedecl(fa.tdk[0], arg._type, is_generic)
        fa.alias = arg._aka |> string()
        if arg.init != null
            fa.value = arg.init |> describe()
        fn.args |> emplace(fa)
    res.result.functions |> emplace(fn)


def parse_annotation(var res : CompletionContext; ann : Annotation&)
    if ann.isBasicStructureAnnotation
        let sann & = unsafe(reinterpret<BasicStructureAnnotation&>(ann))
        var st <- [[CompletionStruct() name="{sann.name}"]]
        if ann._module != null
            st.mod = ann._module.name |> string()
        for_each_field(sann) <| $ [unused_argument(cppName)] (name, cppName, xtype, offset)
            var field <- [[CompletionStructField() name="{name}"]]
            field.tdk = TypeDeclKey(xtype)
            res |> parse_typedecl(field.tdk, xtype, /*is_generic*/false)
            field.offset = int(offset)
            st.fields |> emplace(field)
        res.result.structs |> emplace(st)

    elif ann.isTypeAnnotation
        let tann & = unsafe(reinterpret<TypeAnnotation&>(ann))
        var td <- [[CompletionStruct() name="{tann.name}"]]
        if ann._module != null
            td.mod = "{ann._module.name}"
        res.result.structs |> emplace(td)

    else
        var td <- [[CompletionStruct() name="{ann.name}"]]
        if ann._module != null
            td.mod = "{ann._module.name}"
        res.result.structs |> emplace(td)

/// module

def parse_module(var res : CompletionContext; mod : rtti::Module?)
    // debug(mod)

    mod |> for_each_enumeration() <| $(e)
        // debug(e)
        res |> parse_enum(e)

    mod |> for_each_global() <| $(g)
        // debug(g)
        res |> parse_global(g)

    mod |> for_each_structure() <| $(s)
        res |> parse_struct(s)

    mod |> for_each_typedef() <| $(n, t)
        // debug(t)
        res |> parse_typedef(mod, n, t)

    var inscope funcCppNames : table<string; string>
    mod |> module_for_each_function() <| $(f)
        funcCppNames |> insert(f.name, clone_string(f.cppName))

    mod |> for_each_generic() <| $(f)
        // debug(f)
        res |> parse_function(f, funcCppNames?[string(f.name)] ?? "", true)

    mod |> for_each_function("") <| $(f)
        // debug(f)
        res |> parse_function(f, funcCppNames?[string(f.name)] ?? "", false)

    mod |> module_for_each_annotation() <| $(a)
        res |> parse_annotation(a)
