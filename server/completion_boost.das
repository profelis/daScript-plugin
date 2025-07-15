options gen2
options rtti
options indenting = 4
options gen2 = false
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


struct CompletionAt {
    @optional file : string
    @optional line : int
    @optional column : int
    @optional lineEnd : int
    @optional columnEnd : int
}


struct CompletionResult {
    enums : array<CompletionEnum>
    structs : array<CompletionStruct>
    typeDecls : array<CompletionTypeDecl>
    typeDefs : array<CompletionTypeDef> // aliases, typedef Foo = int
    globals : array<CompletionGlobal>
    functions : array<CompletionFunction>
}


struct CompletionContext {
    typeDecls : table<uint64>
    result : CompletionResult
}

def is_builtin(mod : rtti::Module?) : bool {
    return mod.moduleFlags.builtIn && !mod.moduleFlags.promoted
}

def to_json(val) {
    return val |> sprint_json(/*readable*/true)
}
    // return JV(val) |> write_json()

def fill_at(var res; at : LineInfo?) {
    if (at != null) {
        fill_at(res, at)
    }
}

def fill_at(var res : CompletionAt&; at : LineInfo) {
    res.file = at.fileInfo != null ? "{at.fileInfo.name}" : ""
    res.line = at.line |> int()
    res.column = at.column |> int()
    res.lineEnd = at.last_line |> int()
    res.columnEnd = at.last_column |> int()
}


def BaseTypeString(t : rtti::Type) : string {
    return "none" if (t == Type.none)
    return "auto" if (t == Type.autoinfer)
    return "alias" if (t == Type.alias)
    return "option" if (t == Type.option)
    return "<context>" if (t == Type.fakeContext)
    return "<line info>" if (t == Type.fakeLineInfo)
    return "any" if (t == Type.anyArgument)
    return "void" if (t == Type.tVoid)
    return "bool" if (t == Type.tBool)
    return "int8" if (t == Type.tInt8)
    return "int16" if (t == Type.tInt16)
    return "uint8" if (t == Type.tUInt8)
    return "uint16" if (t == Type.tUInt16)
    return "int64" if (t == Type.tInt64)
    return "uint64" if (t == Type.tUInt64)
    return "int" if (t == Type.tInt)
    return "int2" if (t == Type.tInt2)
    return "int3" if (t == Type.tInt3)
    return "int4" if (t == Type.tInt4)
    return "uint" if (t == Type.tUInt)
    return "uint2" if (t == Type.tUInt2)
    return "uint3" if (t == Type.tUInt3)
    return "uint4" if (t == Type.tUInt4)
    return "float" if (t == Type.tFloat)
    return "float2" if (t == Type.tFloat2)
    return "float3" if (t == Type.tFloat3)
    return "float4" if (t == Type.tFloat4)
    return "double" if (t == Type.tDouble)
    return "range" if (t == Type.tRange)
    return "urange" if (t == Type.tURange)
    return "range64" if (t == Type.tRange64)
    return "urange64" if (t == Type.tURange64)
    return "string" if (t == Type.tString)
    return "structure" if (t == Type.tStructure)
    return "handle" if (t == Type.tHandle)
    return "enum" if (t == Type.tEnumeration)
    return "enum8" if (t == Type.tEnumeration8)
    return "enum16" if (t == Type.tEnumeration16)
    return "enum64" if (t == Type.tEnumeration64)
    return "bitfield" if (t == Type.tBitfield)
    return "pointer" if (t == Type.tPointer)
    return "function" if (t == Type.tFunction)
    return "lambda" if (t == Type.tLambda)
    return "iterator" if (t == Type.tIterator)
    return "array" if (t == Type.tArray)
    return "table" if (t == Type.tTable)
    return "block" if (t == Type.tBlock)
    return "tuple" if (t == Type.tTuple)
    return "variant" if (t == Type.tVariant)
    panic("unknown type: {t}")
    return "{t}"
}

/// enum

struct CompletionEnumValue : CompletionAt {
    name : string
    @optional value : string
}

struct CompletionEnum : CompletionAt {
    name : string
    @optional mod : string
    @optional cpp : string
    @optional tdk : string
    @optional baseType : string
    @optional values : array<CompletionEnumValue>
}

def parse_enum(var res : CompletionContext; e : smart_ptr<Enumeration>&) {
    var en <- CompletionEnum(name = "{e.name}", cpp = "{e.cppName}", baseType = BaseTypeString(e.baseType))
    en |> fill_at(e.at)
    if (e._module != null && length(e._module.name) > 0) {
        en.mod = e._module.name |> string()
        en.tdk = "{en.mod}::{en.name}"
    } else {
        en.tdk = "{en.name}"
    }
    for (ef in e.list) {
        var ev <- CompletionEnumValue(name = "{ef.name}")
        ev |> fill_at(ef.at)
        ev.value = ef.value |> describe()
        en.values |> emplace(ev)
    }
    res.result.enums |> emplace(en)
}

/// global

struct CompletionGlobal : CompletionAt {
    name : string
    @optional tdk : string
    @optional value : string
    @optional mod : string
    @optional gen : bool
    @optional isUnused : bool
}

def parse_global(var res : CompletionContext; g : smart_ptr<ast::Variable>&) {
    var gl <- CompletionGlobal(name = "{g.name}")
    gl |> fill_at(g.at)
    if (g._module != null) {
        gl.mod = g._module.name |> string()
    }
    gl.tdk = TypeDeclKey(g._type)
    res |> parse_typedecl(gl.tdk, g._type, /*is_generic*/false)
    gl.gen = g.flags.generated
    if (g.init != null) {
        gl.value = g.init |> describe()
    }
    gl.isUnused = !g.flags.marked_used && g.isAccessUnused
    res.result.globals |> emplace(gl)
}

/// struct

struct CompletionStructField : CompletionAt {
    name : string
    // mn : string
    tdk : string
    offset : int
    @optional isPrivate : bool
    @optional gen : bool
}

struct CompletionStruct : CompletionAt {
    name : string
    @optional parentName : string
    @optional parentMod : string
    @optional mod : string
    @optional fields : array<CompletionStructField>
    @optional isClass : bool
    @optional isLambda : bool
    @optional isMacro : bool
    @optional isGenerator : bool
    @optional gen : bool
}

def resolve_str(var s : smart_ptr<Structure>&) : Structure? {
    return s |> get_ptr()
}

def resolve_str(var s : Structure?) : Structure? {
    return s
}

def parse_struct(var res : CompletionContext; var s : smart_ptr<Structure>& | Structure?) {
    var st <- CompletionStruct(name = "{s.name}")
    st |> fill_at(s.at)
    if (s._module != null) {
        st.mod = s._module.name |> string()
    }
    st.isClass = s.flags.isClass
    st.isLambda = s.flags.isLambda
    st.isMacro = s.flags.macroInterface
    st.isGenerator = s.flags._generator
    st.gen = s.flags.generated
    if (s.parent != null) {
        parse_struct(res, s.parent)
        st.parentName = s.parent.name |> string()
        st.parentMod = s.parent._module != null ? "{s.parent._module.name}" : ""
    }
    var sptr = s |> resolve_str()
    for (f in s.fields) {
        var field <- CompletionStructField(name = "{f.name}")
        field |> fill_at(f.at)
        field.isPrivate = f.flags.privateField
        field.gen = f.flags.generated
        field.offset = f.offset
        field.tdk = TypeDeclKey(f._type)
        res |> parse_typedecl(field.tdk, f._type, /*is_generic*/false)
        // field.mn = f._type |> get_mangled_name()
        st.fields |> emplace(field)
    }
    res.result.structs |> emplace(st)
}

/// type decl
// TODO: support optional type (split by | )
// TODO: keep full type name as separate field (for preview)
def private cleanupTDK(str : string) : string {
    var parts <- str |> split("const?")

    for (res in parts) {
        res = replace_multiple(res, [ (" ==const", ""), (" const", ""), (" implicit", ""), (";", ",") ])
    }

    return parts |> join("const?")
}

def TypeDeclKey(t : smart_ptr<ast::TypeDecl>&) : string {
    return t |> describe_typedecl(false, false, true) |> cleanupTDK()
}

struct CompletionTypeDeclField {
    name : string
    tdk : string
}

struct CompletionTypeDecl : CompletionAt {
    baseType : string
    tdk : string
    @optional fields : array<CompletionTypeDeclField>
    @optional dim : array<int>
    @optional alias : string
    sizeOf : int
    alignOf : int
    @optional enumName : string
    @optional structName : string
    @optional mod : string // enum name or struct name
    @optional tdk1 : string
    @optional tdk2 : string
    @optional canCopy : bool
    @optional canMove : bool
    @optional canClone : bool
}

let skipTypeDescSize = fixed_array(Type.none, Type.autoinfer, Type.alias, Type.fakeLineInfo,
    Type.fakeContext, Type.alias, Type.typeDecl, Type.typeMacro, Type.option)

def parse_typedecl(var res : CompletionContext; tdk : string; t : smart_ptr<TypeDecl>&; is_generic : bool) : void {
    let tdkHash = hash(tdk)
    if (res.typeDecls.key_exists(tdkHash)) {
        return
    }
    res.typeDecls.insert(tdkHash)
    let resIdx = res.result.typeDecls |> length()
    res.result.typeDecls |> emplace(CompletionTypeDecl())
    // if t.argNames |> length() > 0
    var td <- CompletionTypeDecl(tdk = tdk, baseType = BaseTypeString(t.baseType))
    td.canCopy = t.canCopy
    td.canMove = t.canMove
    td.canClone = t.canClone
    let totalArgs = min(length(t.argNames), length(t.argTypes))
    for (idx, argN, argT in count(), t.argNames, t.argTypes) {
        var tf <- CompletionTypeDeclField(name = length(argN) > 0 ? "{argN}" : "_{idx}")
        tf.tdk = TypeDeclKey(argT)
        res |> parse_typedecl(tf.tdk, argT, is_generic)
        td.fields |> emplace(tf)
    }
    for (idx in totalArgs..length(t.argNames)) {
        assume argN = t.argNames[idx]
        var tf <- CompletionTypeDeclField(name = length(argN) > 0 ? "{argN}" : "_{idx}")
        td.fields |> emplace(tf)
    }
    for (idx in totalArgs..length(t.argTypes)) {
        assume argT = t.argTypes[idx]
        var tf <- CompletionTypeDeclField(name = "_{idx}")
        tf.tdk = TypeDeclKey(argT)
        res |> parse_typedecl(tf.tdk, argT, is_generic)
        td.fields |> emplace(tf)
    }
    td |> fill_at(t.at)
    td.alias = t.alias |> string()
    for (i in t.dim) {
        td.dim |> push(i)
    }
    if (t._module != null) {
        td.mod = t._module.name |> string()
    }

    if (!is_generic && skipTypeDescSize |> find_index(t.baseType) == -1) {
        td.sizeOf = t.sizeOf
        td.alignOf = t.alignOf
    }
    if (t.enumType != null) {
        td.enumName = t.enumType.name |> string()
        td.mod = t.enumType._module != null ? t.enumType._module.name |> string() : ""
    }
    if (t.structType != null) {
        td.structName = t.structType.name |> string()
        td.mod = t.structType._module != null ? t.structType._module.name |> string() : ""
    }
    if (t.annotation != null) {
        td.structName = t.annotation.name |> string()
        td.mod = t.annotation._module != null ? t.annotation._module.name |> string() : ""
    }
    if (t.firstType != null) {
        td.tdk1 = TypeDeclKey(t.firstType)
        res |> parse_typedecl(td.tdk1, t.firstType, is_generic)
    } elif (length(t.dim) > 0) {
        var inscope sub <- t |> clone_type()
        sub.dim |> pop()
        parse_typedecl(res, TypeDeclKey(sub), sub, is_generic)
        td.tdk1 = TypeDeclKey(sub)
    }
    if (t.secondType != null) {
        td.tdk2 = TypeDeclKey(t.secondType)
        res |> parse_typedecl(td.tdk2, t.secondType, is_generic)
    }
    res.result.typeDecls[resIdx] <- td
}
    // return foundIdx

/// type def

struct CompletionTypeDef : CompletionAt {
    name : string
    mod : string
    tdk : string
}

def parse_typedef(var res : CompletionContext; mod : Module?; n : string#; t : smart_ptr<TypeDecl>&) {
    var td <- CompletionTypeDef(name = "{n}")
    td |> fill_at(t.at)
    td.mod = mod.name |> string()
    td.tdk = TypeDeclKey(t)
    res |> parse_typedecl(td.tdk, t, /*is_generic*/false)
    res.result.typeDefs |> emplace(td)
}

/// function

struct CompletionFuncArg : CompletionAt {
    name : string
    @optional alias : string
    @optional variable : bool
    tdk : array<string>
    @optional value : string
}

struct CompletionFunction : CompletionAt {
    name : string
    @optional mod : string
    @optional origMod : string
    @optional cpp : string
    tdk : string
    decl : CompletionAt
    @optional args : array<CompletionFuncArg>
    @optional gen : bool
    @optional isClassMethod : bool
    @optional isGeneric : bool
}


def remove_fn_generation(name : string) : string {
    var lastAt = -1
    for (ch, idx in name, count()) {
        if (ch == '`') {
            lastAt = idx
        }
    }
    if (lastAt == -1) {
        return name
    }
    for (i in lastAt+1..length(name)) {
        if (!is_number(unsafe(character_uat(name, i)))) {
            return name
        }
    }
    if (name.starts_with("`")) {
        return name.slice(1, lastAt)
    }
    return name.slice(0, lastAt)
}

[nodiscard]
def fix_fn_name(name : string) : string {
    var res = name
    if (res |> starts_with("__::")) {
        res = slice(name, 4)
    }
    if (res |> starts_with("builtin`")) {
        res = slice(res, length("builtin`"))
    }
    return remove_fn_generation(res)
}


def parse_function(var res : CompletionContext; f : smart_ptr<Function>&; cpp : string; is_generic : bool) {
    var fn <- CompletionFunction(name = "{f.name}", cpp = "{cpp}")
    fn.name = fix_fn_name(fn.name)
    fn.isGeneric = is_generic
    fn |> fill_at(f.at)
    if (f._module != null) {
        fn.mod = f._module.name |> string()
    }
    var origin = f.origin
    if (origin != null && origin._module != null) {
        fn.origMod = origin._module.name |> string()
    }
    if (f.result != null) {
        fn.tdk = TypeDeclKey(f.result)
        res |> parse_typedecl(fn.tdk, f.result, is_generic)
    }
    fn.decl |> fill_at(f.atDecl)
    fn.gen = f.flags.generated
    fn.isClassMethod = f.flags.isClassMethod
    for (arg in f.arguments) {
        var fa <- CompletionFuncArg(name = "{arg.name}")
        fa.variable = arg._type.flags.removeConstant
        fa |> fill_at(arg.at)
        if (arg._type.baseType == Type.option) {
            for (argType in arg._type.argTypes) {
                fa.tdk.push(TypeDeclKey(argType))
                res |> parse_typedecl(fa.tdk.back(), argType, is_generic)
            }
        } else {
            fa.tdk.push(TypeDeclKey(arg._type))
            res |> parse_typedecl(fa.tdk[0], arg._type, is_generic)
        }
        fa.alias = arg._aka |> string()
        if (arg.init != null) {
            fa.value = arg.init |> describe()
        }
        fn.args |> emplace(fa)
    }
    res.result.functions |> emplace(fn)
}


def parse_annotation(var res : CompletionContext; ann : Annotation&) {
    if (ann.isBasicStructureAnnotation) {
        let sann & = unsafe(reinterpret<BasicStructureAnnotation&>(ann))
        var st <- CompletionStruct(name = "{sann.name}")
        if (ann._module != null) {
            st.mod = ann._module.name |> string()
        }
        for_each_field(sann) <| $ [unused_argument(cppName)] (name, cppName, xtype, offset) {
            var field <- CompletionStructField(name = "{name}")
            field.tdk = TypeDeclKey(xtype)
            res |> parse_typedecl(field.tdk, xtype, /*is_generic*/false)
            field.offset = int(offset)
            st.fields |> emplace(field)
        }
        res.result.structs |> emplace(st)
    } elif (ann.isTypeAnnotation) {
        let tann & = unsafe(reinterpret<TypeAnnotation&>(ann))
        var td <- CompletionStruct(name = "{tann.name}")
        if (ann._module != null) {
            td.mod = "{ann._module.name}"
        }
        res.result.structs |> emplace(td)
    } else {
        var td <- CompletionStruct(name = "{ann.name}")
        if (ann._module != null) {
            td.mod = "{ann._module.name}"
        }
        res.result.structs |> emplace(td)
    }
}

/// module

def parse_module(var res : CompletionContext; mod : rtti::Module?) {
    // debug(mod)

    mod |> for_each_enumeration() <| $(e) {
        // debug(e)
        res |> parse_enum(e)
    }

    mod |> for_each_global() <| $(g) {
        // debug(g)
        res |> parse_global(g)
    }

    mod |> for_each_structure() <| $(s) {
        res |> parse_struct(s)
    }

    mod |> for_each_typedef() <| $(n, t) {
        // debug(t)
        res |> parse_typedef(mod, n, t)
    }

    var inscope funcCppNames : table<string; string>
    mod |> module_for_each_function() <| $(f) {
        funcCppNames |> insert(f.name, clone_string(f.cppName))
    }

    mod |> for_each_generic() <| $(f) {
        // debug(f)
        res |> parse_function(f, funcCppNames?[string(f.name)] ?? "", true)
    }

    mod |> for_each_function("") <| $(f) {
        // debug(f)
        res |> parse_function(f, funcCppNames?[string(f.name)] ?? "", false)
    }

    mod |> module_for_each_annotation() <| $(a) {
        res |> parse_annotation(a)
    }
}
