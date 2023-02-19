module completion shared

options no_aot = true
options indenting = 4

options rtti = true
require rtti
require daslib/json
require daslib/defer
require strings
require daslib/strings_boost
require lsp_types
require lib
require dapLogger
require config

[skip_field_lock_check]
struct Completion
    name : string              // label
    desc : string              // detail
    fullName : string          //
    impls : array<string>      // documentation
    kind : CompletionItemKind
    funcFirstArg : array<string>

def disableLocks(var it : Completion)
    unsafe
        disableLocks(it.funcFirstArg)
        disableLocks(it.impls)

typedef
    Completions = table<string; Completion>
    MetaItem = tuple< string; array<string> >

let
    // src\parser\ds_parser.ypp:129
    keywords = [[string "struct"; "class"; "let"; "def"; "while"; "if"; "static_if"; "else"; "for"; "recover"; "true"; "false"; "new";
                        "typeinfo"; "type"; "in"; "is"; "as"; "elif"; "static_elif"; "array"; "return"; "null"; "break"; "try"; "options";
                        "table"; "expect"; "const"; "require"; "operator"; "enum"; "finally"; "delete"; "deref"; "typedef"; "with";
                        "cast"; "override"; "abstract"; "upcast"; "iterator"; "var"; "addr"; "continue"; "where"; "pass"; "reinterpret";
                        "module"; "public"; "label"; "goto"; "implicit"; "explicit"; "shared"; "private"; "smart_ptr"; "unsafe"]]

    // src\parser\ds_parser.ypp:186
    basicTypes = [[string "bool"; "void"; "string"; "auto"; "int"; "int2"; "int3"; "int4"; "uint"; "bitfield"; "uint2"; "uint3"; "uint4";
                          "float"; "float2"; "float3"; "float4"; "range"; "urange"; "block"; "int64"; "uint64"; "double"; "function";
                          "lambda"; "int8"; "uint8"; "int16"; "uint16"; "tuple"; "variant"; "generator"; "yield"]]

    metaFunc <- [{ MetaItem[]
                    "assert", [{ auto[] "def assert(cond)"; "def assert(cond, string)" }];
                    "verify", [{ auto[] "def verify(cond)"; "def verify(cond, string)" }];
                    "static_assert", [{ auto[] "def static_assert(cond)"; "def static_assert(cond, string)" }];
                    "concept_assert", [{ auto[] "def concept_assert(cond)"; "def concept_assert(cond, string)" }];
                    "debug", [{ auto[] "def debug(value)"; "def debug(value, string)" }];
                    "memzero", [{ auto[] "def memzero(ref)" }];
                    "invoke", [{ auto[] "def invoke(block_or_function_or_lambda)"; "def invoke(block_or_function_or_lambda, ...)" }]
                }]

    // src\ast\ast_lint.cpp:426
    langOptions = [[auto "lint"; "only_fast_aot"; "aot_order_side_effects"; "no_global_heap"; "no_global_variables"; "no_global_variables_at_all"; "no_unused_function_arguments";
                         "no_unused_block_arguments"; "stack"; "intern_strings"; "multiple_contexts"; "persistent_heap"; "persistent_string_heap";
                         "heap_size_hint"; "string_heap_size_hint"; "gc"; "no_aot"; "aot_prologue";
                         "log"; "log_optimization_passes"; "log_stack"; "log_var_scope"; "log_nodes"; "log_nodes_aot_hash"; "log_mem"; "log_cpp"; "log_aot"; "log_infer_passes";
                         "log_require"; "log_compile_time"; "log_total_compile_time"; "log_generics"; "log_mn_hash"; "log_gmn_hash"; "log_ad_hash"; "print_ref"; "print_var_access";
                         "print_c_style"; "rtti"; "optimize"; "fusion"; "remove_unused_symbols"; "always_export_initializer"; "infer_time_folding"; "disable_run";
                         "max_infer_passes"; "indenting"; "debugger"]]

    // src\parser\ds_parser.ypp: 893
    operators <- {{ "!"; "~"; "+="; "-="; "*="; "/="; "%="; "&="; "|="; "^="; "&&="; "||="; "^^="; "&&"; "||"; "^^"; "+"; "-";
                         "*"; "/"; "%"; "<"; ">"; "=="; "!="; "<="; ">="; "&"; "|"; "^"; "++"; "--"; "+++"; "---"; "<<"; ">>"; "<<=";
                         ">>="; "<<<"; ">>>"; "<<<="; ">>>="; "[]"; "?[]"; "."; "?."; "clone"; "finalize"; "??"; "`is"; "`as"; "?as";
                         ":="; "<-"; "safe``operator`qmark`qmark" }}


def private addItem(var item : Completion; var items : Completions; once = false)
    var found = false
    let key = "{item.desc}{int(item.kind)}"
    items |> get(key) <| $(it)
        found = true
        if !once
            for impl in item.impls
                it.impls |> emplace(impl)
            for arg in item.funcFirstArg
                it.funcFirstArg |> emplace(arg)
        delete item
    if !found
        disableLocks(item)
        items[key] <- item


def private module_name(mod)
    return mod == null || length(mod.name) == 0 ? "" : "{mod.name}::"


def private getTypeName(info; var items : Completions)
    return build_string() <| $(var res)
        if info.basicType == Type tHandle
            res |> write(info.annotation != null ? "{module_name(info.annotation._module)}{info.annotation.name}" : "tHandle")
        elif info.basicType == Type tStructure
            res |> write(info.structType?.name ?? "structure")
        elif info.basicType == Type tEnumeration || info.basicType == Type tEnumeration8 || info.basicType == Type tEnumeration16
            res |> write(info.enumType?.name ?? "enumeration")
        elif info.basicType == Type tPointer
            res |> write(info.firstType != null ? getTypeName(*(info.firstType), items) : "void")
            res |> write("?")
        elif info.basicType == Type tArray
            res |> write(info.firstType != null ? "array<{getTypeName(*(info.firstType), items)}>" : "array")
        elif info.basicType == Type tTable
            if info.firstType != null && info.secondType != null
                res |> write("table<{getTypeName(deref(info.firstType), items)};{getTypeName(deref(info.secondType), items)}>")
            else
                res |> write("table")
        elif info.basicType == Type tTuple
            if info.argTypes != null
                res |> write("tuple<")
                var first = true
                for argtype in arg_types(info)
                    if first { first = false; } else { res |> write(";"); }
                    res |> write(getTypeName(argtype, items))
                res |> write(">")
            else
                res |> write("tuple")
        elif info.basicType == Type tVariant
            if info.argTypes != null && info.argNames != null
                res |> write("variant<")
                var all : array<Completion>
                disableLocks(all)
                let variantDesc = build_string() <| $(variantStr)
                    variantStr |> write("variant<")
                    var f = true
                    for argname, argtype in arg_names(info), arg_types(info)
                        if !f
                            res |> write("; ")
                        f = false
                        var desc = "{argname}: {getTypeName(argtype, items)}"
                        res |> write(desc)
                        variantStr |> write("\n\t")
                        variantStr |> write(desc)
                        all |> emplace([[Completion name = "{argname}", fullName = "variant {argname}", desc = "variant {desc}", kind = CompletionItemKind TypeParameter]])
                    res |> write(">")
                    variantStr |> write("\n>")
                for it in all
                    it.impls |> push(variantDesc)
                    addItem(it, items, true)
                delete all
            else
                res |> write("variant")
        elif info.basicType == Type option
            if info.argTypes != null
                var first = true
                for argtype in arg_types(info)
                    if first { first = false; } else { res |> write(" | "); }
                    res |> write(getTypeName(argtype, items))
            else
                res |> write("option")
        elif info.basicType == Type tFunction || info.basicType == Type tLambda || info.basicType == Type tBlock
            res |> write("{get_das_type_name(info.basicType)}<")
            if info.argTypes != null
                res |> write("(")
                var first = true
                for argtype in arg_types(info)
                    if first { first = false; } else { res |> write(";"); }
                    res |> write(getTypeName(argtype, items))
                res |> write(")")
            if info.firstType != null
                res |> write ((info.argTypes != null ? ":" : ""))
                res |> write (getTypeName(*(info.firstType), items))
            res |> write(">")
        elif info.basicType == Type fakeContext
            res |> write("__context__")
        elif info.basicType == Type fakeLineInfo
            res |> write("__lineInfo__")
        else
            res |> write(get_das_type_name(info.basicType))
        for d in each_dim(info)
            res |> write(d != -1 ? "[{d}]" : "[]")
        if info.isConst
            res |> write(" const")
        if info.isRef
            res |> write(" &")
        if info.isTemp
            res |> write("#")
        if info.isImplicit
            res |> write(" implicit")


def private getValue(value)
    if value is nothing
        return ""
    if value is tString
        return " = \"{value}\""
    return " = {value}"

def parseAnnotation(ann : Annotation; fullModuleName : string; var items : Completions; verbose = true)
    if ann.isBasicStructureAnnotation
        let sann & = unsafe(reinterpret<BasicStructureAnnotation&> (ann))
        let structName = "{sann.name}"
        let fullName = "{fullModuleName}{structName}"
        var all : array<Completion>
        disableLocks(all)
        let structDesc = build_string() <| $(structStr)
            structStr |> write("struct {fullName} [{sann.cppName}] fields: {sann.fieldCount}")
            var props : array<string>
            disableLocks(props)
            var fields : array<tuple<uint; string>>
            disableLocks(fields)
            basic_struct_for_each_field(sann) <| $(name, cppName, xtype, offset)
                let field = "{getTypeName(xtype, items)} [{cppName}"
                if offset != -1u
                    fields |> emplace([[auto offset, "\n\t{name}: {field} @ {offset}]"]])
                else
                    props |> push("\n\t{name}: {field}]")
                let fullItemName = "{fullModuleName}{structName}.{name}"
                all |> emplace([[Completion name = "{name}", fullName = fullItemName,
                                        desc = "{fullItemName}: {field}", kind = CompletionItemKind Field]])

            if length(fields) > 0
                fields |> sort() <| $(a; b)
                    return a._0 < b._0
                if verbose
                    for it in fields
                        structStr |> write(it._1)
            if verbose && length(props) > 0
                if length(fields) > 0
                    structStr |> write("\n")
                structStr |> write(join(props, ""))

            delete props
            delete fields

        if verbose
            addItem([[Completion name = structName, fullName = fullName, desc = "struct {fullName}",
                impls <- [{auto[] structDesc }], kind = CompletionItemKind Struct]], items)
        for it in all
            if verbose
                it.impls |> push(structDesc)
            addItem(it, items)

        delete all

    elif ann.isTypeAnnotation
        if verbose
            let tann & = unsafe(reinterpret<TypeAnnotation&> (ann))
            let structName = "{tann.name}"
            let fullName = "{fullModuleName}{structName}"
            addItem([[Completion name = structName, fullName = fullName, desc = "type {fullName} [{tann.cppName}]", kind = CompletionItemKind Struct]], items)

    else
        if verbose
            let structName = "{ann.name}"
            let fullName = "{fullModuleName}{structName}"
            addItem([[Completion name = structName, fullName = fullName, desc = "annotation {fullName}", kind = CompletionItemKind Struct]], items)


def private parseFunction(info; fullModuleName : string; var items : Completions)
    var res <- [[Completion name = "{info.name}", desc = "def {fullModuleName}{info.name}", kind = CompletionItemKind Function]]
    res.fullName = "{fullModuleName}{info.name}"
    let imp = build_string() <| $(impl)
        impl |> write("def ")
        impl |> write(fullModuleName)
        impl |> write(info.name)
        impl |> write("(")
        var firstArg = true
        for it in info
            // if it.basicType == Type fakeContext
            //   continue
            let argType = getTypeName(it, items)
            if firstArg
                res.funcFirstArg |> push <| argType
                firstArg = false
            else
                impl |> write("; ")
            impl |> write(it.name)
            impl |> write(": ")
            impl |> write(argType)
            impl |> write(getValue(get_variable_value(it)))
        impl |> write(")")
        if info.result != null
            impl |> write(": ")
            impl |> write(getTypeName(*info.result, items))
        if (info.flags & FUNCINFO_BUILTIN) == FUNCINFO_BUILTIN && !empty(info.cppName)
            impl |> write(" [")
            impl |> write(info.cppName)
            impl |> write("]")
    res.impls |> push(imp)
    addItem(res, items)


def private parseModule(pMod; var items : Completions)
    let hugeStructDescSize = 500
    let fullModuleName = "{pMod.name}{length(string(pMod.name)) > 0 ? "::" : ""}"

    addItem([[Completion name = "{pMod.name}", desc = "module {pMod.name}", kind = CompletionItemKind Module]], items)
    module_for_each_structure(pMod) <| $(info)
        let structName = "{info.name}"
        let fullName = "{fullModuleName}{structName}"
        let shortStructDesc = build_string() <| $(shortDesc)
            structure_for_each_annotation(info) <| $(ann; annArgs)
                shortDesc |> write("[{ann.name}(")
                var first = true
                for arg in annArgs
                    if first { first = false; } else { shortDesc |> write(", "); }
                    shortDesc |> write(arg.name)
                    shortDesc |> write(getValue(get_annotation_argument_value(arg)))
                shortDesc |> write(")]\n")
            shortDesc |> write("struct {fullName}")
        var structDesc = shortStructDesc
        var all : array<Completion>
        disableLocks(all)
        for it in info
            let desc = "{it.name}: {getTypeName(it, items)}{getValue(get_variable_value(it))}"
            if structDesc |> length() < hugeStructDescSize
                structDesc += "\n\t{desc}"
            all |> emplace([[Completion name = "{it.name}", fullName = "{fullModuleName}{structName}.{desc}", desc = desc, kind = CompletionItemKind Field]])
        addItem([[Completion name = structName, fullName = fullName, desc = "struct {fullName}", impls <- [{auto[] structDesc}], kind = CompletionItemKind Struct]], items)

        if structDesc |> length() >= hugeStructDescSize
            structDesc = shortStructDesc
            for it in all
                it.impls |> push("{structDesc}\n\t...\n\t{it.desc}\n\t...")
                addItem(it, items)
        else
            for it in all
                it.impls |> push(structDesc)
                addItem(it, items)
        delete all

    module_for_each_function(pMod) <| $(info)
        if find(info.name, "`") == -1 // generic
            parseFunction(info, fullModuleName, items)

    module_for_each_generic(pMod) <| $(info)
        parseFunction(info, fullModuleName, items)

    module_for_each_enumeration(pMod) <| $(info)
        let enumName = "{info.name}"
        let fullName = "{fullModuleName}{enumName}"
        var all : array< tuple < int64; Completion > >
        disableLocks(all)
        let enumDesc = build_string() <| $(enumStr)
            enumStr |> write("enum {fullName}")
            for it in info
                enumStr |> write("\n\t")
                enumStr |> write(it.name)
                enumStr |> write(" = ")
                enumStr |> write(it.value)
                let fullItemName = "{fullModuleName}{enumName} {it.name}"
                all |> emplace([[auto it.value, [[Completion name = "{enumName} {it.name}", fullName = fullItemName, desc = "{fullItemName} = {it.value}", kind = CompletionItemKind EnumMember]] ]])

        addItem([[Completion name = enumName, fullName = fullName, desc = "enum {fullName}", impls <- [{auto[] enumDesc}], kind = CompletionItemKind Enum]], items)
        for it in all
            it._1.impls |> push(enumDesc)
            addItem(it._1, items)
        delete all

    module_for_each_global(pMod) <| $(glob)
        let globName = "{glob.name}"
        let fullName = "{fullModuleName}{globName}"
        addItem([[Completion name = globName, fullName = fullName, desc = "const {fullName}{getValue(get_variable_value(glob))}", kind = CompletionItemKind Constant]], items)

    module_for_each_annotation(pMod) <| $(ann)
        parseAnnotation(ann, fullModuleName, items)


def private addKeywords(var items : Completions)
    for it in keywords
        addItem([[Completion name = it, fullName = it, desc = "keyword {it}", kind = CompletionItemKind Keyword, impls <- [{ auto[] "keyword {it}" }] ]], items)
    for it in basicTypes
        addItem([[Completion name = it, fullName = it, desc = "type {it}", kind = CompletionItemKind Keyword, impls <- [{ auto[] "type {it}" }] ]], items)
    for it in metaFunc
        addItem([[Completion name = it._0, fullName = it._0, desc = "def {it._0}", kind = CompletionItemKind Function, impls <- clone_to_move(it._1) ]], items)
    for it in langOptions
        addItem([[Completion name = it, fullName = "option {it}", desc = "option {it}", kind = CompletionItemKind Property, impls <- [{ auto[] "option {it}" }] ]], items)


def private first_part(str : string; del : string)
    var parts <- split(str, del)
    defer_delete(parts)
    return parts[0]


def private isalnum_at(text : string; pos : int)
    let ch = unsafe(character_uat(text, pos))
    return is_alpha(ch) || is_number(ch)


def programCompletion(program : smart_ptr<Program>; globalCompletion : Completions; var items : Completions)
    var temp : Completions
    program_for_each_module(program) <| $(var mod)
        parseModule(mod, temp)
    // let was = length(items)
    for key, value in keys(temp), values(temp)
        if !globalCompletion |> key_exists(key)
            items[key] <- value
    delete temp
    // info("completion: got {length(items)} was {was} items for {program}\n")


def contains(completions : Completions; name : string) : bool
    for it in values(completions)
        if it.name == name
            return true
    return false


def JV(it : Completion) : JsonValue?
    var tab <- {{ "label" => JV(it.name); "detail" => JV(it.desc); "kind" => JV(it.kind) }}
    if !empty(it.fullName)
        tab["filterName"] = JV(it.fullName)
        tab["sortText"] = JV(it.fullName)
    if length(it.impls) > 0
        tab["documentation"] = JV <| {{ "kind" => JV("markdown"); "value" => JV("```dascript\n{join(it.impls, "\n\n")}\n```") }}
    return JV(tab)

def JV(items : Completions) : JsonValue?
    var list <- [{ for it in values(items); JV(it) }]
    disableLocks(list)
    return JV <| {{ "isIncomplete" => JV(false); "items" => JV(list)}}

def is_operator(str : string)
    return operators |> key_exists(str)

[skip_field_lock_check]
class CompletionData
    globalCompletionUpdate : clock
    globalCompletion : Completions

    def updateGlobalList(force : bool) : bool
        if !force && get_clock() - globalCompletionUpdate < 120.0lf
            return false
        globalCompletionUpdate = get_clock()

        let t0 = ref_time_ticks()
        delete globalCompletion
        addKeywords(globalCompletion)
        program_for_each_registered_module() <| $(var mod)
            parseModule(mod, globalCompletion)
        let sec = double(get_time_usec(t0)) / 1000000.0lf
        info("completion: got {length(globalCompletion)} global items in {sec}s\n")
        return true

    def appendToGlobalList(comp : Completions)
        for name, val in keys(comp), values(comp)
            if !globalCompletion |> key_exists(name)
                globalCompletion[name] := val

    def findCompletion(completions : Completions; name : string; blk : block<(val : Completion) : void>)
        var n = name |> strip_left("smart_ptr<") |> strip_left("array<") |> strip_left("def ") |> strip_left("__::`")
        n = n |> first_part(" ") |> first_part("&") |> first_part("?") |> first_part("#") |> first_part("=") |> first_part(">") |> first_part("-") |> first_part("(")
        n = n |> replace("dasvector`", "") |> replace("smart_ptr`", "")
        if length(n) == 0
            return
        info("--- {n} / {name} ----")
        for it in values(completions)
            if it.kind != CompletionItemKind Keyword && it.fullName == n
                invoke(blk, it)

        for it in values(globalCompletion)
            if it.kind != CompletionItemKind Keyword && it.fullName == n
                invoke(blk, it)

    def findFuncCompletion(completions : Completions; name : string; blk : block<(val : Completion) : void>)
        var n = name |> strip_left("smart_ptr<") |> strip_left("__::`")
        n = n |> first_part(" ") |> first_part("&") |> first_part("?") |> first_part("#") |> first_part("=") |> first_part("<") |> first_part("-") |> first_part("(")
        if length(n) == 0
            return
        info("{name} -> {n}")
        completions |> with_func_arg(n, blk)
        globalCompletion |> with_func_arg(n, blk)


def with_func_arg(completions : Completions; name : string; blk : block<(val : Completion) : void>)
    let nLen = length(name)
    for it in values(completions)
        for arg in it.funcFirstArg
            if (arg |> starts_with(name) &&
                    (length(it.funcFirstArg) == nLen || !isalnum_at(arg, nLen))
                )
                invoke(blk, it)
                break
