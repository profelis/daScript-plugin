options no_aot = true
options indenting = 2
options multiple_contexts = true

options rtti = true
require rtti
require rtti_tools
require daslib/json
require strings
require daslib/strings_boost
require lsp_types
require lib
require logger
require daslib/jobque_boost

struct Completion
  name : string              // label
  desc : string              // detail
  fullName : string        //
  impls : array<string>      // documentation
  kind : CompletionItemKind
  funcFirstArg : string

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

  // src\ast\ast_lint.cpp:312
  langOptions = [[auto "lint"; "only_fast_aot"; "aot_order_side_effects"; "no_global_heap"; "no_global_variables"; "no_unused_function_arguments";
                       "no_unused_block_arguments"; "stack"; "intern_strings"; "multiple_contexts"; "persistent_heap"; "persistent_string_heap";
                       "heap_size_hint"; "string_heap_size_hint"; "no_aot"; "aot_prologue";
                       "log"; "log_optimization_passes"; "log_stack"; "log_nodes"; "log_nodes_aot_hash"; "log_mem"; "log_cpp"; "log_aot"; "log_infer_passes";
                       "log_require"; "log_compile_time"; "log_generics"; "log_mn_hash"; "log_gmn_hash"; "log_ad_hash"; "print_ref"; "print_var_access"; "print_c_style";
                       "rtti"; "optimize"; "fusion"; "remove_unused_symbols"; "always_export_initializer"; "infer_time_folding"; "disable_run";
                       "max_infer_passes"; "indenting"; "debugger"]]

  operators = [[auto "!"; "~"; "+="; "-="; "*="; "/="; "%="; "&="; "|="; "^="; "&&="; "||="; "^^="; "&&"; "||"; "^^"; "+"; "-"; "*"; "/"; "%"; "<"; ">";
                     "=="; "!="; "<="; ">="; "&"; "|"; "^"; "++"; "--"; "+++"; "---"; "<<"; ">>"; "<<="; ">>="; "<<<"; ">>>"; "<<<="; ">>>="
  ]]

[private]
def addItem(var item : Completion; var items : Completions; once = false)
  let key = "{item.desc}{int(item.kind)}"
  unsafe
    var it = find_for_edit(items, key)
    if it != null
      if once
        return
      for impl in item.impls
        (*it).impls |> emplace(impl)
      delete item
      return
    items[key] <- item

[private]
def module_name(mod)
  if mod == null
    return ""
  let name = "{mod.name}"
  return length(name) == 0 ? "" : "{name}::"

[private]
def getTypeName(info; var items : Completions)
  var res: string
  if info.basicType == Type tHandle
    res = info.annotation != null ? "{module_name(info.annotation._module)}{info.annotation.name}" : "tHandle"
  elif info.basicType == Type tStructure
    res = info.structType?.name ?? "structure"
  elif info.basicType == Type tEnumeration || info.basicType==Type tEnumeration8 || info.basicType==Type tEnumeration16
    res = info.enumType?.name ?? "enumeration"
  elif info.basicType == Type tPointer
    res = info.firstType != null ? "{getTypeName(*(info.firstType), items)}?" : "void?"
  elif info.basicType == Type tArray
    res = info.firstType != null ? "array<{getTypeName(*(info.firstType), items)}>" : "array"
  elif info.basicType == Type tTable
    if info.firstType != null && info.secondType!=null
      res = "table<{getTypeName(deref(info.firstType), items)};{getTypeName(deref(info.secondType), items)}>"
    else
      res = "table"
  elif info.basicType == Type tTuple
    if info.argTypes != null
      var args <- [{for argtype in arg_types(info); getTypeName(argtype, items)}]
      res = "tuple<" + join(args,";") + ">"
      delete args
    else
      res = "tuple"
  elif info.basicType == Type tVariant
    if info.argTypes != null && info.argNames != null
      res = "variant<"
      var variantDesc = "variant<"
      var all : array<Completion>
      var f = true
      for argname, argtype in arg_names(info), arg_types(info)
        if !f
          res += "; "
        f = false
        var desc = "{argname}: {getTypeName(argtype, items)}"
        res += desc
        variantDesc += "\n\t{desc}"
        all |> emplace([[Completion name = "{argname}", fullName = "variant {argname}", desc = "variant {desc}", kind = CompletionItemKind TypeParameter]])
      res += ">"
      variantDesc += "\n>"
      for it in all
        it.impls |> emplace(variantDesc)
        addItem(it, items, true)
      delete all
    else
      res = "variant"
  elif info.basicType == Type tFunction || info.basicType == Type tLambda || info.basicType == Type tBlock
    res = "{get_das_type_name(info.basicType)}<"
    if info.argTypes != null
      var args <- [{for argtype in arg_types(info); getTypeName(argtype, items)}]
      res += "(" + join(args,"; ") + ")"
      delete args
    if info.firstType != null
      res += (info.argTypes != null ? ":" : "") + "{getTypeName(*(info.firstType), items)}"
    res += ">"
  elif info.basicType == Type fakeContext
    res = "__context__"
  elif info.basicType == Type fakeLineInfo
    res = "__lineInfo__"
  else
    res = get_das_type_name(info.basicType)
  var dim <- [{for d in each_dim(info); d != -1 ? "[{d}]" : "[]"}]
  res += join(dim, "")
  delete dim
  if info.isConst
    res += " const"
  if info.isRef
    res += " &"
  if info.isTemp
    res += "#"
  if info.isImplicit
    res += " implicit"
  return res

[private]
def getValue(value)
  if value is nothing
    return ""
  if value is tString
    return " = \"{value}\""
  return " = {value}"

[export]
def parseAnnotation(ann : rtti::Annotation; fullModuleName : string; var items: Completions; verbose = true)
  if ann.isBasicStructureAnnotation
    unsafe
      let sann & = reinterpret<BasicStructureAnnotation&>(ann)
      let structName = "{sann.name}"
      let fullName = "{fullModuleName}{structName}"
      var structDesc = "struct {fullName} [{sann.cppName}] fields: {sann.fieldCount}"
      var props : array<string>
      var fields : array<tuple<uint; string>>
      var all : array<Completion>
      basic_struct_for_each_field(sann) <| $ ( name, cppName, xtype, offset)
        var field = "{getTypeName(xtype, items)} [{cppName}"
        if offset != -1u
          field += " @ {offset}]"
          fields |> emplace([[auto offset, "\n\t{name}: {field}"]])
        else
          field += "]"
          props |> emplace("\n\t{name}: {field}")
        let fullItemName = "{fullModuleName}{structName}.{name}"
        all |> emplace([[Completion name = "{name}", fullName = fullItemName,
                    desc = "{fullItemName}: {field}", kind = CompletionItemKind Field]])

      if length(fields) > 0
        fields |> sort() <| $(a; b)
          return a._0 < b._0
        if verbose
          for it in fields
            structDesc += it._1
      if verbose && length(props) > 0
        if length(fields) > 0
          structDesc += "\n"
        structDesc += join(props, "")

      if verbose
        addItem([[Completion name = structName, fullName = fullName, desc = "struct {fullName}",
          impls <- [{auto[] structDesc }], kind = CompletionItemKind Struct]], items)
      for it in all
        if verbose
          it.impls |> emplace(structDesc)
        addItem(it, items)

      delete props
      delete fields
      delete all

  elif ann.isTypeAnnotation
    if verbose
      unsafe
        let tann & = reinterpret<TypeAnnotation&>(ann)
        let structName = "{tann.name}"
        let fullName = "{fullModuleName}{structName}"
        addItem([[Completion name = structName, fullName = fullName, desc = "type {fullName} [{tann.cppName}]", kind = CompletionItemKind Struct]], items)

  else
    if verbose
      let structName = "{ann.name}"
      let fullName = "{fullModuleName}{structName}"
      addItem([[Completion name = structName, fullName = fullName, desc = "annotation {fullName}", kind = CompletionItemKind Struct]], items)

[private]
def parseFunction(info; fullModuleName : string; var items : Completions)
  var res <- [[Completion name = "{info.name}", desc = "def {fullModuleName}{info.name}", kind = CompletionItemKind Function]]
  res.fullName = "{fullModuleName}{info.name}"
  var impl = "def {fullModuleName}{info.name}("
  var firstArg = true
  for it in info
    // if it.basicType == Type fakeContext
    //   continue
    let argType = getTypeName(it, items)
    if firstArg
      res.funcFirstArg = argType
    else
      impl += "; "
    firstArg = false
    impl += "{it.name}: {argType}{getValue(get_variable_value(it))}"
  impl += ")"
  if info.result != null
    impl += ": {getTypeName(*info.result, items)}"
  if (info.flags & FUNCINFO_BUILTIN) == FUNCINFO_BUILTIN && !empty(info.cppName)
    impl += " [{info.cppName}]"
  res.impls |> emplace(impl)
  addItem(res, items)

[private]
def parseModule(pMod; var items : Completions)
  let hugeStructDescSize = 500
  let fullModuleName = "{pMod.name}{length(string(pMod.name)) > 0 ? "::" : ""}"

  addItem([[Completion name = "{pMod.name}", desc = "module {pMod.name}", kind = CompletionItemKind Module]], items)
  rtti::module_for_each_structure(pMod) <| $(info)
    let structName = "{info.name}"
    let fullName = "{fullModuleName}{structName}"
    var shortStructDesc = ""
    structure_for_each_annotation(info) <| $(ann; annArgs)
      var args <- [{for arg in annArgs; "{arg.name}{getValue(get_annotation_argument_value(arg))}"}]
      let argT = join(args,", ")
      delete args
      shortStructDesc += "[{ann.name}({argT})]\n"
    shortStructDesc += "struct {fullName}"
    var structDesc = shortStructDesc
    var all : array<Completion>
    for it in info
      let desc = "{it.name}: {getTypeName(it, items)}{getValue(get_variable_value(it))}"
      if structDesc |> length() < hugeStructDescSize
        structDesc += "\n\t{desc}"
      all |> emplace([[Completion name = "{it.name}", fullName = "{fullModuleName}{structName}.{desc}", desc = desc, kind = CompletionItemKind Field]])
    addItem([[Completion name = structName, fullName = fullName, desc = "struct {fullName}", impls <- [{auto[] structDesc}], kind = CompletionItemKind Struct]], items)

    if structDesc |> length() >= hugeStructDescSize
      info("deep: {structName}", get_line_info())
      structDesc = shortStructDesc
      for it in all
        it.impls |> emplace("{structDesc}\n\t...\n\t{it.desc}\n\t...")
        addItem(it, items)
    else
      for it in all
        it.impls |> emplace(structDesc)
        addItem(it, items)
    delete all

  rtti::module_for_each_function(pMod) <| $(info)
    if find(info.name, "`") == -1 // generic
      parseFunction(info, fullModuleName, items)

  rtti::module_for_each_generic(pMod) <| $(info)
    parseFunction(info, fullModuleName, items)

  rtti::module_for_each_enumeration(pMod) <| $(info)
    let enumName = "{info.name}"
    let fullName = "{fullModuleName}{enumName}"
    var enumDesc = "enum {fullName}"
    var all : array< tuple < int64; Completion > >
    for it in info
      enumDesc += "\n\t{it.name} = {it.value}"
      let fullItemName = "{fullModuleName}{enumName} {it.name}"
      all |> emplace([[auto it.value, [[Completion name = "{enumName} {it.name}", fullName = fullItemName, desc = "{fullItemName} = {it.value}", kind = CompletionItemKind EnumMember]] ]])

    addItem([[Completion name = enumName, fullName = fullName, desc = "enum {fullName}", impls <- [{auto[] enumDesc}], kind = CompletionItemKind Enum]], items)
    for it in all
      it._1.impls |> emplace(enumDesc)
      addItem(it._1, items)
    delete all

  rtti::module_for_each_global(pMod) <| $ (glob)
    let globName = "{glob.name}"
    let fullName = "{fullModuleName}{globName}"
    addItem([[Completion name = globName, fullName = fullName, desc = "const {fullName}{getValue(get_variable_value(glob))}", kind = CompletionItemKind Constant]], items)

  rtti::module_for_each_annotation(pMod) <| $(ann)
    parseAnnotation(ann, fullModuleName, items)

[private]
def addKeywords(var items : Completions)
  for it in keywords
    addItem([[Completion name = it, fullName = it, desc = "keyword {it}", kind = CompletionItemKind Keyword, impls <- [{ auto[] "keyword {it}" }] ]], items)
  for it in basicTypes
    addItem([[Completion name = it, fullName = it, desc = "type {it}", kind = CompletionItemKind Keyword, impls <- [{ auto[] "type {it}" }] ]], items)
  for it in metaFunc
    addItem([[Completion name = it._0, fullName = it._0, desc = "def {it._0}", kind = CompletionItemKind Function, impls <- clone_to_move(it._1) ]], items)
  for it in langOptions
    addItem([[Completion name = it, fullName = "option {it}", desc = "option {it}", kind = CompletionItemKind Property, impls <- [{ auto[] "option {it}" }] ]], items)

[export]
def js(it : Completion; doc = true) : JsonValue?
  var tab <- {{ "label" => js(it.name); "detail" => js(it.desc); "kind" => js(double(int(it.kind))) }}
  if !empty(it.fullName)
    tab["filterName"] <- js(it.fullName)
    tab["sortText"] <- js(it.fullName)
  if doc && length(it.impls) > 0
    tab["documentation"] <- js({{ "kind" => js("markdown"); "value" => js("```dascript\n" + join(it.impls, "\n") + "\n```") }})
  return js(tab)

[export]
def js(items : Completions; doc = true) : JsonValue?
  var list <- [{ for it in values(items); js(it, doc) }]
  return js({{ "isIncomplete" => js(false); "items" => js(list)}})

[export]
def is_operator(str : string)
  return find_index(operators, str) >= 0

[private]
def first_part(str : string; del : string)
  let parts <- split(str, del)
  return parts[0]

[private]
def isalnum_at(text : string; pos : int)
  unsafe
    let ch = character_uat(text, pos)
    return is_alpha(ch) || is_number(ch)

[export] class CompletionData
  globalCompletionUpdate : clock
  globalCompletion : Completions

  def updateGlobalList(experementalFeatures: bool)
    if get_clock() - globalCompletionUpdate < 10f
      return
    globalCompletionUpdate = get_clock()

    let useJobs = experementalFeatures
    if !useJobs
      var t0 = ref_time_ticks()
      delete globalCompletion
      addKeywords(globalCompletion)
      program_for_each_registered_module() <| $(var mod)
        parseModule(mod, globalCompletion)
      var dt = get_time_usec(t0)
      var sec = double(dt)/1000000.0lf
      info("completion: got {length(globalCompletion)} global items in {sec}s\n", get_line_info())

    else
      with_job_que <|
        var t0 = ref_time_ticks()

        var modsNum = 0
        program_for_each_registered_module() <| $[unused_argument(mod)](var mod)
          modsNum += 1
        with_channel(modsNum) <| $ (channel)

          for i in range(modsNum)
            new_job <| @
              var coms: Completions
              var num = i
              program_for_each_registered_module() <| $(var mod)
                if num == 0
                  parseModule(mod, coms)
                num --
              for it in values(coms)
                unsafe
                  channel |> push(addr(it))
              channel |> notify()

          delete globalCompletion
          addKeywords(globalCompletion)

          for item in each(channel, type<Completion>)
            var key = "{item.desc}{int(item.kind)}"
            globalCompletion[key] := item

        var dt = get_time_usec(t0)
        var sec = double(dt)/1000000.0lf
        info("completion: got {length(globalCompletion)} global items in {sec} s in {modsNum} jobs\n", get_line_info())


  def programCompletion(program : smart_ptr<Program>; var items : Completions)
    var temp : Completions
    program_for_each_module(program) <| $(var mod)
      parseModule(mod, temp)
    let was = length(items)
    for key, value in keys(temp), values(temp)
      if !globalCompletion |> key_exists(key)
        items[key] <- value
    delete temp
    info("completion: got {length(items)} was {was} items for {program}\n", get_line_info())

  def getCompletion(items : Completions)
    var list <- [{ for it in values(items); js(it) }]
    for val in values(globalCompletion)
      list |> emplace(js(val))
    info("> merge program completion {length(items)} with global {length(globalCompletion)} => {length(list)}\n", get_line_info())
    return js({{ "isIncomplete" => js(false); "items" => js(list)}})


  def findCompletion(completions : Completions; name : string; blk : block<(val : Completion):void>)
    var n = name |> strip_left("smart_ptr<") |> strip_left("array<") |> strip_left("def ") |> strip_left("__::`")
    n = n |> first_part(" ") |> first_part("&") |> first_part("?") |> first_part("#") |> first_part("=") |> first_part(">") |> first_part("-") |> first_part("(")
    n = n |> replace("dasvector`", "") |> replace("smart_ptr`", "")
    info("--- {n} / {name} ----", get_line_info())
    for it in values(completions)
      if it.kind != CompletionItemKind Keyword && it.fullName == n
        invoke(blk, it)

    for it in values(globalCompletion)
      if it.kind != CompletionItemKind Keyword && it.fullName == n
        invoke(blk, it)

  def findFuncCompletion(completions : Completions; name : string; blk : block<(val : Completion):void>)
    var n = name |> strip_left("smart_ptr<") |> strip_left("__::`")
    n = n |> first_part(" ") |> first_part("&") |> first_part("?") |> first_part("#") |> first_part("=") |> first_part("<") |> first_part("-") |> first_part("(")
    info("--- {n} / {name} ----", get_line_info())
    for it in values(completions)
      if (starts_with(it.funcFirstArg, n) &&
          (length(it.funcFirstArg) == length(n) || !isalnum_at(it.funcFirstArg, length(n)))
        )
        // info(it, get_line_info())
        invoke(blk, it)

    for it in values(globalCompletion)
      if (starts_with(it.funcFirstArg, n) &&
          (length(it.funcFirstArg) == length(n) || !isalnum_at(it.funcFirstArg, length(n)))
        )
        // info(it, get_line_info())
        invoke(blk, it)
