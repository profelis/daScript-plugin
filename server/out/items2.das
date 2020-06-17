options indenting=2
options rtti = true
require rtti
require daslib/strings

enum ItemKind
  Text = 1
  Method = 2
  Function = 3
  Constructor = 4
  Field = 5
  Variable = 6
  Class = 7
  Interface = 8
  Module = 9
  Property = 10
  Unit = 11
  Value = 12
  Enum = 13
  Keyword = 14
  Snippet = 15
  Color = 16
  File = 17
  Reference = 18
  Folder = 19
  EnumMember = 20
  Constant = 21
  Struct = 22
  Event = 23
  Operator = 24
  TypeParameter = 25

struct Item
  name : string              // label
  desc : string              // detail
  fullName : string        //
  impls : array<string>      // documentation
  kind : ItemKind

typedef
  Items = table<string; Item>
  MetaItem = tuple< string; array<string> >

let
  // src\parser\ds_parser.ypp:129
  keywords = [[string "struct"; "class"; "let"; "def"; "while"; "if"; "static_if"; "else"; "for"; "recover"; "true"; "false"; "new";
                      "typeinfo"; "type"; "in"; "is"; "as"; "elif"; "static_elif"; "array"; "return"; "null"; "break"; "try"; "options";
                      "table"; "expect"; "const"; "require"; "operator"; "enum"; "finally"; "delete"; "deref"; "scope"; "typedef";
                      "with"; "cast"; "override"; "abstract"; "upcast"; "iterator"; "var"; "addr"; "continue"; "where"; "pass"; "reinterpret";
                      "module"; "public"; "label"; "goto"; "implicit"; "shared"; "smart_ptr"]]

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
  langOptions = [[auto "lint"; "only_fast_aot"; "aot_order_side_effects"; "no_global_heap"; "no_global_variables"; "no_unused_function_arguments"; "heap";
                       "heap_page"; "string_heap"; "string_heap_page"; "stack"; "intern_const_strings"; "intern_strings"; "no_aot"; "aot_prologue"; "plot";
                       "plot_read_write"; "log"; "log_optimization_passes"; "log_stack"; "log_nodes"; "log_mem"; "log_cpp"; "log_aot"; "log_infer_passes";
                       "log_require"; "log_compile_time"; "log_generics"; "log_mn_hash"; "log_ad_hash"; "print_ref"; "print_var_access"; "print_c_style";
                       "rtti"; "optimize"; "fusion"; "remove_unused_symbols"; "always_export_initializer"; "infer_time_folding"; "disable_run";
                       "max_infer_passes"; "indenting"; "debugger"]]

def addItem(var item : Item; var items : Items; once = false)
  let key = "{item.desc}{int(item.kind)}"
  unsafe
    var it = find_for_edit(items, key)
    if it != null
      if once
        return
      for impl in item.impls
        (*it).impls |> push(impl)
      return
    items[key] <- item


def itemToString(it : Item) : string
  let st = build_string() <| $(var writer)
    write(writer, "\{ \"label\":\"")
    write(writer, it.name)
    write(writer, "\", \"detail\":\"")
    write(writer, it.desc)
    if it.fullName != ""
      write(writer, "\", \"filterText\":\"")
      write(writer, it.fullName)
      write(writer, "\", \"sortText\":\"")
      write(writer, it.fullName)
    write(writer, "\", \"kind\":")
    write(writer, int(it.kind))
    if length(it.impls) > 0
      write(writer, ", \"documentation\": \{ \"kind\":\"markdown\", \"value\":\"```dascript\\n")
      write(writer, join(it.impls, "\\n"))
      write(writer, "\\n```\"}")
    write(writer, " \}")
  return st

def dumpItems(items : Items)
  print("\{ \"result\":\"ok\",\n\"items\":[\n")
  var f = true
  for it in values(items)
    if !f
      print(",\n")
    f = false
    print(itemToString(it))
  print("\n]\}\n")

def getTypeName(info; var items : Items)
  var res: string
  if info.basicType == Type tHandle
    res = info.annotation != null ? string(info.annotation.name) : "tHandle"
  elif info.basicType == Type tStructure
    res = info.structType?.name ?? "structure"
  elif info.basicType == Type tEnumeration | info.basicType==Type tEnumeration8 | info.basicType==Type tEnumeration16
    res = info.enumType?.name ?? "enumeration"
  elif info.basicType == Type tPointer
    res = info.firstType != null ? "{getTypeName(*(info.firstType), items)}?" : "void?"
  elif info.basicType == Type tArray
    res = info.firstType != null ? "array<{getTypeName(*(info.firstType), items)}>" : "array"
  elif info.basicType == Type tTable
    if info.firstType != null & info.secondType!=null
      res = "table<{getTypeName(deref(info.firstType), items)};{getTypeName(deref(info.secondType), items)}>"
    else
      res = "table"
  elif info.basicType == Type tTuple
    if info.argTypes != null
      res = "tuple<" + join([{for argtype in arg_types(info); getTypeName(argtype, items)}],";") + ">"
    else
      res = "tuple"
  elif info.basicType == Type tVariant
    if info.argTypes != null & info.argNames != null
      res = "variant<"
      var variantDesc = "variant<"
      var all : array<Item>
      var f = true
      for argname,argtype in arg_names(info),arg_types(info)
        if !f
          res += "; "
        f = false
        var desc = "{argname}: {getTypeName(argtype, items)}"
        res += desc
        variantDesc += "\\n\\t{desc}"
        all |> emplace([[Item name = "{argname}", fullName = "variant {argname}", desc = "variant {desc}", kind = ItemKind TypeParameter]])
      res += ">"
      variantDesc += "\\n>"
      for it in all
        it.impls |> push(variantDesc)
        addItem(it, items, true)
    else
      res = "variant"
  elif info.basicType == Type tFunction | info.basicType == Type tLambda | info.basicType == Type tBlock
    res = "{get_das_type_name(info.basicType)}<"
    if info.argTypes != null
      res += "(" + join([{for argtype in arg_types(info); getTypeName(argtype, items)}],"; ") + ")"
    if info.firstType != null
      res += (info.argTypes != null ? ":" : "") + "{getTypeName(*(info.firstType), items)}"
    res += ">"
  elif info.basicType == Type fakeContext
    res = "__context__"
  else
    res = get_das_type_name(info.basicType)
  res += join([{for d in each_dim(info); d != -1 ? "[{d}]" : "[]"}],"")
  if info.isConst
    res += " const"
  if info.isRef
    res += " &"
  if info.isTemp
    res += "#"
  if info.isImplicit
    res += " implicit"
  return res

def getValue(value)
  if value is nothing
    return ""
  if value is tString
    return " = \"{value}\""
  return " = {value}"

def parseAnnotation(ann; fullModuleName : string; var items: Items)
  if ann.isBasicStructureAnnotation
    unsafe
      let sann & = reinterpret<BasicStructureAnnotation&>(ann)
      let structName = "{sann.name}"
      let fullName = "{fullModuleName}{structName}"
      var structDesc = "struct {fullName} [{sann.cppName}] fields: {sann.fieldCount}"
      var props : array<string>
      var fields : array< tuple<uint; string> >
      var all : array<Item>
      basic_struct_for_each_field(sann) <| $ ( name,cppName,xtype,offset)
        var field = "{getTypeName(xtype, items)} [{cppName}"
        if offset != -1u
          field += " @ {offset}]"
          fields |> emplace([[auto offset, "\\n\\t{name}: {field}"]])
        else
          field += "]"
          props |> push("\\n\\t{name}: {field}")
        let fullItemName = "{fullModuleName}{structName}.{name}"
        all |> emplace([[Item name = "{name}", fullName = fullItemName,
                    desc = "{fullItemName}: {field}", kind = ItemKind Field]])

      if length(fields) > 0
        fields |> sort() <| $(a; b)
          return a._0 < b._0
        for it in fields
          structDesc += it._1
      if length(props) > 0
        if length(fields) > 0
          structDesc += "\\n"
        structDesc += props |> join("")

      addItem([[Item name = structName, fullName = fullName, desc = "struct {fullName}",
        impls <- [{auto[] structDesc }], kind = ItemKind Struct]], items)
      for it in all
        it.impls |> push_clone(structDesc)
        addItem(it, items)

  elif ann.isTypeAnnotation
    unsafe
      let tann & = reinterpret<TypeAnnotation&>(ann)
      let structName = "{tann.name}"
      let fullName = "{fullModuleName}{structName}"
      addItem([[Item name = structName, fullName = fullName, desc = "type {fullName} [{tann.cppName}]", kind = ItemKind Struct]], items)

  else
    let structName = "{ann.name}"
    let fullName = "{fullModuleName}{structName}"
    addItem([[Item name = structName, fullName = fullName, desc = "annotation {fullName}", kind = ItemKind Struct]], items)

def parseFunction(info; fullModuleName : string; var items : Items)
  var res <- [[Item name = "{info.name}", desc = "def {fullModuleName}{info.name}", kind = ItemKind Function]]
  res.fullName = "{fullModuleName}{info.name}"
  var impl = "def {fullModuleName}{info.name}("
  var firstArg = true
  for it in info
    // if it.basicType == Type fakeContext
    //   continue
    if !firstArg
      impl += "; "
    firstArg = false
    impl += "{it.name}: {getTypeName(it, items)}{getValue(get_variable_value(it))}"
  impl += ")"
  if info.result != null
    impl += ": {getTypeName(*(info.result), items)}"
  if (info.flags & FUNCINFO_BUILTIN) == FUNCINFO_BUILTIN & info.cppName != ""
    impl += " [{info.cppName}]"
  res.impls |> push(impl)
  addItem(res, items)

def parseModule(pMod; var items : Items)
  let fullModuleName = "{pMod.name}::"

  addItem([[Item name = "{pMod.name}", desc = "module {pMod.name}", kind = ItemKind Module]], items)
  rtti::module_for_each_structure(pMod) <| $(info)
    let structName = "{info.name}"
    let fullName = "{fullModuleName}{structName}"
    var shortStructDesc = ""
    structure_for_each_annotation(info) <| $(ann; annArgs)
      let argT = join([{for arg in annArgs; "{arg.name}{getValue(get_annotation_argument_value(arg))}"}],", ")
      shortStructDesc += "[{ann.name}({argT})]\\n"
    shortStructDesc += "struct {fullName}"
    var structDesc = shortStructDesc
    var all : array<Item>
    for it in info
      let desc = "{it.name}: {getTypeName(it, items)}{getValue(get_variable_value(it))}"
      structDesc += "\\n\\t{desc}"
      all |> emplace([[Item name = "{it.name}", fullName = "{fullModuleName}{structName}.{desc}", desc = desc, kind = ItemKind Field]])
    addItem([[Item name = structName, fullName = fullName, desc = "struct {fullName}", impls <- [{auto[] structDesc}], kind = ItemKind Struct]], items)

    if structDesc |> length() > 500
      structDesc = shortStructDesc
      for it in all
        it.impls |> push("{structDesc}\\n\\t...\\n\\t{it.desc}\\n\\t...")
        addItem(it, items)
    else
      for it in all
        it.impls |> push(structDesc)
        addItem(it, items)

  rtti::module_for_each_function(pMod) <| $(info)
    if find(info.name, "`") == -1 // generic
      parseFunction(info, fullModuleName, items)

  rtti::module_for_each_generic(pMod) <| $(info)
    parseFunction(info, fullModuleName, items)

  rtti::module_for_each_enumeration(pMod) <| $(info)
    let enumName = "{info.name}"
    let fullName = "{fullModuleName}{enumName}"
    var enumDesc = "enum {fullName}"
    var all : array< tuple < int64; Item > >
    for it in info
      enumDesc += "\\n\\t{it.name} = {it.value}"
      let fullItemName = "{fullModuleName}{enumName} {it.name}"
      all |> emplace([[auto it.value, [[Item name = "{it.name}", fullName = fullItemName, desc = "{fullItemName} = {it.value}", kind = ItemKind EnumMember]] ]])

    addItem([[Item name = enumName, fullName = fullName, desc = "enum {fullName}", impls <- [{auto[] enumDesc}], kind = ItemKind Enum]], items)
    for it in all
      it._1.impls |> push(enumDesc)
      addItem(it._1, items)

  rtti::module_for_each_global(pMod) <| $ (glob)
    let globName = "{glob.name}"
    let fullName = "{fullModuleName}{globName}"
    addItem([[Item name = globName, fullName = fullName, desc = "const {fullName}{getValue(get_variable_value(glob))}", kind = ItemKind Constant]], items)

  rtti::module_for_each_annotation(pMod) <| $(ann)
    parseAnnotation(ann, fullModuleName, items)

def addKeywords(var items : Items)
  for it in keywords
    addItem([[Item name = it, fullName = it, desc = "keyword {it}", kind = ItemKind Keyword, impls <- [{ auto[] "keyword {it}" }] ]], items)
  for it in basicTypes
    addItem([[Item name = it, fullName = it, desc = "type {it}", kind = ItemKind Keyword, impls <- [{ auto[] "type {it}" }] ]], items)
  for it in metaFunc
    addItem([[Item name = it._0, fullName = it._0, desc = "def {it._0}", kind = ItemKind Function, impls <- clone_to_move(it._1) ]], items)
  for it in langOptions
    addItem([[Item name = it, fullName = "option {it}", desc = "option {it}", kind = ItemKind Property, impls <- [{ auto[] "option {it}" }] ]], items)

// [export]
// def dumpModule(name : string)
//   compile_file(name) <| $(ok, program, err)
//     if !ok
//       print(string(err))
//       return
//     var items : Items
//     addKeywords(items)
//     program_for_each_module(program) <| $(var mod)
//       parseModule(mod, items)
//     dumpItems(items)

def dumpAllModules()
  var items : Items
  addKeywords(items)
  program_for_each_registered_module() <| $(var mod)
    parseModule(mod, items)
  dumpItems(items)

[init]
def main()
  dumpAllModules()

