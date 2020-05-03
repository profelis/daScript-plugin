options indenting=2
require rtti

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

let
  keywords = [[auto "struct"; "let"; "def"; "while"; "if"; "static_if"; "else"; "for"; "catch"; "true"; "false"; "new"; "typeinfo"; "type"; "in"; "is"; "as";
                    "elif"; "static_elif"; "array"; "return"; "null"; "break"; "try"; "options"; "table"; "expect"; "const"; "require"; "operator"; "enum";
                    "finally"; "delete"; "deref"; "scope"; "typedef"; "with"; "cast"; "override"; "upcast"; "iterator"; "var"; "addr"; "continue"; "where";
                    "pass"; "reinterpret"; "module"; "public"; "label"; "goto"; "implicit"; "shared"
              ]]

  basicTypes = [[auto "bool"; "void"; "string"; "auto"; "int"; "int2"; "int3"; "int4"; "uint"; "uint2"; "uint3"; "uint4"; "float"; "float2"; "float3";
                      "float4"; "range"; "urange"; "block"; "int64"; "uint64"; "double"; "function"; "lambda"; "int8"; "uint8"; "int16"; "uint16"; "tuple";
                      "variant"; "generator"; "yield"]]

  metaFunc = [[auto "assert"; "verify"; "static_assert"; "concept_assert"; "debug"; "memzero"; "invoke"]]

  langOptions = [[auto "lint"; "only_fast_aot"; "aot_order_side_effects"; "no_global_heap"; "no_global_variables"; "no_unused_function_arguments"; "heap";
                       "string_heap"; "stack"; "intern_const_strings"; "intern_strings"; "no_aot"; "aot_prologue"; "plot"; "plot_read_write"; "log";
                       "log_optimization_passes"; "log_stack"; "log_nodes"; "log_mem"; "log_cpp"; "log_aot"; "log_infer_passes"; "log_require";
                       "log_compile_time"; "log_generics"; "log_mn_hash"; "log_ad_hash"; "print_ref"; "print_var_access"; "print_c_style"; "rtti"; "optimize";
                       "fusion"; "remove_unused_symbols"; "always_export_initializer"; "infer_time_folding"; "disable_run"; "max_infer_passes"; "indenting"]]

[unsafe]
def addItem(var item; var items : Items)
  let key = "{item.desc}{int(item.kind)}"
  var it = find_for_edit(items, key)
  if it != null
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
      write(writer, ", \"documentation\":\"")
      write(writer, join(it.impls, "\\n"))
      write(writer, "\"")
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

def getTypeName(info)
  var res: string
  if info.basicType == Type tHandle
    res = string(info.annotation_or_name.name)
  elif info.basicType == Type tStructure
    res = info.structType?.name ?? "structure"
  elif info.basicType == Type tEnumeration | info.basicType==Type tEnumeration8 | info.basicType==Type tEnumeration16
    res = info.enumType?.name ?? "enumeration"
  elif info.basicType == Type tPointer
    res = info.firstType != null ? "{getTypeName(*(info.firstType))}?" : "void?"
  elif info.basicType == Type tArray
    res = info.firstType != null ? "array<{getTypeName(*(info.firstType))}>" : "array"
  elif info.basicType == Type tTable
    if info.firstType != null & info.secondType!=null
      res = "table<{getTypeName(deref(info.firstType))};{getTypeName(deref(info.secondType))}>"
    else
      res = "table"
  else
    res = "{info.basicType}"
  if info.isConst
    res += " const"
  return res

def parseFunction(info; fullModuleName : string)
  var res <- [[Item name = "{info.name}", desc = "def {fullModuleName}{info.name}", kind = ItemKind Function]]
  res.fullName = "{fullModuleName}{info.name}"
  var impl = "def {fullModuleName}{info.name}("
  var firstArg = true
  for it in info
    if it.basicType == Type fakeContext
      continue
    if !firstArg
      impl += "; "
    firstArg = false
    impl += "{it.name}: {getTypeName(it)}"
  impl += ")"
  if info.result != null
    impl += ": {getTypeName(*(info.result))}"
  if (info.flags & FUNCINFO_BUILTIN) == FUNCINFO_BUILTIN & info.cppName != ""
    impl += " [{info.cppName}]"
  res.impls |> push(impl)
  return <- res

def globalValue(glob)
  if glob.basicType == Type tInt
    return " = {glob.iValue}"
  elif glob.basicType == Type tUInt
    return " = {glob.uValue}"
  elif glob.basicType == Type tInt64
    return " = {glob.i64Value}"
  elif glob.basicType == Type tUInt64
    return " = {glob.u64Value}"
  elif glob.basicType == Type tFloat
    return " = {glob.fValue}"
  elif glob.basicType == Type tDouble
    return " = {glob.dfValue}"
  elif glob.basicType == Type tBool
    return " = {glob.bValue}"
  elif glob.basicType == Type tString
    return " = {glob.sValue}"
  else
    return ""

def parseModule(pMod; var items : Items)
  let fullModuleName = "{pMod.name}::"

  addItem([[Item name = "{pMod.name}", desc = "module {pMod.name}", kind = ItemKind Module]], items)
  rtti::module_for_each_structure(pMod) <| $(info)
    let structName = "{info.name}"
    let fullName = "{fullModuleName}{structName}"
    var structDesc = "struct {fullName}"

    var all : array<Item>
    for it in info
      let desc = "{it.name}: {getTypeName(it)}"
      structDesc += "\\n\\t{desc}"
      all |> emplace([[Item name = "{it.name}", fullName = "{fullModuleName}{structName}.{desc}", desc = desc, kind = ItemKind Field]])
    addItem([[Item name = structName, fullName = fullName, desc = "struct {fullName}", impls <- [{auto[] structDesc}], kind = ItemKind Struct]], items)
    for it in all
      it.impls |> push(structDesc)
      addItem(it, items)

  rtti::module_for_each_function(pMod) <| $(info)
    if find(info.name, "`") != -1 // generic
      return
    addItem(parseFunction(info, fullModuleName), items)

  rtti::module_for_each_generic(pMod) <| $(info)
    addItem(parseFunction(info, fullModuleName), items)

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

  rtti_builtin_module_for_each_global(pMod) <| $ (glob)
    let globName = "{glob.name}"
    addItem([[Item name = globName, fullName = "{fullModuleName}{globName}", desc = "const {fullModuleName}{globName}{globalValue(glob)}", kind = ItemKind Constant]], items)

def addKeywords(var items : Items)
  for it in keywords
    addItem([[Item name = it, fullName = it, desc = "keyword {it}", kind = ItemKind Keyword]], items)
  for it in basicTypes
    addItem([[Item name = it, fullName = it, desc = "type {it}", kind = ItemKind Keyword]], items)
  for it in metaFunc
    addItem([[Item name = it, fullName = it, desc = "{it}(...)", kind = ItemKind Function]], items)
  for it in langOptions
    addItem([[Item name = it, fullName = "option {it}", desc = "option {it}", kind = ItemKind Property]], items)

[export]
def dumpModule(name : string)
  compile_file(name) <| $(ok, program, err)
    if !ok
      print(string(err))
      return
    var items : Items
    addKeywords(items)
    program_for_each_module(program) <| $(var mod)
      parseModule(mod, items)
    dumpItems(items)


def dumpAllModules()
  var items : Items
  addKeywords(items)
  program_for_each_registered_module() <| $(var mod)
    parseModule(mod, items)
  dumpItems(items)

[init]
def main()
  // dumpModule("test.das")
  dumpAllModules()

