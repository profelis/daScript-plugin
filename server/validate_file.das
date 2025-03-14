options indenting = 4
options gen2 = false
options no_global_variables
options no_unused_function_arguments
options no_unused_block_arguments
options strict_smart_pointers
options no_aot

require ast
require rtti
require strings
require fio
require daslib/ast_boost
require daslib/strings_boost
require daslib/das_source_formatter
require completion_boost

def arg_value(args : array<string>; name : string; default_value : string) : string
    let idx = args |> find_index(name)
    return idx >= 0 && idx + 1 < length(args) ? args[idx + 1] : default_value

def arg_value(args : array<string>; name : string; default_value : int) : int
    let idx = args |> find_index(name)
    return idx >= 0 && idx + 1 < length(args) ? to_int(args[idx + 1]) : default_value

struct ValidateConfig
    compiler : string
    file : string
    originalFile : string
    projectFile : string
    resultFile : string
    ignoreSharedModules : bool
    noGlobalVariables : bool
    noUnusedBlockArguments : bool
    noUnusedFunctionArguments : bool
    failOnLackOfAotExport : bool
    version2syntax : bool
    gen2MakeSyntax : bool
    globalCompletion : bool
    autoFormat : bool

    fileAccessRoots : table<string; string>

let WRITE_RESULT = true
let WRITE_RESULT_TO_FILE = false

[export]
def main()
    var args <- get_command_line_arguments()
    var config : ValidateConfig

    for arg, idx in args, count()
        if arg == "--file-access-root"
            if idx + 1 < length(args)
                var inscope parts <- args[idx + 1] |> split(":")
                if length(parts) == 2
                    config.fileAccessRoots |> insert(parts[0], parts[1])

    config.compiler = args[0]
    config.file = args |> arg_value("--file", "")
    config.originalFile = args |> arg_value("--original-file", config.file)
    config.projectFile = args |> arg_value("--project-file", "")
    config.resultFile = args |> arg_value("--result", "")

    config.ignoreSharedModules = args |> has_value("--ignore-shared-modules")
    config.noGlobalVariables = args |> has_value("--no-global-variables")
    config.noUnusedBlockArguments = args |> has_value("--no-unused-block-arguments")
    config.noUnusedFunctionArguments = args |> has_value("--no-unused-function-arguments")
    config.failOnLackOfAotExport = args |> has_value("--fail-on-lack-of-aot-export")
    config.version2syntax = args |> has_value("--version-2-syntax")
    config.gen2MakeSyntax = args |> has_value("--gen2-make-syntax")
    config.globalCompletion = args |> has_value("--global-completion")
    config.autoFormat = args |> has_value("--auto-format")

    var res : ValidationResult
    res |> compile(config)
    res.dasRoot = get_das_root()

    let textRes = config.autoFormat ? res.autoFormat : res |> to_json()

    if WRITE_RESULT_TO_FILE
        mkdir("results")
        let fileName = file_name_to_uri(config.originalFile) |> split("/") |> back()
        fopen("results/{fileName}.json", "w") <| $(f)
            if f == null
                error("unable to open file 'results/{fileName}.json'")
                unsafe(fio::exit(1))
            f |> fwrite(textRes)

    if WRITE_RESULT
        fopen(config.resultFile, "w") <| $(f)
            if f == null
                error("unable to open file '{config.resultFile}'")
                unsafe(fio::exit(1))
            f |> fwrite(textRes)


struct DasError : CompletionAt
    what : string
    extra : string
    fixme : string
    cerr : int
    level : int // 0 error, 1 warning

struct ModDeps
    mod : string
    depth : int

struct ModuleRequirement : CompletionAt
    mod : string
    req : string
    isPublic : bool
    dependencies : array<ModDeps>


struct ValidationResult
    tokens : array<DasToken>
    errors : array<DasError>
    builtinMods : array<string>
    // processedMods : array<string>
    // allMods : array<string>
    completion : CompletionResult
    dasRoot : string
    autoFormat : string
    requirements : array<ModuleRequirement>


def init_file_access(var access; fileAccessorRoots : table<string; string>)
    for rootName, rootPath in keys(fileAccessorRoots), values(fileAccessorRoots)
        access |> add_file_access_root(rootName, rootPath)

struct ErrorData
    message : string

def compile(var res : ValidationResult; config : ValidateConfig)

    fopen(config.file, "rb") <| $(fr)
        if fr == null
            res.errors |> emplace([[DasError() what = "unable to open file '{config.file}'" ]])
            return
        fmap(fr) <| $(fileCont)

            if config.autoFormat
                res.autoFormat = das_source_formatter::format_source(fileCont)
                return

            var inscope access <- make_file_access(config.projectFile)
            access |> set_file_source(config.originalFile, string(fileCont))
            access |> init_file_access(config.fileAccessRoots)

            using() <| $(var mg : ModuleGroup)
                var cp = CodeOfPolicies()
                cp.ignore_shared_modules = config.ignoreSharedModules
                cp.no_global_variables = config.noGlobalVariables
                cp.no_unused_block_arguments = config.noUnusedBlockArguments
                cp.no_unused_function_arguments = config.noUnusedFunctionArguments
                cp.fail_on_lack_of_aot_export = config.failOnLackOfAotExport
                cp.version_2_syntax = config.version2syntax
                cp.gen2_make_syntax = config.gen2MakeSyntax
                cp.export_all = true
                cp.aot_module = true
                cp.completion = true
                cp.no_optimizations = true
                try
                    compile_file(config.originalFile, access, unsafe(addr(mg)), cp) <| $(ok, program, error)


                        if ok && program != null
                            var inscope completion <- CompletionContext()

                            var allDeps : table<uint64>

                            program |> for_each_require_declaration() <| $(mod, name, file, isPublic, at)
                                var modReq <- [[ModuleRequirement()
                                    mod = mod != null ? "{mod.name}" : "",
                                    req = "{name}",
                                    isPublic = isPublic
                                ]]
                                allDeps |> insert(intptr(mod))
                                modReq |> fill_at(at)
                                modReq.file = "{file}"
                                if mod != null
                                    var allMods : table<uint64>
                                    var mods : array<tuple<mod : Module?; depth : int>>
                                    allMods |> insert(intptr(mod))
                                    mods |> push([[auto mod, 0]]) // root dep is public for local module
                                    while length(mods) > 0
                                        var mod2 = mods |> back()
                                        mods |> pop()
                                        mod2.mod |> module_for_each_dependency() <| $(dep, isPublic2)
                                            return if !isPublic2 || dep == null

                                            modReq.dependencies |> emplace([[ModDeps() mod = "{dep.name}", depth = mod2.depth + 1]])
                                            allDeps |> insert(intptr(dep))
                                            if !allMods |> key_exists(intptr(dep))
                                                mods |> push([[auto dep, mod2.depth + 1]])
                                                allMods |> insert(intptr(dep))

                                res.requirements |> emplace(modReq)

                            allDeps |> insert(intptr(program |> get_this_module()))

                            // if config.globalCompletion
                            // if true
                            //     program_for_each_registered_module() <| $(mod)
                            //         res.allMods |> push("{mod.name}")
                            //         if !is_builtin(mod)
                            //             return
                            //         if key_exists(processed, "{mod.name}")
                            //             return
                            //         completion |> parse_module(mod)
                            //         processed |> insert("{mod.name}")

                            // else

                            program_for_each_registered_module() <| $(mod)
                                if mod |> is_builtin()
                                    res.builtinMods |> push("{mod.name}")

                            var processed : table<string>
                            if true
                                program |> get_ptr() |> for_each_module() <| $(mod)
                                    // res.progMods |> push("{mod.name}")
                                    // global completion now contains only $(builtin) module
                                    if config.globalCompletion && mod.name != "$"
                                        return
                                    if !config.globalCompletion && (!key_exists(allDeps, intptr(mod)) || mod.name == "$")
                                        return
                                    if key_exists(processed, "{mod.name}")
                                        return
                                    completion |> parse_module(mod)
                                    processed |> insert("{mod.name}")

                            // res.processedMods <- [for it in keys(processed); it]

                            var visitor <- new AstData()
                            visitor.res = unsafe(addr(res))
                            visitor.completion = unsafe(addr(completion))
                            var inscope adapter <- make_visitor(*visitor)
                            program |> visit(adapter)
                            unsafe
                                delete visitor

                            res.completion <- completion.result

                        if program == null
                            res.errors |> emplace([[DasError() what = "{error}", file=config.file ]])
                            return

                        for err in program.errors
                            res.errors |> emplace([[DasError()
                                what = "{err.what}",
                                extra = "{err.extra}",
                                fixme = "{err.fixme}",
                                file=err.at.fileInfo != null ? "{err.at.fileInfo.name}" : "",
                                line = err.at.line |> int(),
                                column = err.at.column |> int(),
                                lineEnd = err.at.last_line |> int(),
                                columnEnd = err.at.last_column |> int(),
                                cerr = int(err.cerr),
                            ]])
                recover
                    let msg = empty(this_context().exception) ? "failed to compile" : "failed to compile: {this_context().exception}"
                    res.errors |> emplace([[DasError() what = msg, file=config.file ]])


struct DasToken : CompletionAt
    declAt : CompletionAt = [[CompletionAt() line = -1, column = -1]]
    kind : string
    mod : string
    name : string
    value : string
    alias : string
    tdk : string
    parentTdk : string
    isUnused : bool
    isConst : bool
    // ptr : string


[unused_argument(completion)] def DasToken(expr; var completion : CompletionContext ?) : DasToken
    var token = DasToken()
    token.kind = "{expr.__rtti}"
    if expr._type != null
        token.tdk = TypeDeclKey(expr._type)
        token.alias = expr._type.alias |> string()
        token.isConst = expr._type.flags.constant
        *completion |> parse_typedecl(token.tdk, expr._type, false)
    token |> fill_at(expr.at)
    // token.ptr = intptr(expr |> get_ptr())
    return token

class AstData : AstVisitor
    [[do_not_delete]] res : ValidationResult?
    [[do_not_delete]] completion : CompletionContext?
    skipExprs : array<bool>
    visitedExprs : table<uint64>
    visitedTypedecls : table<uint64>
    // def override preVisitFunction(fun:FunctionPtr) : void
    //     debug(fun)
    // def override preVisitExprTypeDecl(expr:smart_ptr<ExprTypeDecl>) : void
    //     if expr == null
    //         return
    //     let tdk = TypeDeclKey(expr._type)
    //     let foundNew = *completion |> parse_typedecl(tdk, expr._type)
    //     print("{tdk} new? {foundNew}\n")

    // TODO: enum
    // TODO: variant
    // TODO: tuple
    // TODO: typedef

    def annotation_to_tokens(annotations : AnnotationList)
        for ann in annotations
            let annType = (ann.annotation.isBasicStructureAnnotation ? "BasicStructureAnnotation" :
                ann.annotation.isFunctionAnnotation ? "FunctionAnnotation" :
                ann.annotation.isStructureTypeAnnotation ? "StructureTypeAnnotation" :
                ann.annotation.isStructureAnnotation ? "StructureAnnotation" :
                ann.annotation.isTypeAnnotation ? "TypeAnnotation" :
                "Annotation")
            var token <- DasToken()
            token.kind = "annotation"
            token.name = annType
            token.mod = ann.annotation._module != null ? "{ann.annotation._module.name}" : ""
            token |> fill_at(ann.at)
            res.tokens |> emplace(token)

    // visitedStructures : table<uint64>

    def override preVisitStructure(str : StructurePtr) : void
        if str == null
            return
        str.annotations |> annotation_to_tokens()
        let mod = str._module != null ? "{str._module.name}" : ""
        var token <- DasToken()
        token.kind = "struct"
        token.name = "{str.name}"
        token.mod = mod
        token |> fill_at(str.at)
        res.tokens |> emplace(token)

        let stName = str._module != null ? "{str._module.name}::{str.name}" : "{str.name}"

        var inscope fields : table<string>
        var parent = str.parent
        while parent != null
            for fld in parent.fields
                fields |> insert("{fld.name}")
            parent = parent.parent

        for fld in str.fields
            if fields.key_exists("{fld.name}")
                continue
            var token2 <- DasToken()
            token2.kind = "field"
            token2.name = "{fld.name}"
            token2.tdk = TypeDeclKey(fld._type)
            token2.isConst = fld._type.flags.constant
            *completion |> parse_typedecl(token2.tdk, fld._type, false)
            token2.mod = mod
            token2 |> fill_at(fld.at)
            res.tokens |> emplace(token2)


    [unused_argument(index, last)] def override preVisitExprMakeStructField(expr : smart_ptr<ExprMakeStruct>; index : int; decl : MakeFieldDeclPtr; last : bool) : void
        if length(skipExprs) > 0 || expr == null || expr.genFlags.generated
            return
        var token <- DasToken()
        token.kind = "fieldDecl"
        token.name = "{decl.name}"
        token.value = decl.value |> describe()
        token.tdk = TypeDeclKey(decl.value._type)
        token.parentTdk = TypeDeclKey(expr._type)
        // token.isConst = decl.flags.
        *completion |> parse_typedecl(token.tdk, decl.value._type, false)
        token |> fill_at(expr.at)
        // token.declAt |> fill_at(decl.at)
        res.tokens |> emplace(token)


    def preVisitTypeDecl_(typ : TypeDeclPtr) : int
        if typ == null
            return -1
        if key_exists(visitedTypedecls, intptr(typ))
            return -1
        visitedTypedecls |> insert(intptr(typ))
        let tdk = TypeDeclKey(typ)
        *completion |> parse_typedecl(tdk, typ, false)
        var token <- DasToken()
        token.kind = "typedecl"
        // token.name = tdk
        token.tdk = tdk
        token.alias = typ.alias |> string()
        token.mod = typ._module != null ? "{typ._module.name}" : ""
        token.isConst = typ.flags.constant
        token |> fill_at(typ.at)
        res.tokens |> emplace(token)
        return length(res.tokens) - 1

    ignoreNextTypeDecl : bool

    def override preVisitTypeDecl(typ : TypeDeclPtr) : void
        if ignoreNextTypeDecl
            return
        preVisitTypeDecl_(typ)

    [unused_argument(name)] def override preVisitAlias(typ : TypeDeclPtr; name : das_string) : void
        ignoreNextTypeDecl = true
        let idx = preVisitTypeDecl_(typ)
        if idx >= 0
            // res.tokens[idx].name = "{name}"
            res.tokens[idx].alias = "{name}"

    [unused_argument(name)] def override visitAlias(var typ : TypeDeclPtr; name : das_string) : TypeDeclPtr
        ignoreNextTypeDecl = false
        return <- typ


    def override preVisitExpression(expr : ExpressionPtr) : void
        let ptr = intptr(expr |> get_ptr())
        if key_exists(visitedExprs, ptr)
            return
        visitedExprs |> insert(ptr)

        if length(skipExprs) > 0 || expr == null || expr.genFlags.generated
            return
        if expr is ExprBlock || expr is ExprMakeBlock // ignore blocks, huge amount of text, no value
            return
        if expr is ExprFor || expr is ExprIfThenElse // ignore too
            return
        if expr is ExprFakeLineInfo || expr is ExprFakeContext // generated exprs
            return
        if expr is ExprCall || expr is ExprLet || expr is ExprVar || expr is ExprOp1 // will be parsed separately
            return
        if expr is ExprRef2Value // ignore, we need sub exprs
            return
        var token <- DasToken(expr, completion)
        if expr is ExprField
            let field = expr as ExprField
            token.name = "{field.name}"
            if field.field != null
                token.declAt |> fill_at(field.field.at)
                if field.field._type != null
                    token.tdk = TypeDeclKey(field.field._type)
                    token.isConst = field.field._type.flags.constant
                    *completion |> parse_typedecl(token.tdk, field.field._type, false)
        elif expr is ExprAddr
            let exprAddr = expr as ExprAddr
            token.name = "{exprAddr.target}"
            if exprAddr.func != null
                token.declAt |> fill_at(exprAddr.func.at)
                if exprAddr.func.result != null
                    let resTdk = TypeDeclKey(exprAddr.func.result)
                //     token.isConst = exprAddr.func.result.flags.constant
                    *completion |> parse_typedecl(resTdk, exprAddr.func.result, false)
        elif expr is ExprGoto
            let gt = expr as ExprGoto
            token.name = "{gt.labelName}"
        elif expr is ExprLabel
            let lbl = expr as ExprLabel
            token.name = "{lbl.labelName}"
        else
            token.name = expr |> describe()
        res.tokens |> emplace(token)

    // def preVisitExprLooksLikeCall_(expr:ExprLooksLikeCall?): void
    //     if length(skipExprs) > 0 || expr == null || expr.genFlags.generated
    //         return
    //     var token <- DasToken(expr, completion)
    //     token.name = "{expr.name}"
    //     token.value = "def {expr.name}"
    //     res.tokens |> emplace(token)

    // def override preVisitExprLooksLikeCall(expr : smart_ptr<ExprLooksLikeCall>) : void
    //     if length(skipExprs) > 0 || expr == null || expr.genFlags.generated
    //         return
    //     preVisitExprLooksLikeCall_(expr |> get_ptr())

    def preVisitExprCallFunc(expr : ExprCallFunc?) : void
        if length(skipExprs) > 0 || expr == null || expr.genFlags.generated
            return
        var token <- DasToken(expr, completion)
        token.name = "{expr.name}" |> fix_fn_name()
        token.value = "def {token.name}("
        if expr.func != null
            token.mod = expr.func._module != null ? "{expr.func._module.name}" : ""
            token.declAt |> fill_at(expr.func.at)
            var first = true
            for a in expr.func.arguments
                token.value += first ? "" : ", "
                first = false
                token.value += "{a.name}"
                if a._aka != ""
                    token.value += " aka {a._aka}"
                if a._type != null
                    let atdk = TypeDeclKey(a._type)
                    token.value += ": {atdk}"
                    *completion |> parse_typedecl(atdk, a._type, false)
                if a.init != null
                    token.value += " = {a.init |> describe()}"
            token.value += ")"
            if expr.func.result != null
                let restdk = TypeDeclKey(expr.func.result)
                token.value += ": {restdk}"
                *completion |> parse_typedecl(restdk, expr.func.result, false)
        res.tokens |> emplace(token)

    def override preVisitExprCall(expr : smart_ptr<ExprCall>) : void
        if length(skipExprs) > 0 || expr == null || expr.genFlags.generated
            return
        preVisitExprCallFunc(expr |> get_ptr())

    def override preVisitExprLet(expr : smart_ptr<ExprLet>) : void
        if length(skipExprs) > 0 || expr == null || expr.genFlags.generated
            return
        for v in expr.variables
            var token <- DasToken(expr, completion)
            token.name = "{v.name}"
            if v._type != null
                token.tdk = TypeDeclKey(v._type)
                token.alias = v._type.alias |> string()
                token.isConst = v._type.flags.constant
                *completion |> parse_typedecl(token.tdk, v._type, false)
            token |> fill_at(v.at)
            if v.init != null
                token.value = v.init |> describe()
            token.mod = v._module != null ? "{v._module.name}" : ""
            token.isUnused = !v.flags.marked_used && v.isAccessUnused
            res.tokens |> emplace(token)

    [unused_argument(last)] def override preVisitExprForVariable(expr : smart_ptr<ExprFor>; svar : VariablePtr; last : bool) : void
        if length(skipExprs) > 0 || expr == null || expr.genFlags.generated || svar.flags.generated
            return
        var token <- DasToken(expr, completion)
        token.name = "{svar.name}"
        if svar._type != null
            token.tdk = TypeDeclKey(svar._type)
            token.alias = svar._type.alias |> string()
            token.isConst = svar._type.flags.constant
        *completion |> parse_typedecl(token.tdk, svar._type, false)
        token |> fill_at(svar.at)
        if svar.init != null
            token.value = svar.init |> describe()
        token.mod = svar._module != null ? "{svar._module.name}" : ""
        token.isUnused = !svar.flags.marked_used && svar.isAccessUnused
        res.tokens |> emplace(token)


    def override preVisitExprVar(expr : smart_ptr<ExprVar>) : void
        if length(skipExprs) > 0 || expr == null || expr.genFlags.generated
            return
        if expr.variable == null || expr.variable.flags.generated
            return
        var token <- DasToken(expr, completion)
        token.name = "{expr.variable.name}"
        token.declAt |> fill_at(expr.variable.at)
        token.mod = expr.variable._module != null ? "{expr.variable._module.name}" : ""
        if token.mod == ""
            token.mod = funcModule
        if expr.variable.init != null
            token.value = expr.variable.init |> describe()
        if expr.variable._type != null
            token.tdk = TypeDeclKey(expr.variable._type)
            token.isConst = expr.variable._type.flags.constant
            token.alias = expr.variable._type.alias |> string()
            *completion |> parse_typedecl(token.tdk, expr.variable._type, false)
        // if expr._type != null
        //     token.isConst = expr._type.flags.constant
        //     token.tdk = TypeDeclKey(expr._type)
        //     *completion |> parse_typedecl(token.tdk, expr._type)
        res.tokens |> emplace(token)

    // this code breaks array comprehension hints, see generateComprehension(...) for more info
    // def override preVisitExprBlock(blk : smart_ptr<ExprBlock>) : void
    //     if blk.genFlags.generated
    //         skipExprs |> push(true)
    //         return
    //     if length(skipExprs) > 0 
    //         return    

    // def override visitExprBlock(var blk:smart_ptr<ExprBlock>) : ExpressionPtr
    //     if blk.genFlags.generated
    //         skipExprs |> pop()
    //     return <- blk

    funcModule : string

    def override preVisitFunction(fun : FunctionPtr) : void
        if fun.flags.generated && !fun.flags._generator && !fun.flags._lambda
            skipExprs |> push(true)
            return
        delete skipExprs
        if fun.flags._lambda
            return
        let genericFunc = fun.isGeneric

        fun.annotations |> annotation_to_tokens()

        var token <- DasToken()
        token.kind = "func"
        token |> fill_at(fun.at)
        token.name = "{fun.name}" |> fix_fn_name()
        if fun.result != null
            let tdk = TypeDeclKey(fun.result)
            token.tdk = tdk
            token.alias = fun.result.alias |> string()
            token.isConst = fun.result.flags.constant
            *completion |> parse_typedecl(tdk, fun.result, genericFunc)
        token.mod = fun._module != null ? "{fun._module.name}" : ""
        funcModule = token.mod
        token.declAt |> fill_at(fun.atDecl)
        token.value = "def {token.name}("
        var first = true
        for a in fun.arguments
            token.value += first ? "" : ", "
            first = false
            token.value += "{a.name}"
            if a._aka != ""
                token.value += " aka {a._aka}"
            if a._type != null
                let atdk = TypeDeclKey(a._type)
                token.value += ": {atdk}"
                *completion |> parse_typedecl(atdk, a._type, genericFunc)
            if a.init != null
                token.value += " = {a.init |> describe()}"
        token.value += ")"
        if fun.result != null
            let restdk = TypeDeclKey(fun.result)
            token.value += ": {restdk}"
            *completion |> parse_typedecl(restdk, fun.result, genericFunc)
        res.tokens |> emplace(token)


    [unused_argument(lastArg)] def override preVisitFunctionArgument(fun : FunctionPtr; arg : VariablePtr; lastArg : bool) : void
        if length(skipExprs) > 0 || fun == null || fun.flags.generated || arg.flags.generated
            return
        var token <- DasToken()
        token.kind = "func_arg"
        token.name = "{arg.name}"
        if arg.init != null
            token.value = "{arg.init |> describe()}"
        token.tdk = TypeDeclKey(arg._type)
        token.alias = arg._type.alias |> string()
        token.isConst = arg._type.flags.constant
        *completion |> parse_typedecl(token.tdk, arg._type, fun.isGeneric)
        token.mod = fun._module != null ? "{fun._module.name}" : ""
        token |> fill_at(arg.at)
        res.tokens |> emplace(token)

    def override visitFunction(var fun : FunctionPtr) : FunctionPtr
        delete skipExprs
        return <- fun

    [unused_argument(lastArg)] def override preVisitExprBlockArgument(blk : smart_ptr<ExprBlock>; arg : VariablePtr; lastArg : bool) : void
        if length(skipExprs) > 0 || blk == null || blk.genFlags.generated || arg.flags.generated
            return
        blk.annotations |> annotation_to_tokens()
        var token <- DasToken()
        token.kind = "block_arg"
        token.name = "{arg.name}"
        if arg.init != null
            token.value = "{arg.init |> describe()}"
        token.tdk = TypeDeclKey(arg._type)
        token.alias = arg._type.alias |> string()
        token.isConst = arg._type.flags.constant
        *completion |> parse_typedecl(token.tdk, arg._type, false)
        token.mod = arg._module != null ? "{arg._module.name}" : ""
        if token.mod == ""
            token.mod = funcModule
        token |> fill_at(arg.at)
        res.tokens |> emplace(token)

    def override preVisitExprOp1(expr : smart_ptr<ExprOp1>) : void
        if length(skipExprs) > 0 || expr == null || expr.genFlags.generated
            return
        preVisitExprCallFunc(expr |> get_ptr())

    def override preVisitExprOp2(expr : smart_ptr<ExprOp2>) : void
        if length(skipExprs) > 0 || expr == null || expr.genFlags.generated
            return
        preVisitExprCallFunc(expr |> get_ptr())

    // def override preVisitExprTypeInfo(expr : smart_ptr<ExprTypeInfo>) : void
    //     if length(skipExprs) > 0 || expr == null || expr.genFlags.generated
    //         return
    //     if length(skipExprs) > 0 || expr == null || expr.genFlags.generated
    //         return
    //     var name = "typeinfo"
    //     var desc = "{name}("
    //     if !empty(expr.trait)
    //         desc += "{expr.trait}"
    //     if !empty(expr.subtrait)
    //         desc += "<{expr.subtrait}"
    //         if !empty(expr.extratrait)
    //             desc += "; {expr.extratrait}"
    //         desc += "> "
    //     else
    //         desc += " "
    //     var typ = expr._type != null ? describe(expr._type) : ""
    //     if expr.subexpr != null && expr.subexpr._type != null
    //         desc += "arg1 : {describe(expr.subexpr._type)}"
    //     elif expr.typeexpr != null
    //         desc += "type<{describe(expr.typeexpr)}>"
    //     desc += ")"
    //     if typ != ""
    //         desc += " : {typ}"
    //     var token <- DasToken()
    //     token.kind = "typeinfo"
    //     token.name = name
    //     token.value = desc
    //     token.tdk = TypeDeclKey(expr._type)
    //     token.isConst = expr._type.flags.constant
    //     *completion |> parse_typedecl(token.tdk, expr._type)
    //     token |> fill_at(expr.at)
    //     res.tokens |> emplace(token)


    // def override preVisitTypeDecl(typ:TypeDeclPtr) : void
    //     debug(typ)
    // TODO:
    // - variables
    // - functions
    // - operators (Op1, Op2)

    def override preVisitExprAssume(expr : smart_ptr<ExprAssume>) : void
        if length(skipExprs) > 0 || expr == null || expr.genFlags.generated
            return
        var token <- DasToken(expr, completion)
        token.name = "{expr.alias}"
        if expr.subexpr != null
            token.value = "{expr.subexpr |> describe()}"
            token |> fill_at(expr.subexpr.at)
            if expr.subexpr._type != null
                token.tdk = TypeDeclKey(expr.subexpr._type)
                token.isConst = expr.subexpr._type.flags.constant
                token.alias = expr.subexpr._type.alias |> string()
                *completion |> parse_typedecl(token.tdk, expr.subexpr._type, false)
        token |> fill_at(expr.at)
        res.tokens |> emplace(token)
