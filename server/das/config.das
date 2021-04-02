options indenting = 4
options no_aot = true

require daslib/json
require strings
require json_tools
require fio
require fs_tools

struct PluginConfigPolicies
    noGlobalVariables: bool = false
    noUnusedBlockArguments: bool = false
    noUnusedFunctionArguments: bool = false
    failOnLackOfAotExport: bool = false

def PluginConfigPolicies(val : JsonValue?)
    return <- [[PluginConfigPolicies
        noGlobalVariables = job(val, "no_global_variables"),
        noUnusedBlockArguments = job(val, "no_unused_block_arguments"),
        noUnusedFunctionArguments = job(val, "no_unused_function_arguments"),
        failOnLackOfAotExport = job(val, "fail_on_lack_of_aot_export")
    ]]


struct PluginConfigHovers
    verbose : bool

def PluginConfigHovers(val : JsonValue?)
    return <- [[PluginConfigHovers verbose = job(val, "verbose")]]

struct PluginConfigProject
    roots : array<string>
    file : string
    scanWorkspace: bool

def PluginConfigProject(val : JsonValue?)
    var roots = joj(val, "roots")
    if roots == null
        return <- [[PluginConfigProject]]
    return <- [[PluginConfigProject
        file = jos(val, "file"),
        scanWorkspace = job(val, "scanWorkspace"),
        roots <- [{ for it in roots.value as _array; j_s(it) }]
    ]]

struct PluginConfig
    hovers : PluginConfigHovers
    // compiler : string // unused
    project : PluginConfigProject
    experimental: bool
    policies: PluginConfigPolicies
    // server : { args : array<string> } // unused

def PluginConfig(val : JsonValue?)
    return <- [[PluginConfig
        hovers <- PluginConfigHovers(joj(val, "hovers")),
        project <- PluginConfigProject(joj(val, "project")),
        policies <- PluginConfigPolicies(joj(val, "policies")),
        experimental = job(val, "experimental")
    ]]

[private]
def is_valid(path: string)
    let st = stat(path)
    return st.is_valid

def resolve_path(path : string; config : PluginConfig) : string
    if path |> empty()
        return path
    if path |> is_valid()
        return path |> fix_path()
    for it in config.project.roots
        let newPath = it |> join_path(path)
        if newPath |> is_valid()
            return newPath
    let root = get_das_root()
    for it in [[auto[] "daslib"; "src/builtin"]]
        let newPath = root |> join_path(it) |> join_path(path)
        if newPath |> is_valid()
            return newPath
    return path
