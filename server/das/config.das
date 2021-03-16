options indenting = 4
options no_aot = true

require daslib/json
require strings
require json_tools
require fio
require lib

struct PluginConfigHovers
    verbose : bool

def PluginConfigHovers(val : JsonValue?)
    return <- [[PluginConfigHovers verbose = job(val, "verbose")]]

struct PluginConfigProject
    roots : array<string>
    file : string

def PluginConfigProject(val : JsonValue?)
    var roots = joj(val, "roots")
    if roots == null
        return <- [[PluginConfigProject]]
    return <- [[PluginConfigProject
        file = jos(val, "file"),
        roots <- [{ for it in roots.value as _array; j_s(it) }]
    ]]

struct PluginConfig
    hovers : PluginConfigHovers
    // compiler : string // unused
    project : PluginConfigProject
    experimental: bool
    // server : { args : array<string> } // unused

def PluginConfig(val : JsonValue?)
    return <- [[PluginConfig
        hovers <- PluginConfigHovers(joj(val, "hovers")),
        project <- PluginConfigProject(joj(val, "project")),
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
        return path
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
