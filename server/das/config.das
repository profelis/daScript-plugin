require daslib/json
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
    // server : { args : array<string> } // unused

def PluginConfig(val : JsonValue?)
    return <- [[PluginConfig
        hovers <- PluginConfigHovers(joj(val, "hovers")),
        project <- PluginConfigProject(joj(val, "project"))
    ]]

def resolve_path(path : string; config : PluginConfig) : string
    if path == ""
        return path
    let st = stat(path)
    if st.is_valid
        return path
    for it in config.project.roots
        let newPath = join_path(it, path)
        let newSt = stat(newPath)
        if newSt.is_valid
            return newPath
    let root = get_das_root()
    for it in [[string[] "daslib"; "src/builtin"]]
        let sysPath = join_path(root, it)
        let newPath = join_path(sysPath, path)
        let newSt = stat(newPath)
        if newSt.is_valid
            return newPath
    return path