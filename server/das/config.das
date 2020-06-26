require daslib/json
require json_tools

struct PluginConfigHovers
    verbose : bool

def PluginConfigHovers(val : JsonValue?)
    return <- [[PluginConfigHovers verbose = job(val, "verbose")]]

struct PluginConfigProject
    roots : array<string>

def PluginConfigProject(val : JsonValue?)
    var roots = joj(val, "roots")
    if roots == null
        return <- [[PluginConfigProject]]
    return <- [[PluginConfigProject
        roots <- [{ for it in roots.value as _array; j_s(it) }]
    ]]

struct PluginConfig
    hovers : PluginConfigHovers
    // compiler : string // unused
    project : PluginConfigProject

def PluginConfig(val : JsonValue?)
    return <- [[PluginConfig
        hovers <- PluginConfigHovers(joj(val, "hovers")),
        // compiler = jos(val, "compiler"),
        project <- PluginConfigProject(joj(val, "project"))
    ]]
