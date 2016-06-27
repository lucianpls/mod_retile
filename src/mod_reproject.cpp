/*
*
*
*/

#include "mod_reproject.h"
#include <cmath>

// Returns NULL if it worked as expected, returns a four integer value from "x y", "x y z" or "x y z c"
static char *get_xyzc_size(struct sz *size, const char *value) {
    char *s;
    if (!value)
        return "values missing";
    size->x = apr_strtoi64(value, &s, 0);
    size->y = apr_strtoi64(s, &s, 0);
    size->c = 3;
    size->z = 1;
    if (errno == 0 && *s) { // Read optional third and fourth integers
        size->z = apr_strtoi64(s, &s, 0);
        if (*s)
            size->c = apr_strtoi64(s, &s, 0);
    } // Raster size is 4 params max
    if (errno || *s)
        return "incorrect format";
    return NULL;
}

static void *create_dir_config(apr_pool_t *p, char *path)
{
    repro_conf *c = (repro_conf *)apr_pcalloc(p, sizeof(repro_conf));
    c->doc_path = path;
    return c;
}

// Returns a table read from a file, or NULL and an error message
static apr_table_t *read_pKVP_from_file(apr_pool_t *pool, const char *fname, char **err_message)

{
    // Should parse it here and initialize the configuration structure
    err_message = NULL;
    ap_configfile_t *cfg_file;
    apr_status_t s = ap_pcfg_openfile(&cfg_file, pool, fname);

    if (APR_SUCCESS != s) { // %pm means print status error string
        *err_message = apr_psprintf(pool, "%s - %pm", fname, &s);
        return NULL;
    }

    char buffer[MAX_STRING_LEN];
    apr_table_t *table = apr_table_make(pool, 8);
    // This can return ENOSPC if lines are too long
    while (APR_SUCCESS == (s = ap_cfg_getline(buffer, MAX_STRING_LEN, cfg_file))) {
        if ((strlen(buffer) == 0) || buffer[0] == '#')
            continue;
        const char *value = buffer;
        char *key = ap_getword_white(pool, &value);
        apr_table_add(table, key, value);
    }

    ap_cfg_closefile(cfg_file);
    if (s == APR_ENOSPC) {
        *err_message = apr_psprintf(pool, "%s lines should be smaller than %d",
            fname, MAX_STRING_LEN);
        return NULL;
    }

    return table;
}

static void init_rsets(apr_pool_t *p, struct TiledRaster &raster)
{
    // Clean up pagesize defaults
    raster.pagesize.c = raster.size.c;
    raster.pagesize.z = 1;

    struct rset level;
    level.width = 1 + (raster.size.x - 1) / raster.pagesize.x;
    level.height = 1 + (raster.size.y - 1) / raster.pagesize.y;
    // How many levels do we have
    raster.n_levels = 2 + ilogb(max(level.height, level.width) - 1);
    raster.rsets = (struct rset *)apr_pcalloc(p, sizeof(rset) * raster.n_levels);

    // Populate rsets from the bottom, the way tile protcols count levels
    // These are MRF rsets, not all of them are visible
    struct rset *r = raster.rsets + raster.n_levels - 1;
    for (int i = 0; i < raster.n_levels; i++) {
        *r-- = level;
        // Prepare for the next level, assuming powers of two
        level.width = 1 + (level.width - 1) / 2;
        level.height = 1 + (level.height - 1) / 2;
    }

    // MRF has one tile at the top
    ap_assert(raster.rsets[0].height == 1 && raster.rsets[0].width == 1);
}

static char *ConfigRaster(apr_pool_t *p, apr_table_t *kvp, struct TiledRaster &raster)
{
    const char *line;
    line = apr_table_get(kvp, "Size");
    if (!line)
        return "Size directive is mandatory";
    const char *err_message;
    err_message = get_xyzc_size(&(raster.size), line);
    if (err_message) return apr_pstrcat(p, "Size ", err_message, NULL);
    // Optional page size, defaults to 512x512
    raster.pagesize.x = raster.pagesize.y = 512;
    line = apr_table_get(kvp, "PageSize");
    if (line) {
        err_message = get_xyzc_size(&(raster.pagesize), line);
        if (err_message) return apr_pstrcat(p, "PageSize ", err_message, NULL);
    }

    line = apr_table_get(kvp, "SkippedLevels");
    if (line)
        raster.skip = apr_atoi64(line);

    // Default projection is WM, meaning web mercator
    line = apr_table_get(kvp, "Projection");
    raster.projection = line ? apr_pstrdup(p, line) : "WM";

    init_rsets(p, raster);
    return NULL;
}

static const char *read_config(cmd_parms *cmd, repro_conf *c, const char *src, const char *fname)
{
    char *err_message;

    // Start with the source configuration
    apr_table_t *kvp = read_pKVP_from_file(cmd->temp_pool, src, &err_message);
    if (NULL == kvp) return err_message;

    err_message = ConfigRaster(cmd->pool, kvp, c->inraster);
    if (err_message) return apr_pstrcat(cmd->pool, "Source ", err_message, NULL);

    // Then the real configuration file
    kvp = read_pKVP_from_file(cmd->temp_pool, fname, &err_message);
    if (NULL == kvp) return err_message;
    err_message = ConfigRaster(cmd->pool, kvp, c->raster);
    if (err_message) return err_message;

    // Allow for one or more RegExp guard
    // One of them has to match if the request is to be considered
    const char *line = apr_table_get(kvp, "RegExp");
    if (line) {
        if (c->regexp == 0)
            c->regexp = apr_array_make(cmd->pool, 2, sizeof(ap_regex_t));
        ap_regex_t *m = (ap_regex_t *)apr_array_push(c->regexp);
        int error = ap_regcomp(m, line, 0);
        if (error) {
            int msize = 2048;
            err_message = (char *)apr_pcalloc(cmd->pool, msize);
            ap_regerror(error, m, err_message, msize);
            return apr_pstrcat(cmd->pool, "MRF Regexp incorrect ", err_message);
        }
    }

    return NULL;
}

//
// Tokenize a string into an array
//  
static apr_array_header_t* tokenize(apr_pool_t *p, const char *s, char sep = '/')
{
    apr_array_header_t* arr = apr_array_make(p, 10, sizeof(char *));
    while (sep == *s) s++;
    char *val;
    while (*s && (val = ap_getword(p, &s, sep))) {
        char **newelt = (char **)apr_array_push(arr);
        *newelt = val;
    }
    return arr;
}

#define REQ_ERR_IF(X) if (X) {\
    return HTTP_BAD_REQUEST; \
}

static int handler(request_rec *r)
{
    if (r->method_number != M_GET) return DECLINED;
    if (r->args) return DECLINED; // Don't accept arguments

    repro_conf *cfg = (repro_conf *)ap_get_module_config(r->per_dir_config, &reproject_module);

    if (cfg->regexp) { // Check the guard regexps if they exist, matches agains URL
        int i;
        char * url_to_match = r->args ? apr_pstrcat(r->pool, r->uri, "?", r->args, NULL) : r->uri;
        for (i = 0; i < cfg->regexp->nelts; i++) {
            ap_regex_t *m = &APR_ARRAY_IDX(cfg->regexp, i, ap_regex_t);
            if (ap_regexec(m, url_to_match, 0, NULL, 0)) continue; // Not matched
            break;
        }
        if (i == cfg->regexp->nelts) // No match found
            return DECLINED;
    }

    apr_array_header_t *tokens = tokenize(r->pool, r->uri);
    if (tokens->nelts < 3) return DECLINED; // At least Level Row Column

    // Use a xyzc structure, with c being the level
    // Input order is M/Level/Row/Column, with M being optional
    sz tile;
    memset(&tile, 0, sizeof(tile));

    // Need at least three numerical arguments
    tile.x = apr_atoi64(*(char **)apr_array_pop(tokens)); REQ_ERR_IF(errno);
    tile.y = apr_atoi64(*(char **)apr_array_pop(tokens)); REQ_ERR_IF(errno);
    tile.c = apr_atoi64(*(char **)apr_array_pop(tokens)); REQ_ERR_IF(errno);

    // TODO: Handler
    return DECLINED;
}

#undef REQ_ERR_IF

static const command_rec cmds[] =
{
    AP_INIT_TAKE2(
    "Reproject_ConfigurationFiles",
    (cmd_func) read_config, // Callback
    0, // Self-pass argument
    ACCESS_CONF, // availability
    "source then output configuration files"
    ),

    { NULL }
};

static void register_hooks(apr_pool_t *p)
{
    ap_hook_handler(handler, NULL, NULL, APR_HOOK_MIDDLE);
}

module AP_MODULE_DECLARE_DATA reproject_module = {
    STANDARD20_MODULE_STUFF,
    create_dir_config,
    0, // No dir_merge
    0, // No server_config
    0, // No server_merge
    cmds, // configuration directives
    register_hooks // processing hooks
};