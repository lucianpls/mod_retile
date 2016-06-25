/*
*
*
*/

#include "mod_reproject.h"

static void *create_dir_config(apr_pool_t *p, char *arg)
{
    return (repro_conf *)apr_pcalloc(p, sizeof(repro_conf));
}

static const char *read_config(cmd_parms *cmd, repro_conf *conf, const char *fname)
{
    return NULL;
}

static int handler(request_rec *r)
{
    return DECLINED;
}

static const command_rec cmds[] =
{
    AP_INIT_TAKE1(
    "Reproject_ConfigurationFile",
    (cmd_func) read_config, // Callback
    0, // Self-pass argument
    ACCESS_CONF, // availability
    "The configuration file for this module"
    ),

    { NULL }
};

static void register_hooks(apr_pool_t *p)
{
    ap_hook_handler(handler, NULL, NULL, APR_HOOK_FIRST);
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