/*
 * mod_reproject.h
 *
 */

#if !defined(MOD_REPROJECT_H)
#include <httpd.h>
#include <http_config.h>
#include <http_main.h>
#include <http_protocol.h>
#include <http_core.h>
#include <http_request.h>
#include <http_log.h>

typedef struct {
    int a;
} repro_conf;

extern module AP_MODULE_DECLARE_DATA reproject_module;

#endif