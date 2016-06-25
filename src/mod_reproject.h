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

#include <apr_strings.h>

#if defined(APLOG_USE_MODULE)
APLOG_USE_MODULE(reproject);
#endif

struct sz {
    apr_int64_t x, y, z, c;
};

struct rset {
    // in tiles
    int width;
    // in tiles
    int height;
};

struct TiledRaster {
    struct sz size, pagesize;
    struct rset *rsets;
    int n_levels;
};

typedef struct {
    // http_root path of this configuration
    const char *doc_path;
    struct TiledRaster raster, inraster;
} repro_conf;

extern module AP_MODULE_DECLARE_DATA reproject_module;

#endif