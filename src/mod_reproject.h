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
    int width, height; // In tiles
};

struct TiledRaster {
    // Size and pagesize of the raster
    struct sz size, pagesize;
    // width and height for each pyramid level
    struct rset *rsets;
    // how many levels from full size, computed
    int n_levels;
    // How many levels to skip at the top of the pyramid
    int skip;

    // geographical projection
    char *projection;
};

typedef struct {
    // http_root path of this configuration
    const char *doc_path;
    // array of guard regexp, one of them has to match
    apr_array_header_t *regexp;

    struct TiledRaster raster, inraster;
} repro_conf;

extern module AP_MODULE_DECLARE_DATA reproject_module;

#endif