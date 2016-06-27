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

// signatures in big endian, to autodetect tile type
#define PNG_SIG 0x89504e47
#define JPEG_SIG 0xffd8ffe0
#define LERC_SIG 0x436e745a

#define READ_RIGHTS APR_FOPEN_READ | APR_FOPEN_BINARY | APR_FOPEN_LARGEFILE

// This one is not a type, just an encoding
#define GZIP_SIG 0x436e745a

// Conversion to and from network order, endianess depenent

#if (APR_IS_BIGENDIAN == 0) // Little endian
#if defined(WIN32) // Windows
#define ntoh32(v) _byteswap_ulong(v)
#define hton32(v) _byteswap_ulong(v)
#define ntoh64(v) _byteswap_uint64(v)
#define hton64(v) _byteswap_uint64(v)
#else // Assume linux
#define ntoh32(v) __builtin_bswap32(v)
#define hton32(v) __builtin_bswap32(v)
#define ntoh64(v) __builtin_bswap64(v)
#define hton64(v) __builtin_bswap64(v)
#endif
#else // Big endian, do nothing
#define ntoh32(v)  (v)
#define ntoh64(v)  (v)
#define hton32(v)  (v)
#define hton64(v)  (v)
#endif

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
    // The output and input raster figures
    struct TiledRaster raster, inraster;

    // http_root path of this configuration
    const char *doc_path;
    // array of guard regexp, one of them has to match
    apr_array_header_t *regexp;

    // Output mime-type, default is JPEG
    char *mime_type;
    // ETag initializer
    apr_uint64_t seed;
    // Buffer for the emtpy tile etag
    char eETag[16];
    // Empty tile buffer, if provided
    apr_uint32_t *empty;
    // Size of empty tile, in bytes
    apr_int64_t esize;
    apr_off_t eoffset;

} repro_conf;

extern module AP_MODULE_DECLARE_DATA reproject_module;

#endif