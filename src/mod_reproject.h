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
#include <png.h>

// signatures in big endian, to autodetect tile type
#define PNG_SIG 0x89504e47
#define JPEG_SIG 0xffd8ffe0
#define LERC_SIG 0x436e745a
// This one is not a type, just an encoding
#define GZIP_SIG 0x436e745a

#define READ_RIGHTS APR_FOPEN_READ | APR_FOPEN_BINARY | APR_FOPEN_LARGEFILE

// Size of receive buffer, can be made larger
#define DEFAULT_INPUT_SIZE 1024*1024

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

// Separate channels and level, just in case
struct sz {
    apr_int64_t x, y, z, c, l;
};

struct bbox_t {
    double xmin, ymin, xmax, ymax;
};

struct rset {
    // Resolution in units per pixel
    double rx, ry;
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
    const char *projection;
    bbox_t bbox;
};

struct  repro_conf {
    // http_root path of this configuration
    const char *doc_path;

    // The output and input raster figures
    TiledRaster raster, inraster;

    // local web path to redirect the source requests
    const char *source, *postfix;

    // array of guard regexp, one of them has to match
    apr_array_header_t *regexp;

    // Output mime-type, default is JPEG
    const char *mime_type;
    // ETag initializer
    apr_uint64_t seed;
    // Buffer for the emtpy tile etag
    char eETag[16];
    // Empty tile buffer, if provided
    apr_uint32_t *empty;
    // Size of empty tile, in bytes
    apr_size_t esize;
    apr_off_t eoffset;

    // Meaning depends on format
    double quality;

    // What is the buffer size for retrieving tiles
    apr_size_t max_input_size;
    // What is the buffer size for outgoing tiles
    apr_size_t max_output_size;

    // Choose a lower res input instead of a higher one
    int oversample;

    // Is mod_reproject configured in this path
    int enabled;
};

extern module AP_MODULE_DECLARE_DATA reproject_module;

typedef struct {
    char *buffer;
    int size;
} storage_manager;

// Any codec needs a static place for an error message and a line stride when decoding
struct codec_params {
    char error_message[1024];
    apr_uint32_t line_stride;
};

// In JPEG_codec.cpp
// raster defines the expected tile
// src contains the input JPEG
// buffer is the location of the first byte on the first line of decoded data
// line_stride is the size of a line in buffer (larger or equal to decoded JPEG line)
// Returns NULL if everything looks fine, or an error message
struct jpeg_params : codec_params {
    int quality;
};

const char *jpeg_stride_decode(jpeg_params &params, const TiledRaster &raster, storage_manager &src,
    void *buffer);
const char *jpeg_encode(jpeg_params &params, const TiledRaster &raster, storage_manager &src,
    storage_manager &dst);

struct png_params : codec_params {
    // As defined by PNG
    int color_type, bit_depth;
    // 0 to 9
    int compression_level;
    // If true, NDV is the transparent color
    int has_transparency;
    // If has_transparency, this is the transparent color definition
    png_color_16 NDV;
};

// In PNG_codec.cpp
// raster defines the expected tile
// src contains the input PNG
// buffer is the location of the first byte on the first line of decoded data
// line_stride is the size of a line in buffer (larger or equal to decoded PNG line)
// Returns NULL if everything looks fine, or an error message
const char *png_stride_decode(png_params &params, const TiledRaster &raster, 
    storage_manager &src, void *buffer);
const char *png_encode(png_params &params, const TiledRaster &raster, 
    storage_manager &src, storage_manager &dst);

#endif
