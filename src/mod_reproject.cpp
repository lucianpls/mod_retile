/*
 * mod_reproject.cpp
 * An AHTSE tile to tile conversion module
 * Uses a 3-4 paramter rest tile service as a data source
 *
 * (C) Lucian Plesea 2016-2020
 */

// TODO: Improve endianess support
// TODO: Add LERC support, via libahtse
// TODO: Allow overlap between tiles

#include <ahtse.h>
// From mod_receive
#include <receive_context.h>

#include <httpd.h>
#include <http_config.h>
#include <http_main.h>
#include <http_protocol.h>
#include <http_core.h>
#include <http_request.h>
#include <http_log.h>
#include <apr_strings.h>
#include <vector>

extern module AP_MODULE_DECLARE_DATA reproject_module;

#if defined(APLOG_USE_MODULE)
APLOG_USE_MODULE(reproject);
#endif

NS_AHTSE_USE
using namespace std;

// C++ style, calculate pi once, instead of using the _USE_MATH_DEFINES 
const static double pi = acos(-1.0);

// first param is reverse of radius, second is input coordinate
typedef double coord_conv_f(double, double);

// Identical projection coordinate conversion
static double same_proj(double, double c) {
    return c;
}

// Web mercator X to longitude in degrees
static double wm2lon(double eres, double x) {
    return 360 * eres * x;
}

static double lon2wm(double eres, double lon) {
    return lon / eres / 360;
}

static double m2lon(double eres, double x) {
    return wm2lon(eres, x);
}

static double lon2m(double eres, double lon) {
    return lon2wm(eres, lon);
}

// Web mercator Y to latitude in degrees
static double wm2lat(double eres, double y) {
    return 90 * (1 - 4 / pi * atan(exp(eres * pi * 2 * -y)));
}

// Goes out of bounds close to the poles, valid latitude range is under 85.052
static double lat2wm(double eres, double lat) {
    if (abs(lat) > 85.052)
        return (lat > 0) ? (0.5 / eres) : (-0.5 / eres); // pi*R or -pi*R
    return log(tan(pi / 4 * (1 + lat / 90))) / eres / 2 / pi;
}

// Mercator, projection EPSG:3395, conversion to WebMercator and degrees
// Earth
const double E = 0.08181919084262149; // sqrt(f * ( 2 - f)), f = 1/298.257223563

static double lat2m(double eres, double lat) {
    // WGS84
    // Real mercator reaches a bit further on earth due to flattening
    if (abs(lat) > 85.052)
        return (lat > 0) ? (0.5 / eres) : (-0.5 / eres); // pi*R or -pi*R
    double s = sin(pi * lat / 180);
    return log(tan((1 + s) / (1 - s) * pow((1 - E * s) / (1 + E * s), E))) / eres / 2 / pi;
}

// The iterative solution, slightly time-consuming
static double m2lat(double eres, double y) {
    // Normalize y
    y *= eres * pi * 2;
    // Starting value, in radians
    double lat = pi / 2 - 2 * atan(exp(-y));
    // Max 10 iterations, it takes about 6 or 7
    for (int i = 0; i < 10; i++) {
        double es = E * sin(lat);
        double nlat = pi / 2 - 2 * atan(exp(-y) * pow((1 - es) / (1 + es), E / 2));
        if (lat == nlat) // Max 
            break; // Normal exit
        lat = nlat;
    }
    return lat * 180 / pi;  // Return the value in degrees
}

// Web mercator to mercator and vice-versa are composite transformations
static double m2wm(double eres, double y) {
    return lat2wm(eres, m2lat(eres, y));
}

static double wm2m(double eres, double y) {
    return lat2m(eres, wm2lat(eres, y));
}

// reprojection codes
typedef enum {
    P_AFFINE = 0, P_GCS2WM, P_WM2GCS, P_WM2M, P_M2WM, P_COUNT
} PCode;

// Tables of reprojection code dependent functions, to dispatch on
// Could be done with a switch, this is more compact and easier to extend
// The order has to match the PCode definitions
static coord_conv_f* cxf[P_COUNT] = { same_proj, wm2lon, lon2wm, same_proj, same_proj };
static coord_conv_f* cyf[P_COUNT] = { same_proj, wm2lat, lat2wm, m2wm, wm2m };

#define USER_AGENT "AHTSE Reproject"

struct  repro_conf {
    // The reprojection function to be used, also used as an enable flag
    PCode code;

    // The output and input raster figures
    TiledRaster raster, inraster;

    // local web path to redirect the source requests
    const char* source, * postfix;

    // array of guard regex pointers, one of them has to match
    apr_array_header_t* arr_rxp;

    // Output mime-type, default is JPEG
    const char* mime_type;
    // ETag initializer
    apr_uint64_t seed;
    // Buffer for the emtpy tile etag
    char eETag[16];

    // Meaning depends on format
    double quality;
    // Normalized earth resolution: 1 / (2 * PI * R)
    double eres;

    // What is the buffer size for retrieving tiles
    apr_size_t max_input_size;
    // What is the buffer size for outgoing tiles
    apr_size_t max_output_size;

    // Choose a lower res input instead of a higher one
    int oversample;
    int max_extra_levels;

    // Use NearNb, not bilinear interpolation
    int nearNb;

    // Flag to turn on transparency for formats that do support it
    int has_transparency;
    int indirect;
};

// A structure for the coordinate information used in the current tile conversion
struct work {
    repro_conf *c;
    // Output bbox
    bbox_t out_bbox;
    // Output bbox in input projection
    bbox_t out_equiv_bbox;
    // Input bounding box
    bbox_t in_bbox;
    // Output tile
    sz out_tile;
    // Input tile range
    sz tl, br;
    // Numerical ETag
    apr_uint64_t seed;
    int in_level;
};

static void *create_dir_config(apr_pool_t *p, char *path) {
    repro_conf *c = reinterpret_cast<repro_conf *>(apr_pcalloc(p, sizeof(repro_conf)));
    return c;
}

// Allow for one or more RegExp guard
// One of them has to match if the request is to be considered
static const char *set_regexp(cmd_parms *cmd, repro_conf *c, const char *pattern) {
    return add_regexp_to_array(cmd->pool, &c->arr_rxp, pattern);
}

// Is the projection GCS
static bool is_gcs(const char *projection) {
    return !apr_strnatcasecmp(projection, "GCS")
        || !apr_strnatcasecmp(projection, "WGS84")
        || !apr_strnatcasecmp(projection, "EPSG:4326");
}

// Is the projection spherical mercator, include the Pseudo Mercator code
static bool is_wm(const char *projection) {
    return !apr_strnatcasecmp(projection, "WM")
        || !apr_strnatcasecmp(projection, "EPSG:3857")  // The current code
        || !apr_strnatcasecmp(projection, "EPSG:3785"); // Wrong code
}

static bool is_m(const char *projection) {
    return !apr_strnatcasecmp(projection, "Mercator")
        || !apr_strnatcasecmp(projection, "EPSG:3395");
}

// If projection is the same, the transformation is an affine scaling
#define IS_AFFINE_SCALING(cfg) (!apr_strnatcasecmp(cfg->inraster.projection, cfg->raster.projection))
#define IS_GCS2WM(cfg) (is_gcs(cfg->inraster.projection) && is_wm(cfg->raster.projection))
#define IS_WM2GCS(cfg) (is_wm(cfg->inraster.projection) && is_gcs(cfg->raster.projection))
#define IS_WM2M(cfg) (is_wm(cfg->inraster.projection) && is_m(cfg->raster.projection))

static int etag_matches(request_rec *r, const char *ETag) {
    const char *ETagIn = apr_table_get(r->headers_in, "If-None-Match");
    return ETagIn != 0 && strstr(ETagIn, ETag);
}

// Pick an input level based on desired output resolution
static int pick_input_level(work &info, double rx, double ry) {
    // The raster levels are in increasing resolution order, test until the best match, both x and y
    int choiceX, choiceY;
    int over = info.c->oversample;

    const TiledRaster &raster = info.c->inraster;
    for (choiceX = 0; choiceX < (raster.n_levels - 1); choiceX++) {
        double cres = raster.rsets[choiceX].rx;
        cres += cres / raster.pagesize.x / 2; // Add half pixel worth to choose matching level
        if (cres < rx) { // This is the better choice
            if (!over) choiceX -= 1; // Use the lower resolution level if not oversampling
            if (choiceX < raster.skip)
                choiceX = raster.skip; // Only use defined levels
            break;
        }
    }

    for (choiceY = 0; choiceY < (raster.n_levels - 1); choiceY++) {
        double cres = raster.rsets[choiceY].ry;
        cres += cres / raster.pagesize.y / 2; // Add half pixel worth to avoid jitter noise
        if (cres < ry) { // This is the best choice
            if (!over) choiceY -= 1; // Use the higher level if oversampling
            if (choiceY < raster.skip)
                choiceY = raster.skip; // Only use defined levels
            break;
        }
    }

    // Pick the higher level number for normal quality
    info.in_level = (choiceX > choiceY) ? choiceX : choiceY;
    // Make choiceX the lower level, to see how far we would be
    if (choiceY < choiceX) choiceX = choiceY;

    // Use min of higher level or low + max extra
    if (info.in_level > choiceX + info.c->max_extra_levels)
        info.in_level = choiceX + info.c->max_extra_levels;

    return info.in_level;
}

// From a tile location, generate a bounding box of a raster
static void tile_to_bbox(const TiledRaster &raster, const sz *tile, bbox_t &bb) {
    double rx = raster.rsets[tile->l].rx;
    double ry = raster.rsets[tile->l].ry;

    // Compute the top left
    bb.xmin = raster.bbox.xmin + tile->x * rx * raster.pagesize.x;
    bb.ymax = raster.bbox.ymax - tile->y * ry * raster.pagesize.y;
    // Adjust for the bottom right
    bb.xmax = bb.xmin + rx * raster.pagesize.x;
    bb.ymin = bb.ymax - ry * raster.pagesize.y;
}

static int ntiles(const sz &tl, const sz &br) {
    return int((br.x - tl.x) * (br.y - tl.y));
}

// From a bounding box, calculate the top-left and bottom-right tiles of a specific level of a raster
// Input level is absolute, the one set in output tiles is relative
static void bbox_to_tile(const TiledRaster &raster, int level, const bbox_t &bb, sz &tl_tile, sz &br_tile) {
    double rx = raster.rsets[level].rx;
    double ry = raster.rsets[level].ry;
    double x = (bb.xmin - raster.bbox.xmin) / (rx * raster.pagesize.x);
    double y = (raster.bbox.ymax - bb.ymax) / (ry * raster.pagesize.y);

    // Truncate is fine for these two, after adding quarter pixel to eliminate jitter
    // X and Y are in pages, so a pixel is 1/pagesize
    tl_tile.x = int(x + 0.25 / raster.pagesize.x);
    tl_tile.y = int(y + 0.25 / raster.pagesize.y);

    x = (bb.xmax - raster.bbox.xmin) / (rx * raster.pagesize.x);
    y = (raster.bbox.ymax - bb.ymin) / (ry * raster.pagesize.y);

    // Pad these quarter pixel to avoid jitter
    br_tile.x = int(x + 0.25 / raster.pagesize.x);
    br_tile.y = int(y + 0.25 / raster.pagesize.y);
    // Use a tile only if we get more than half pixel in
    if (x - br_tile.x > 0.5 / raster.pagesize.x) br_tile.x++;
    if (y - br_tile.y > 0.5 / raster.pagesize.y) br_tile.y++;
}

// Fetches and decodes all tiles between tl and br, writes output in buffer
// aligned as a single raster
// Returns APR_SUCCESS if everything is fine, otherwise an HTTP error code
static apr_status_t retrieve_source(request_rec *r, work &info, void **buffer)
{
    const  sz &tl = info.tl, &br = info.br;
    repro_conf *cfg = info.c;
    apr_uint64_t &etag_out = info.seed;
    const char *error_message;

    // a reasonable number of input tiles, 64 is a good figure
    int nt = ntiles(tl, br);
    SERVER_ERR_IF(nt > 6, r, "Too many input tiles required, maximum is 64");

    // Allocate a buffer for receiving responses, gets reused
    storage_manager src;
    src.size = static_cast<int>(cfg->max_input_size);
    src.buffer = reinterpret_cast<char*>(apr_palloc(r->pool, src.size));

    int pixel_size = GDTGetSize(cfg->inraster.datatype);

    // inraster->pagesize.c has to be set correctly
    int input_line_width = int(cfg->inraster.pagesize.x * cfg->inraster.pagesize.c * pixel_size);
    int pagesize = int(input_line_width * cfg->inraster.pagesize.y);

    // Output buffer
    apr_size_t bufsize = static_cast<apr_size_t>(pagesize) * nt;
    if (*buffer == nullptr) // Allocate the buffer if not provided, filled with zeros
        *buffer = apr_pcalloc(r->pool, bufsize);

    // Count of tiles with data
    int count = 0;
    const char* user_agent = apr_table_get(r->headers_in, "User-Agent");
    user_agent = (user_agent == nullptr) ? USER_AGENT :
        apr_pstrcat(r->pool, USER_AGENT ", ", user_agent, NULL);

    // Retrieve every required tile and decompress them in the right place
    for (int y = int(tl.y); y < br.y; y++) for (int x = int(tl.x); x < br.x; x++) {
        char *sub_uri = apr_pstrcat(r->pool,
            (tl.z == 0) ?
            apr_psprintf(r->pool, "%s/%d/%d/%d", cfg->source, int(tl.l), y, x) :
            apr_psprintf(r->pool, "%s/%d/%d/%d/%d", cfg->source, int(tl.z), int(tl.l), y, x),
            cfg->postfix, NULL);

        subr srequest(r);
        srequest.agent = user_agent;

        LOG(r, "Requesting %s", sub_uri);
        src.size = static_cast<int>(cfg->max_input_size);
        auto status = srequest.fetch(sub_uri, src);
        if (status != APR_SUCCESS) {
            if (status == HTTP_NOT_FOUND)
                continue; // Ignore errors
            return status; // Othey type of error, passed through
        }

        apr_uint64_t etag;
        int empty_flag = 0;
        if (!srequest.ETag.empty()) {
            etag = base32decode(srequest.ETag.c_str(), &empty_flag);
            if (empty_flag)
                continue; // Ignore empty input tiles, they don't get counted
        }
        else { // Input came without an ETag, make one up
            etag = src.size; // Start with the input tile size
            // And pick some data out of the input buffer, towards the end
            if (src.size > 50) {
                char *tptr = src.buffer + src.size - 24; // Temporary pointer
                tptr -= reinterpret_cast<apr_uint64_t>(tptr) % 8; // Make it 8 byte aligned
                etag ^= *reinterpret_cast<apr_uint64_t*>(tptr);
                tptr = src.buffer + src.size - 35; // Temporary pointer
                tptr -= reinterpret_cast<apr_uint64_t>(tptr) % 8; // Make it 8 byte aligned
                etag ^= *reinterpret_cast<apr_uint64_t*>(tptr);
            }
        }
        // Build up the outgoing ETag
        etag_out = (etag_out << 8) | (0xff & (etag_out >> 56)); // Rotate existing tag
        etag_out ^= etag; // And combine it with the incoming tile etag

        // Set expected values
        codec_params params;
        memset(&params, 0, sizeof(params));
        params.size = cfg->inraster.pagesize;
        params.dt = cfg->inraster.datatype;
        params.line_stride = int((br.x - tl.x) * input_line_width);

        // Location of first byte of this input tile
        void* b = (char*)(*buffer) + pagesize * (y - tl.y) * (br.x - tl.x)
            + input_line_width * (x - tl.x);

        // This should be moved to libahtse, format independent stride decode
        apr_uint32_t sig = 0;
        memcpy(&sig, src.buffer, sizeof(sig));
        switch (sig)
        {
        case JPEG_SIG:
            error_message = jpeg_stride_decode(params, src, b);
            break;
        case PNG_SIG:
            error_message = png_stride_decode(params, src, b);
            break;
        default:
            error_message = "Unsupported format received";
        }

        if (error_message) { // Something went wrong
            ap_log_rerror(APLOG_MARK, APLOG_ERR, 0, r, "%s decode from :%s", error_message, sub_uri);
            return HTTP_NOT_FOUND;
        }

        count++; // count the valid tiles
    }
    return count ? APR_SUCCESS : HTTP_NOT_FOUND;
}

// Interpolation line, contains the above line and the relative weight (never zero)
// These don't have to be bit fields, but it keeps them smaller
// w is weigth of next line *256, can be 0 but not 256.
// line is the higher line to be interpolated, always positive
struct iline {
    unsigned int w : 8, line : 24;
};

// Offset should be Out - In, center of first pixels, real world coordinates
// If this is negative, we got trouble?
static void init_ilines(double delta_in, double delta_out, double offset, iline *itable, int lines)
{
    for (int i = 0; i < lines; i++) {
        double pos = (offset + i * delta_out) / delta_in;
        // The high line
        itable[i].line = static_cast<int>(ceil(pos));
        if (ceil(pos) != floor(pos))
            itable[i].w = static_cast<int>(floor(256.0 * (pos - floor(pos))));
        else // Perfect match with this line
            itable[i].w = 255;
    }
}

// Adjust an interpolation table to avoid addressing unavailable lines
// Max available is the max available line
static void adjust_itable(iline *table, int n, unsigned int max_avail) {
    // Adjust the end first
    while (n && table[--n].line > max_avail) {
        table[n].line = max_avail;
        table[n].w = 255; // Mostly the last available line
    }
    for (int i = 0; i < n && table[i].line <= 0; i++) {
        table[i].line = 1;
        table[i].w = 0; // Use line zero value
    }
}

// A 2D buffer
struct interpolation_buffer {
    void *buffer;       // Location of first value per line
    sz size;            // Describes the organization of the buffer
};

// Perform the actual interpolation using ilines, working type WT
template<typename T = apr_byte_t, typename WT = apr_int32_t> static void interpolate(
    const interpolation_buffer &src, interpolation_buffer &dst,
    const iline *h, const iline *v)
{
    const int colors = static_cast<int>(dst.size.c);
    ap_assert(src.size.c == colors); // Same number of colors
    T *data = reinterpret_cast<T *>(dst.buffer);
    T *s = reinterpret_cast<T *>(src.buffer);
    const int slw = static_cast<int>(src.size.x * colors);

    // single band optimization
    if (1 == colors) {
        for (int y = 0; y < dst.size.y; y++) {
            const WT vw = v[y].w;
            for (int x = 0; x < dst.size.x; x++)
            {
                const WT hw = h[x].w;
                const int idx = slw * v[y].line + h[x].line; // high left index
                const WT lo = static_cast<WT>(s[idx - slw - 1]) * (256 - hw)
                    + static_cast<WT>(s[idx - slw]) * hw;
                const WT hi = static_cast<WT>(s[idx - 1]) * (256 - hw)
                    + static_cast<WT>(s[idx]) * hw;
                const WT value = hi * vw + lo * (256 - vw);
                *data++ = static_cast<T>(value / (256 * 256));
            }
        }
        return;
    }

    // More than one band
    for (int y = 0; y < dst.size.y; y++) {
        const WT vw = v[y].w;
        for (int x = 0; x < dst.size.x; x++)
        {
            const WT hw = h[x].w;
            int idx = slw * v[y].line + h[x].line * colors; // high left index
            for (int c = 0; c < colors; c++) {
                const WT lo = static_cast<WT>(s[idx + c - slw]) * hw +
                    static_cast<WT>(s[idx + c - slw - colors]) * (256 - hw);
                const WT hi = static_cast<WT>(s[idx + c]) * hw +
                    static_cast<WT>(s[idx + c - colors]) * (256 - hw);
                const WT value = hi * vw + lo * (256 - vw);
                *data++ = static_cast<T>(value / (256 * 256));
            }
        }
    }
}

// NearNb sampling, based on ilines
// Uses the weights to pick between two choices
template<typename T = apr_byte_t> static void interpolateNN(
    const interpolation_buffer &src, interpolation_buffer &dst,
    const iline *h, const iline *v)
{
    ap_assert(src.size.c == dst.size.c);
    T *data = reinterpret_cast<T *>(dst.buffer);
    T *s = reinterpret_cast<T *>(src.buffer);
    const int colors = static_cast<int>(dst.size.c);

    // Precompute the horizontal pick table, the vertical is only used once
    std::vector<int> hpick(static_cast<unsigned int>(dst.size.x));
    for (int i = 0; i < static_cast<int>(hpick.size()); i++)
        hpick[i] = colors * (h[i].line - ((h[i].w < 128) ? 1 : 0));

    if (colors == 1) { // optimization, only two loops
        for (int y = 0; y < static_cast<int>(dst.size.y); y++) {
            int vidx = static_cast<int>(src.size.x * (v[y].line - ((v[y].w < 128) ? 1 : 0)));
            for (auto const &hid : hpick)
                *data++ = s[vidx + hid];
        }
        return;
    }

    for (int y = 0; y < static_cast<int>(dst.size.y); y++) {
        int vidx = static_cast<int>(colors * src.size.x * (v[y].line - ((v[y].w < 128) ? 1 : 0)));
        for (auto const &hid : hpick)
            for (int c = 0; c < colors; c++)
                *data++ = s[vidx + hid + c];
    }
}

// Calls the interpolation for the right data type
void resample(const repro_conf *cfg, const iline *h,
    const interpolation_buffer &src, interpolation_buffer &dst)
{
#define RESAMP(T) if (cfg->nearNb) interpolateNN<T>(src, dst, h, v); else interpolate<T>(src, dst, h, v)
    const iline *v = h + dst.size.x;
    switch (cfg->raster.datatype) {
    case GDT_UInt16:
        RESAMP(apr_uint16_t);
        break;
    case GDT_Int16:
        RESAMP(apr_int16_t);
        break;
    default: // Byte
        RESAMP(apr_byte_t);
    }
#undef RESAMP
}

// The x dimension is most of the time linear, convenience function
static void prep_x(work &info, iline *table) {
    bbox_t &bbox = info.out_equiv_bbox;
    const double out_r = (bbox.xmax - bbox.xmin) / info.c->raster.pagesize.x;
    const double in_r = info.c->inraster.rsets[info.tl.l].rx;
    const double offset = bbox.xmin - info.in_bbox.xmin + 0.5 * (out_r - in_r);
    init_ilines(in_r, out_r, offset, table, static_cast<int>(info.c->raster.pagesize.x));
}

// Initialize ilines for y
// coord_f is the function converting from output coordinates to input
static void prep_y(work &info, iline *table, coord_conv_f coord_f) {
    const int size = static_cast<int>(info.c->raster.pagesize.y);
    const double out_r = (info.out_bbox.ymax - info.out_bbox.ymin) / size;
    const double in_r = info.c->inraster.rsets[info.tl.l].ry;
    double offset = info.in_bbox.ymax - 0.5 * in_r;
    for (int i = 0; i < size; i++) {
        // Coordinate of output line in input projection
        const double coord = coord_f(info.c->eres, info.out_bbox.ymax - out_r * (i + 0.5));
        // Same in pixels
        const double pos = (offset - coord) / in_r;
        table[i].line = static_cast<int>(ceil(pos)); // higher line
        table[i].w = (ceil(pos) == floor(pos)) ? 255 :
            static_cast<int>(floor(256.0 * (pos - floor(pos))));
    }
}

static int handler(request_rec *r)
{
    if (r->method_number != M_GET)
        return DECLINED;

    auto* cfg = get_conf<repro_conf>(r, &reproject_module);
    if (!cfg || cfg->code >= P_COUNT || !cfg->source ||
        (cfg->indirect && r->main == nullptr) ||
        !cfg->arr_rxp || !requestMatches(r, cfg->arr_rxp))
        return DECLINED;

    work info;
    info.c = cfg;
    info.seed = cfg->seed;
    sz& tile = info.out_tile;
    bbox_t& oebb = info.out_equiv_bbox;
    memset(&tile, 0, sizeof(tile));

    if (APR_SUCCESS != getMLRC(r, tile, true))
        return HTTP_BAD_REQUEST;

    if (tile.l < 0)
        return sendEmptyTile(r, cfg->raster.missing);
    tile.l += cfg->raster.skip;

    // Outside of bounds tile returns a not-found error
    if (tile.l >= cfg->raster.n_levels ||
        tile.x >= cfg->raster.rsets[tile.l].w ||
        tile.y >= cfg->raster.rsets[tile.l].h)
        return HTTP_BAD_REQUEST;

    // Need to have mod_receive available
    SERVER_ERR_IF(!ap_get_output_filter_handle("Receive"), r, "mod_receive not installed");

    tile_to_bbox(cfg->raster, &(info.out_tile), info.out_bbox);
    // calculate the input projection equivalent bbox
    oebb.xmin = cxf[cfg->code](cfg->eres, info.out_bbox.xmin);
    oebb.xmax = cxf[cfg->code](cfg->eres, info.out_bbox.xmax);
    oebb.ymin = cyf[cfg->code](cfg->eres, info.out_bbox.ymin);
    oebb.ymax = cyf[cfg->code](cfg->eres, info.out_bbox.ymax);
    double out_equiv_rx = (oebb.xmax - oebb.xmin) / cfg->raster.pagesize.x;
    double out_equiv_ry = (oebb.ymax - oebb.ymin) / cfg->raster.pagesize.y;

    // WM and GCS distortion is under 12:1, this eliminates the case outside of WM
    if (out_equiv_ry < out_equiv_rx / 12)
        return sendEmptyTile(r, cfg->raster.missing);

    // Pick the input level
    int input_l = pick_input_level(info, out_equiv_rx, out_equiv_ry);
    bbox_to_tile(cfg->inraster, input_l, oebb, info.tl, info.br);

    info.tl.z = info.br.z = info.out_tile.z;
    info.tl.c = info.br.c = cfg->inraster.pagesize.c;
    info.tl.l = info.br.l = input_l;
    tile_to_bbox(info.c->inraster, &info.tl, info.in_bbox);
    // Use relative level to request the data
    info.tl.l -= cfg->inraster.skip;
    info.br.l -= cfg->inraster.skip;

    // Incoming tiles buffer
    void *buffer = NULL;
    apr_status_t status = retrieve_source(r, info, &buffer);
    if (APR_SUCCESS != status) return status;
    // back to absolute level for input tiles
    info.tl.l = info.br.l = input_l;

    // Check the etag match before preparing output
    char ETag[16];
    tobase32(info.seed, ETag, info.seed == cfg->seed ? 1 : 0);
    apr_table_set(r->headers_out, "ETag", ETag);
    if (etag_matches(r, ETag))
        return HTTP_NOT_MODIFIED;

    // Outgoing raw tile buffer
    int pixel_size = static_cast<int>(cfg->raster.pagesize.c * GDTGetSize(cfg->raster.datatype));
    storage_manager raw;
    raw.size = static_cast<int>(cfg->raster.pagesize.x * cfg->raster.pagesize.y * pixel_size);
    raw.buffer = static_cast<char *>(apr_palloc(r->pool, raw.size));

    // Set up the input and output 2D interpolation buffers
    interpolation_buffer ib = { buffer, cfg->inraster.pagesize };
    // The input buffer contains multiple input pages
    ib.size.x *= (info.br.x - info.tl.x);
    ib.size.y *= (info.br.y - info.tl.y);
    interpolation_buffer ob = { raw.buffer, cfg->raster.pagesize };

    iline *table = static_cast<iline *>(apr_palloc(r->pool, static_cast<apr_size_t>(sizeof(iline)*(ob.size.x + ob.size.y))));
    iline *ytable = table + ob.size.x;

    // The x dimension scaling is always linear
    prep_x(info, table);
    adjust_itable(table, static_cast<int>(ob.size.x), static_cast<unsigned int>(ib.size.x - 1));
    prep_y(info, ytable, cyf[cfg->code]);
    adjust_itable(ytable, static_cast<int>(ob.size.y), static_cast<unsigned int>(ib.size.y - 1));
    resample(cfg, table, ib, ob);    // Perform the actual resampling

    // A buffer for the output tile
    storage_manager dst;
    dst.size = static_cast<int>(cfg->max_output_size);
    dst.buffer = static_cast<char*>(apr_palloc(r->pool, dst.size));
    const char *error_message = "Unknown output format requested";

    if (NULL == cfg->mime_type || 0 == apr_strnatcmp(cfg->mime_type, "image/jpeg")) {
        jpeg_params params;
        set_jpeg_params(cfg->raster,&params);
        params.quality = static_cast<int>(cfg->quality);
        error_message = jpeg_encode(params, raw, dst);
    }
    else if (0 == apr_strnatcmp(cfg->mime_type, "image/png")) {
        png_params params;
        set_png_params(cfg->raster, &params);
        if (cfg->quality < 10) // Otherwise use the default of 6
            params.compression_level = static_cast<int>(cfg->quality);
        if (cfg->has_transparency)
            params.has_transparency = true;
        error_message = png_encode(params, raw, dst);
    }

    if (error_message) {
        ap_log_rerror(APLOG_MARK, APLOG_ERR, 0, r, "%s encoding :%s", error_message, r->uri);
        // Something went wrong if compression fails
        return HTTP_INTERNAL_SERVER_ERROR;
    }

    apr_table_set(r->headers_out, "ETag", ETag);
    return sendImage(r, dst, cfg->mime_type);
}

static const char *read_config(cmd_parms *cmd, repro_conf *c, const char *src, const char *fname)
{
    const char *err_message, *line;
    // Start with the source configuration
    apr_table_t* kvp = readAHTSEConfig(cmd->temp_pool, src, &err_message);
    if (nullptr == kvp)
        return err_message;
    err_message = const_cast<char*>(configRaster(cmd->pool, kvp, c->inraster));
    if (err_message)
        return err_message;

    // Then the real configuration file
    kvp = readAHTSEConfig(cmd->temp_pool, fname, &err_message);
    if (NULL == kvp) return err_message;
    err_message = const_cast<char *>(configRaster(cmd->pool, kvp, c->raster));
    if (err_message) return err_message;

    // Output mime type, defaults to jpeg
    line = apr_table_get(kvp, "MimeType");
    c->mime_type = (line) ? apr_pstrdup(cmd->pool, line) : "image/jpeg";

    // Get the planet circumference in meters, for partial coverages
    line = apr_table_get(kvp, "Radius");
    // Stored as radius and the inverse of circumference
    double radius = (line) ? strtod(line, NULL) : 6378137.0;
    c->eres = 1.0 / (2 * pi * radius);

    // Sampling flags
    c->oversample = NULL != apr_table_get(kvp, "Oversample");
    c->nearNb = NULL != apr_table_get(kvp, "Nearest");

    line = apr_table_get(kvp, "ExtraLevels");
    c->max_extra_levels = (line) ? int(atoi(line)) : 0;

    line = apr_table_get(kvp, "ETagSeed");
    // Ignore the flag
    int flag;
    c->seed = line ? base32decode(line, &flag) : 0;
    // Set the missing tile etag, with the extra bit set
    tobase32(c->seed, c->eETag, 1);

    // EmptyTile, defaults to pass-through
    line = apr_table_get(kvp, "EmptyTile");
    if (line) {
        err_message = readFile(cmd->pool, c->raster.missing.data, line);
        if (err_message)
            return err_message;
    }

    line = apr_table_get(kvp, "InputBufferSize");
    c->max_input_size = MAX_TILE_SIZE;
    if (line)
        c->max_input_size = static_cast<apr_size_t>(apr_strtoi64(line, nullptr, 0));

    line = apr_table_get(kvp, "OutputBufferSize");
    c->max_output_size = MAX_TILE_SIZE;
    if (line)
        c->max_output_size = static_cast<apr_size_t>(apr_strtoi64(line, nullptr, 0));

    c->quality = 75.0; // Default for JPEG
    line = apr_table_get(kvp, "Quality");
    if (line)
        c->quality = strtod(line, nullptr);

    line = apr_table_get(kvp, "Transparency");
    if (line)
        c->has_transparency = getBool(line);

    // Set the reprojection code
    if (IS_AFFINE_SCALING(c))
        c->code = P_AFFINE;
    else if (IS_GCS2WM(c))
        c->code = P_GCS2WM;
    else if (IS_WM2GCS(c))
        c->code = P_WM2GCS;
    else if (IS_WM2M(c))
        c->code = P_WM2M;
    else
        return "Can't find reprojection function";

    return nullptr;
}

static const command_rec cmds[] =
{
    AP_INIT_TAKE2(
    "Reproject_ConfigurationFiles",
    (cmd_func)read_config, // Callback
    0, // Self-pass argument
    ACCESS_CONF, // availability
    "Required, source and output configuration files"
    ),

    AP_INIT_TAKE1(
    "Reproject_RegExp",
    (cmd_func)set_regexp,
    0, // Self-pass argument
    ACCESS_CONF, // availability
    "One or more, regular expression that the URL has to match"
    ),

    AP_INIT_TAKE1(
    "Reproject_Source",
    (cmd_func)ap_set_string_slot,
    (void *)APR_OFFSETOF(repro_conf, source),
    ACCESS_CONF,
    "Required, internal redirect path for the source"
    ),

    AP_INIT_TAKE1(
    "Reproject_SourcePostfix",
    (cmd_func)ap_set_string_slot,
    (void *)APR_OFFSETOF(repro_conf, postfix),
    ACCESS_CONF,
    "Optional, internal redirect path ending, to be added after the M/L/R/C"
    ),

    AP_INIT_FLAG(
    "Reproject_Indirect",
    (cmd_func) ap_set_flag_slot,
    (void *)APR_OFFSETOF(repro_conf, indirect),
    ACCESS_CONF,
    "If set, the module does not respond to external requests"
    ),

    { NULL }
};

static void register_hooks(apr_pool_t *p) {
    ap_hook_handler(handler, nullptr, nullptr, APR_HOOK_MIDDLE);
}

module AP_MODULE_DECLARE_DATA reproject_module = {
    STANDARD20_MODULE_STUFF,
    create_dir_config,
    NULL, // No dir_merge
    NULL, // No server_config
    NULL, // No server_merge
    cmds, // configuration directives
    register_hooks // processing hooks
};
