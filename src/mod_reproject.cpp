/*
 * mod_reproject.cpp
 * An AHTSE tile to tile conversion module, should do most of the functionality required by a WMS server
 * Uses a 3-4 paramter rest tile service as a data source
 * Currently not functional, working on affine scalling only (pan and zoom)
 *
 * (C) Lucian Plesea 2016
 */

#include "mod_reproject.h"
#include <cmath>
#include <clocale>

// From mod_receive
#include <receive_context.h>

using namespace std;

#define USER_AGENT "AHTSE Reproject"

static int send_image(request_rec *r, apr_uint32_t *buffer, apr_size_t size, char *mime_type = NULL)
{
    if (mime_type)
        ap_set_content_type(r, mime_type);
    else
        switch (hton32(*buffer)) {
        case JPEG_SIG:
            ap_set_content_type(r, "image/jpeg");
            break;
        case PNG_SIG:
            ap_set_content_type(r, "image/png");
            break;
        default: // LERC goes here too
            ap_set_content_type(r, "application/octet-stream");
    }
    // Is it gzipped content?
    if (GZIP_SIG == hton32(*buffer))
        apr_table_setn(r->headers_out, "Content-Encoding", "gzip");

    // TODO: Set headers, as chosen by user
    ap_set_content_length(r, size);
    ap_rwrite(buffer, size, r);
    return OK;
}

// Returns NULL if it worked as expected, returns a four integer value from "x y", "x y z" or "x y z c"
static char *get_xyzc_size(struct sz *size, const char *value) {
    char *s;
    if (!value)
        return " values missing";
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
        return " incorrect format";
    return NULL;
}

// Converts a 64bit value into 13 trigesimal chars
static void uint64tobase32(apr_uint64_t value, char *buffer, int flag = 0) {
    static char b32digits[] = "0123456789abcdefghijklmnopqrstuv";
    // From the bottom up
    buffer[13] = 0; // End of string marker
    for (int i = 0; i < 13; i++, value >>= 5)
        buffer[12 - i] = b32digits[value & 0x1f];
    buffer[0] |= flag << 4; // empty flag goes in top bit
}

// Return the value from a base 32 character
// Returns a negative value if char is not a valid base32 char
// ASCII only
static int b32(unsigned char c) {
    if (c < '0') return -1;
    if (c - '0' < 10) return c - '0';
    if (c < 'A') return -1;
    if (c - 'A' < 22) return c - 'A' + 10;
    if (c < 'a') return -1;
    if (c - 'a' < 22) return c - 'a' + 10;
    return -1;
}

static apr_uint64_t base32decode(unsigned char *s, int *flag) {
    apr_int64_t value = 0;
    while (*s == '"') s++; // Skip initial quotes
    *flag = (b32(*s) >> 4 ) & 1; // Pick up the flag from bit 5
    for (int v = b32(*s++) & 0xf; v >= 0; v = b32(*s++))
        value = (value << 5) + v;
    return value;
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
        *err_message = apr_psprintf(pool, " %s - %pm", fname, &s);
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
        *err_message = apr_psprintf(pool, " %s lines should be smaller than %d",
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
    level.width = int(1 + (raster.size.x - 1) / raster.pagesize.x);
    level.height = int(1 + (raster.size.y - 1) / raster.pagesize.y);
    level.rx = (raster.bbox.xmax - raster.bbox.xmin) / raster.size.x;
    level.ry = (raster.bbox.ymax - raster.bbox.ymin) / raster.size.y;

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
        level.rx *= 2;
        level.ry *= 2;
    }

    // MRF has one tile at the top
    ap_assert(raster.rsets[0].height == 1 && raster.rsets[0].width == 1);
    ap_assert(raster.n_levels > raster.skip);
}

// Temporary switch locale to C, get four comma separated numbers in a bounding box, WMS style
static char *getbbox(const char *line, bbox_t *bbox)
{
    const char *lcl = setlocale(LC_NUMERIC, NULL);
    char *message = " format incorrect, expects four comma separated C locale numbers";
    char *l;
    setlocale(LC_NUMERIC, "C");

    do {
        bbox->xmin = strtod(line, &l); if (*l++ != ',') break;
        bbox->ymin = strtod(l, &l);    if (*l++ != ',') break;
        bbox->xmax = strtod(l, &l);    if (*l++ != ',') break;
        bbox->ymax = strtod(l, &l);
        message = NULL;
    } while (false);

    setlocale(LC_NUMERIC, lcl);
    return message;
}


static char *ConfigRaster(apr_pool_t *p, apr_table_t *kvp, struct TiledRaster &raster)
{
    const char *line;
    line = apr_table_get(kvp, "Size");
    if (!line)
        return "Size directive is mandatory";
    const char *err_message;
    err_message = get_xyzc_size(&(raster.size), line);
    if (err_message) return apr_pstrcat(p, "Size", err_message, NULL);
    // Optional page size, defaults to 512x512
    raster.pagesize.x = raster.pagesize.y = 512;
    line = apr_table_get(kvp, "PageSize");
    if (line) {
        err_message = get_xyzc_size(&(raster.pagesize), line);
        if (err_message) return apr_pstrcat(p, "PageSize", err_message, NULL);
    }

    line = apr_table_get(kvp, "SkippedLevels");
    if (line)
        raster.skip = int(apr_atoi64(line));

    // Default projection is WM, meaning web mercator
    line = apr_table_get(kvp, "Projection");
    raster.projection = line ? apr_pstrdup(p, line) : "WM";

    // Bounding box: minx, miny, maxx, maxy
    raster.bbox.xmin = raster.bbox.ymin = 0.0;
    raster.bbox.xmax = raster.bbox.ymax = 1.0;
    line = apr_table_get(kvp, "BoundingBox");
    if (line)
        err_message = getbbox(line, &raster.bbox);
    if (err_message)
        return apr_pstrcat(p, "BoundingBox", err_message, NULL);

    init_rsets(p, raster);

    return NULL;
}

static char *read_empty_tile(cmd_parms *cmd, repro_conf *c, const char *line)
{
    // If we're provided a file name or a size, pre-read the empty tile in the 
    apr_file_t *efile;
    apr_off_t offset = c->eoffset;
    apr_status_t stat;
    char *last;

    c->esize = (apr_size_t)apr_strtoi64(line, &last, 0);
    // Might be an offset, or offset then file name
    if (last != line)
        apr_strtoff(&(c->eoffset), last, &last, 0);
    
    while (*last && isblank(*last)) last++;
    const char *efname = last;

    // Use the temp pool for the file open, it will close it for us
    if (!c->esize) { // Don't know the size, get it from the file
        apr_finfo_t finfo;
        stat = apr_stat(&finfo, efname, APR_FINFO_CSIZE, cmd->temp_pool);
        if (APR_SUCCESS != stat)
            return apr_psprintf(cmd->pool, "Can't stat %s %pm", efname, stat);
        c->esize = (apr_size_t)finfo.csize;
    }
    stat = apr_file_open(&efile, efname, READ_RIGHTS, 0, cmd->temp_pool);
    if (APR_SUCCESS != stat)
        return apr_psprintf(cmd->pool, "Can't open empty file %s, %pm", efname, stat);
    c->empty = (apr_uint32_t *)apr_palloc(cmd->pool, (apr_size_t)c->esize);
    stat = apr_file_seek(efile, APR_SET, &offset);
    if (APR_SUCCESS != stat)
        return apr_psprintf(cmd->pool, "Can't seek empty tile %s: %pm", efname, stat);
    apr_size_t size = (apr_size_t)c->esize;
    stat = apr_file_read(efile, c->empty, &size);
    if (APR_SUCCESS != stat)
        return apr_psprintf(cmd->pool, "Can't read from %s: %pm", efname, stat);
    apr_file_close(efile);
    return NULL;
}

static const char *read_config(cmd_parms *cmd, repro_conf *c, const char *src, const char *fname)
{
    char *err_message;
    const char *line;

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

    // Output mime type
    line = apr_table_get(kvp, "MimeType");
    c->mime_type = (line) ? apr_pstrdup(cmd->pool, line) : "image/jpeg";

    // Oversample flag
    line = apr_table_get(kvp, "Oversample");
    c->oversample = (line != NULL);

    // Allow for one or more RegExp guard
    // One of them has to match if the request is to be considered
    line = apr_table_get(kvp, "RegExp");
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

    line = apr_table_get(kvp, "ETagSeed");
    // Ignore the flag
    int flag;
    c->seed = line ? base32decode((unsigned char *)line, &flag) : 0;
    // Set the missing tile etag, with the extra bit set
    uint64tobase32(c->seed, c->eETag, 1);

    // EmptyTile, default to nothing
    line = apr_table_get(kvp, "EmptyTile");
    if (line) {
        err_message = read_empty_tile(cmd, c, line);
        if (err_message) return err_message;
    }

    line = apr_table_get(kvp, "InputBufferSize");
    c->max_input_size = DEFAULT_INPUT_SIZE;
    if (line) 
        c->max_input_size = (apr_size_t)apr_strtoi64(line, NULL, 0);

    line = apr_table_get(kvp, "OutputBufferSize");
    c->max_output_size = DEFAULT_INPUT_SIZE;
    if (line)
        c->max_output_size = (apr_size_t)apr_strtoi64(line, NULL, 0);

    line = apr_table_get(kvp, "SourcePath");
    if (!line)
        return "SourcePath directive is missing";
    c->source = apr_pstrdup(cmd->pool, line);

    c->quality = 75.0; // Default for JPEG
    line = apr_table_get(kvp, "Quality");
    if (line)
        c->quality = strtod(line, NULL);

    // Enabled if we got this far
    c->enabled = TRUE;
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

static int etag_matches(request_rec *r, const char *ETag) {
    const char *ETagIn = apr_table_get(r->headers_in, "If-None-Match");
    return ETagIn != 0 && strstr(ETagIn, ETag);
}

// Returns the empty tile if defined
static int send_empty_tile(request_rec *r) {
    repro_conf *cfg = (repro_conf *)ap_get_module_config(r->per_dir_config, &reproject_module);
    if (etag_matches(r, cfg->eETag)) {
        apr_table_setn(r->headers_out, "ETag", cfg->eETag);
        return HTTP_NOT_MODIFIED;
    }

    if (!cfg->empty) return DECLINED;
    return send_image(r, cfg->empty, cfg->esize);
}

// Returns a bad request error if condition is met
#define REQ_ERR_IF(X) if (X) {\
    return HTTP_BAD_REQUEST; \
}

// If the condition is met, sends the message to the error log and returns HTTP INTERNAL ERROR
#define SERR_IF(X, msg) if (X) { \
    ap_log_error(APLOG_MARK, APLOG_ERR, 0, r->server, msg);\
    return HTTP_INTERNAL_SERVER_ERROR; \
}

// Pick an input level based on desired output resolution
// TODO: Consider the Y resolution too
static int input_level(struct TiledRaster &raster, double res, int over) {
    // The raster levels are in increasing resolution order, test until 
    for (int choice = raster.skip; choice < raster.n_levels; choice++) {
        double cres = raster.rsets[choice].rx;
        cres -= cres / raster.pagesize.x / 2; // Add half pixel worth to avoid jitter noise
        if (cres < res) { // This is the best choice, we will return
            if (over) choice -= 1; // Use the lower resolution if oversampling
            if (choice < raster.skip)
                return raster.skip;
            return choice;
        }
    }
    // If we didn't make a choice yet, use the highest resolution input
    return raster.n_levels -1;
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

// From a bounding box, calculate the top-left and bottom-right tiles of a specific level of a raster
// Input level is absolute, the one set in output tiles is relative
static void bbox_to_tile(const TiledRaster &raster, int level, const bbox_t &bb, sz *tl_tile, sz *br_tile) {
    double rx = raster.rsets[level].rx;
    double ry = raster.rsets[level].ry;
    double x = (bb.xmin - raster.bbox.xmin) / (rx * raster.pagesize.x);
    double y = (raster.bbox.ymax - bb.ymax) / (ry * raster.pagesize.y);

    // Truncate is fine for these two, after adding quarter pixel to eliminate jitter
    tl_tile->x = int(x + rx / 4);
    tl_tile->y = int(y + ry / 4);

    x = (bb.xmax - raster.bbox.xmin) / (rx * raster.pagesize.x);
    y = (raster.bbox.ymax - bb.ymin) / (ry * raster.pagesize.y);

    // Pad these quarter pixel to avoid jitter
    br_tile->x = int(x + rx / 4);
    br_tile->y = int(y + ry / 4);
    if (x - br_tile->x > rx / 2) br_tile->x++;
    if (y - br_tile->y > ry / 2) br_tile->y++;
}


// Returns APR_SUCCESS if everything is fine, otherwise an HTTP error code
// Fetches and decodes all tiles between tl and br, writes output in buffer
// aligned as a single raster
static apr_status_t retrieve_source(request_rec *r, const  sz &tl, const sz &br, void **buffer)
{
    repro_conf *cfg = (repro_conf *)ap_get_module_config(r->per_dir_config, &reproject_module);
    const char *error_message;

    int ntiles = int((br.x - tl.x) * (br.y - tl.y));
    // Should have a reasonable number of input tiles, 64 is a good figure
    SERR_IF(ntiles > 64, "Too many input tiles required, maximum is 64");

    // Allocate a buffer for receiving responses
    receive_ctx rctx;
    rctx.maxsize = cfg->max_input_size;
    rctx.buffer = (char *)apr_palloc(r->pool, rctx.maxsize);

    ap_filter_t *rf = ap_add_output_filter("Receive", &rctx, r, r->connection);

    // Assume data type is byte for now, raster->pagesize.c has to be set correctly
    int input_line_width = int(cfg->inraster.pagesize.x * cfg->raster.pagesize.c);
    int pagesize = int(input_line_width * cfg->inraster.pagesize.y);
    int line_stride = int((br.x - tl.x) * input_line_width);
    apr_size_t bufsize = pagesize * ntiles;
    if (*buffer == NULL) // Allocate the buffer if not provided, filled with zeros
        *buffer = apr_pcalloc(r->pool, bufsize);

    // Retrieve every required tile and decompress it in the right place
    for (int y = int(tl.y); y < br.y; y++) for (int x = int(tl.x); x < br.x; x++) {
        char *sub_uri = (tl.z == 0) ?
            apr_psprintf(r->pool, "%s/%d/%d/%d", cfg->source, int(tl.l), y, x) :
            apr_psprintf(r->pool, "%s/%d/%d/%d/%d", cfg->source, int(tl.z), int(tl.l), y, x);
        request_rec *rr = ap_sub_req_lookup_uri(sub_uri, r, r->output_filters);

        // Location of first byte of this input tile
        void *b = (char *)(*buffer) + pagesize * (y - tl.y) * (br.x - tl.x) + input_line_width * (x - tl.x);

        // Set up user agent signature, prepend the info
        const char *user_agent = apr_table_get(r->headers_in, "User-Agent");
        user_agent = user_agent == NULL ? USER_AGENT :
            apr_pstrcat(r->pool, USER_AGENT ", ", user_agent, NULL);
        apr_table_setn(rr->headers_in, "User-Agent", user_agent);

        rctx.size = 0; // Reset the receive size
        int rr_status = ap_run_sub_req(rr);
        if (rr_status != APR_SUCCESS) {
            ap_remove_output_filter(rf);
            ap_log_rerror(APLOG_MARK, APLOG_ERR, rr_status, r, "Receive failed for %s", sub_uri);
            return rr_status; // Pass status along
        }

        apr_uint32_t sig;
        memcpy(&sig, rctx.buffer, sizeof(sig));

        switch (hton32(sig))
        {
        case JPEG_SIG:
            error_message = jpeg_stride_decode(cfg->inraster, (storage_manager &)(rctx), b, line_stride);
            break;
        default:
            error_message = apr_pstrcat(r->pool, "Unsupported format received from ", sub_uri, NULL);
        }

        if (error_message != NULL) {
            ap_log_rerror(APLOG_MARK, APLOG_ERR, 0, r, "%s from :%s", error_message, sub_uri);
            // Something went wrong
            return HTTP_NOT_FOUND;
        }
    }

    ap_remove_output_filter(rf);
    apr_table_clear(r->headers_out); // Clean up the headers set by subrequests

    return APR_SUCCESS;
}

// Interpolation line, contains the above line and the relative weight (never zero)
typedef struct {
    // w is Weigth of next line, can be 0 but not 256.
    // line is the higher line to be interpolated, always positive
    unsigned int line : 24, w:8;
} iline;

// Offset should be Out - In, center of first pixels, real world coordinates
// If this is negative, we got trouble?
static int init_ilines(double delta_in, double delta_out, double offset, iline *itable, int lines)
{
    for (int i = 0; i < lines; i++) {
        double pos = (offset + i * delta_out) / delta_in;
        // The high line
        itable[i].line = static_cast<int>(ceil(pos));
        // Weight of high line, under 256
        itable[i].w = static_cast<int>(floor(256.0 * (pos - floor(pos))));
    }
    return 0;
}

// Adjust an interpolation table to avoid addressing unavailable lines
// Max available is the max available line
static void adjust_itable(iline *table, int n, int max_avail) {
    // Adjust the end first
    while (table[--n].line > max_avail) {
        table[n].line = max_avail;
        table[n].w = 255; // Mostly the last available line
    }
    for (int i = 0; table[i].line == 0; i++) {
        table[i].line = 1;
        table[i].w = 0; // Use line zero value
    }
}

// An typed interpolation buffer
template<typename T>
struct interpolation_buffer {
    T *buffer;          // Location of first value per line
    sz size;            // Describes the organization of the buffer
};


//
// Perform the actual interpolation, using the working type WT
// IMPROVEMENT: interpolate could reuse lines of H values.
//
template<typename T, typename WT = int> static void interpolate(
    const interpolation_buffer<T> &src, interpolation_buffer<T> &dst,
    iline *h = NULL, iline *v = NULL)
{
    // input and output number of channels should be the same
    ap_assert(src.size.c == dst.size.c);
    T *data = dst.buffer;
    const int colors = dst.size.c;
    // Source line width
    const int slw = src.size.x * colors;
    // Destination line width
    const int dlw = dst.size.x * colors;
    for (int y = 0; y < dst.size.y; y++) {
        unsigned int vw = v[y].w;
        for (int x = 0; x < dst.size.x; x++)
        {
            unsigned int hw = h[x].w;
            int idx = slw * v[y].line + h[x].line * colors; // Top left index
            for (int c = 0; c < colors; c++) {
                WT hi = static_cast<WT>(src.buffer[idx + c]) * hw + 
                    static_cast<WT>(src.buffer[idx + c - colors]) * (256 - hw);
                WT lo = static_cast<WT>(src.buffer[idx + c - slw]) * hw + 
                    static_cast<WT>(src.buffer[idx + c - slw - colors]) * (256 - hw);
                // Now interpolate the high and low vertical weight
                WT value = hi * vw + lo * (256 - vw);
                // The value is multipled by 256^2 because each interpolation is times 256
                // Make sure the working type is large enough to eliminate overflow
                *data++ = static_cast<T>(value / (256 * 256));
            }
        }
    }
}

// Interpolate input to output
// TODO: Consider the ry
static int affine_interpolate(request_rec *r, const sz *out_tile, const sz *tl, const sz *br,
    void *src, storage_manager &dst)
{
    repro_conf *cfg = (repro_conf *)ap_get_module_config(r->per_dir_config, &reproject_module);
    const double out_rx = cfg->raster.rsets[out_tile->l].rx;
    const double out_ry = cfg->raster.rsets[out_tile->l].ry;
    const int input_l = input_level(cfg->inraster, out_rx, cfg->oversample);
    const double in_rx = cfg->inraster.rsets[input_l].rx;
    const double in_ry = cfg->inraster.rsets[input_l].ry;
    double offset;

    // Input and output interpolation buffers
    interpolation_buffer<unsigned char> ib = 
        { reinterpret_cast<unsigned char *>(src), cfg->inraster.pagesize};
        // Input size is a multiple of input pagesize
    ib.size.x *= (br->x - tl->x);
    ib.size.y *= (br->y - tl->y);
    interpolation_buffer<unsigned char> ob = 
        { reinterpret_cast<unsigned char *>(dst.buffer), cfg->raster.pagesize};

    // Compute the bounding boxes
    bbox_t out_bbox, in_bbox;
    tile_to_bbox(cfg->raster, out_tile, out_bbox);
    tile_to_bbox(cfg->inraster, tl, in_bbox);

    // Allocate space for the interpolation tables, for both the x and the y
    iline *table = (iline *)apr_palloc(r->pool, sizeof(iline)*ob.size.x + ob.size.y);
    iline *h_table = table;
    iline *v_table = table + ob.size.x;
    offset = out_bbox.xmin - in_bbox.xmin + (out_rx - in_rx) / 2.0;
    init_ilines(in_rx, out_rx, offset, h_table, ob.size.x);
    offset = out_bbox.ymax - in_bbox.ymax + (out_ry - in_ry) / 2.0;
    init_ilines(in_ry, out_ry, offset, v_table, ob.size.y);

    // Adjust the interpolation tables to avoid addressing pixels outside of the bounding box
    adjust_itable(h_table, ob.size.x, ib.size.x - 1);
    adjust_itable(v_table, ob.size.y, ib.size.y - 1);

    interpolate(ib, ob, h_table, v_table);

    return APR_SUCCESS;
}

// Projection is not changed, only scaling
// TODO: Consider ry
static int affine_scaling_handler(request_rec *r, sz *tile) {
    repro_conf *cfg = (repro_conf *)ap_get_module_config(r->per_dir_config, &reproject_module);
    double out_rx = cfg->raster.rsets[tile->l].rx;

    // The absolute input level
    int input_l = input_level(cfg->inraster, out_rx, cfg->oversample);

    // Compute the output tile bounding box
    bbox_t bbox;
    sz tl_tile, br_tile;

    // Convert output tile to bbox, then to input tile range
    tile_to_bbox(cfg->raster, tile, bbox);  
    bbox_to_tile(cfg->inraster, input_l, bbox, &tl_tile, &br_tile);

    // Complete the setup, pass the z and adjust the level
    tl_tile.z = br_tile.z = tile->z;
    // Copy the c from input
    tl_tile.c = br_tile.c = cfg->inraster.pagesize.c;
    tl_tile.l = br_tile.l = input_l - cfg->inraster.skip;

    void *buffer = NULL;
    apr_status_t status = retrieve_source(r, tl_tile, br_tile, &buffer);

    if (status != APR_SUCCESS)
        return status;

    // TODO: Resample and reassemble in output raw buffer
    storage_manager src, dst;
    dst.size = cfg->max_output_size;
    dst.buffer = (char *)apr_palloc(r->pool, (apr_size_t)dst.size);
    src.buffer = (char *)buffer;
    src.size = (int)(cfg->raster.pagesize.x * cfg->raster.pagesize.y * cfg->raster.pagesize.c);

    // If only one tile was read and input and output page matches, it's a tile transform, not scaling
    if (!(tl_tile.x == br_tile.x && tl_tile.y == br_tile.y
        && cfg->raster.pagesize.x == cfg->inraster.pagesize.x 
        && cfg->raster.pagesize.y == cfg->inraster.pagesize.y))
    {   // Need to do scaling, get a real output buffer
        src.buffer = (char *)apr_palloc(r->pool, src.size);
        affine_interpolate(r, tile, &tl_tile, &br_tile, buffer, src);
    }

    const char *error_message = jpeg_encode(cfg->raster, src, dst, cfg->quality);
    if (error_message) {
        ap_log_rerror(APLOG_MARK, APLOG_ERR, 0, r, "%s from :%s", error_message, r->uri);
        // Something went wrong if JPEG compression fails
        return HTTP_INTERNAL_SERVER_ERROR;
    }
    
    return send_image(r, (apr_uint32_t *)dst.buffer, dst.size, cfg->mime_type);
}

// If projection is the same, the transformation is an affine scaling
#define IS_AFFINE_SCALING(cfg) !apr_strnatcasecmp(cfg->inraster.projection, cfg->raster.projection)

static int handler(request_rec *r)
{
    // TODO: use r->header_only to verify ETags, assuming the subrequests are faster in that mode
    if (r->method_number != M_GET) return DECLINED;
    if (r->args) return DECLINED; // Don't accept arguments

    repro_conf *cfg = (repro_conf *)ap_get_module_config(r->per_dir_config, &reproject_module);
    if (!cfg->enabled) return DECLINED;

    if (cfg->regexp) { // Check the guard regexps if they exist, matches agains URL
        int i;
        char * url_to_match = r->args ? apr_pstrcat(r->pool, r->uri, "?", r->args, NULL) : r->uri;
        for (i = 0; i < cfg->regexp->nelts; i++) {
            ap_regex_t *m = &APR_ARRAY_IDX(cfg->regexp, i, ap_regex_t);
            if (!ap_regexec(m, url_to_match, 0, NULL, 0)) break; // Found
        }
        if (i == cfg->regexp->nelts) // No match found
            return DECLINED;
    }


    apr_array_header_t *tokens = tokenize(r->pool, r->uri);
    if (tokens->nelts < 3) return DECLINED; // At least Level Row Column

    // Use a xyzc structure, with c being the level
    // Input order is M/Level/Row/Column, with M being optional
    struct sz tile;
    memset(&tile, 0, sizeof(tile));

    // Need at least three numerical arguments
    tile.x = apr_atoi64(*(char **)apr_array_pop(tokens)); REQ_ERR_IF(errno);
    tile.y = apr_atoi64(*(char **)apr_array_pop(tokens)); REQ_ERR_IF(errno);
    tile.l = apr_atoi64(*(char **)apr_array_pop(tokens)); REQ_ERR_IF(errno);

    // We can ignore the error on this one, defaults to zero
    // The parameter before the level can't start with a digit for an extra-dimensional MRF
    if (cfg->raster.size.z != 1 && tokens->nelts)
        tile.z = apr_atoi64(*(char **)apr_array_pop(tokens));

    // Don't allow access to levels less than zero, send the empty tile instead
    if (tile.l < 0)
        return send_empty_tile(r);

    // Adjust the level to what the input is
    tile.l += cfg->raster.skip;

    // Need to have mod_receive available
    SERR_IF(!ap_get_output_filter_handle("Receive"), "mod_receive not installed");

    if (IS_AFFINE_SCALING(cfg))
        return affine_scaling_handler(r, &tile);

    // TODO: Rest of the handler
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