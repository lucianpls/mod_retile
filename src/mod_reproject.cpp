/*
 * mod_reproject.cpp
 * An AHTSE tile to tile conversion module, should do most of the functionality required by a WMS server
 * Uses a 3-4 paramter rest tile service as a data source
 * Currently not functional, working on affine scalling only (pan and zoom)
 *
 * (C) Lucian Plesea 2016
 */

// TODO: Handle ETag conditional requests
// TODO: Implement GCS to and from WM.  This is partly done, but should reuse more code, use classes?
// TODO: Add LERC support
// TODO: Allow overlap between tiles

#include "mod_reproject.h"
#include <cmath>
#include <clocale>
#include <algorithm>
#include <vector>
#include <cctype>

// From mod_receive
#include <receive_context.h>

using namespace std;

// Rather than use the _USE_MATH_DEFINES, just calculate pi once, C++ style
const static double pi = acos(-1.0);

#define USER_AGENT "AHTSE Reproject"

// Given a data type name, returns a data type
static GDALDataType GetDT(const char *name) {
    if (name == NULL) return GDT_Byte;
    if (!apr_strnatcasecmp(name, "UINT16"))
        return GDT_UInt16;
    else if (!apr_strnatcasecmp(name, "INT16") || !apr_strnatcasecmp(name, "INT"))
        return GDT_Int16;
    else if (!apr_strnatcasecmp(name, "UINT32"))
        return GDT_UInt32;
    else if (!apr_strnatcasecmp(name, "INT32") || !apr_strnatcasecmp(name, "INT"))
        return GDT_Int32;
    else if (!apr_strnatcasecmp(name, "FLOAT32") || !apr_strnatcasecmp(name, "FLOAT"))
        return GDT_Float32;
    else if (!apr_strnatcasecmp(name, "FLOAT64") || !apr_strnatcasecmp(name, "DOUBLE"))
        return GDT_Float64;
    else
        return GDT_Byte;
}

static int send_image(request_rec *r, apr_uint32_t *buffer, apr_size_t size, const char *mime_type = NULL)
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
static const char *get_xyzc_size(struct sz *size, const char *value) {
    char *s;
    if (!value)
        return " values missing";
    size->x = apr_strtoi64(value, &s, 0);
    size->y = apr_strtoi64(s, &s, 0);
    size->c = 3;
    size->z = 1;
    if (errno == 0 && *s != 0) {
        // Read optional third and fourth integers
        size->z = apr_strtoi64(s, &s, 0);
        if (*s != 0)
            size->c = apr_strtoi64(s, &s, 0);
    }
    if (errno != 0 || *s != 0) {
        // Raster size is 4 params max
        return " incorrect format";
    }
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
    *err_message = NULL;
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
        *err_message = apr_psprintf(pool, "maximum line length of %d exceeded", MAX_STRING_LEN);
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
static const char *getbbox(const char *line, bbox_t *bbox)
{
    const char *lcl = setlocale(LC_NUMERIC, NULL);
    const char *message = " format incorrect, expects four comma separated C locale numbers";
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

static const char *ConfigRaster(apr_pool_t *p, apr_table_t *kvp, struct TiledRaster &raster)
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

    // Optional data type, defaults to unsigned byte
    raster.datatype = GetDT(apr_table_get(kvp, "DataType"));

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

// Allow for one or more RegExp guard
// One of them has to match if the request is to be considered
static const char *set_regexp(cmd_parms *cmd, repro_conf *c, const char *pattern)
{
    char *err_message = NULL;
    if (c->regexp == 0)
        c->regexp = apr_array_make(cmd->pool, 2, sizeof(ap_regex_t));
    ap_regex_t *m = (ap_regex_t *)apr_array_push(c->regexp);
    int error = ap_regcomp(m, pattern, 0);
    if (error) {
        int msize = 2048;
        err_message = (char *)apr_pcalloc(cmd->pool, msize);
        ap_regerror(error, m, err_message, msize);
        return apr_pstrcat(cmd->pool, "Reproject Regexp incorrect ", err_message, NULL);
    }
    return NULL;
}

static const char *read_config(cmd_parms *cmd, repro_conf *c, const char *src, const char *fname)
{
    char *err_message;
    const char *line;

    // Start with the source configuration
    apr_table_t *kvp = read_pKVP_from_file(cmd->temp_pool, src, &err_message);
    if (NULL == kvp) return err_message;

    err_message = const_cast<char*>(ConfigRaster(cmd->pool, kvp, c->inraster));
    if (err_message) return apr_pstrcat(cmd->pool, "Source ", err_message, NULL);

    // Then the real configuration file
    kvp = read_pKVP_from_file(cmd->temp_pool, fname, &err_message);
    if (NULL == kvp) return err_message;
    err_message = const_cast<char *>(ConfigRaster(cmd->pool, kvp, c->raster));
    if (err_message) return err_message;

    // Output mime type
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

    line = apr_table_get(kvp, "SourcePostfix");
    if (line)
        c->postfix = apr_pstrdup(cmd->pool, line);

    c->quality = 75.0; // Default for JPEG
    line = apr_table_get(kvp, "Quality");
    if (line)
        c->quality = strtod(line, NULL);

    // Enabled if we got this far
    c->enabled = true;
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
static int input_level(const TiledRaster &raster, double res, int over) {
    // The raster levels are in increasing resolution order, test until 
    for (int choice = 0; choice < raster.n_levels; choice++) {
        double cres = raster.rsets[choice].rx;
        cres -= cres / raster.pagesize.x / 2; // Add half pixel worth to avoid jitter noise
        if (cres < res) { // This is the best choice, we will return
            if (over) choice -= 1; // Use the lower resolution if oversampling
            if (choice < raster.skip)
                return raster.skip;
            return choice;
        }
    }
    // Use the highest resolution level
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
    // X and Y are in pages, so a pixel is 1/pagesize
    tl_tile->x = int(x + 0.25 / raster.pagesize.x);
    tl_tile->y = int(y + 0.25 / raster.pagesize.y);

    x = (bb.xmax - raster.bbox.xmin) / (rx * raster.pagesize.x);
    y = (raster.bbox.ymax - bb.ymin) / (ry * raster.pagesize.y);

    // Pad these quarter pixel to avoid jitter
    br_tile->x = int(x + 0.25 / raster.pagesize.x);
    br_tile->y = int(y + 0.25 / raster.pagesize.y);
    // Use a tile only if we get more than half pixel in
    if (x - br_tile->x > 0.5 / raster.pagesize.x) br_tile->x++;
    if (y - br_tile->y > 0.5 / raster.pagesize.y) br_tile->y++;
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

    codec_params params;
    int pixel_size = DT_SIZE(cfg->inraster.datatype);

    // inraster->pagesize.c has to be set correctly
    int input_line_width = int(cfg->inraster.pagesize.x * cfg->inraster.pagesize.c * pixel_size);
    int pagesize = int(input_line_width * cfg->inraster.pagesize.y);

    params.line_stride = int((br.x - tl.x) * input_line_width);

    apr_size_t bufsize = pagesize * ntiles;
    if (*buffer == NULL) // Allocate the buffer if not provided, filled with zeros
        *buffer = apr_pcalloc(r->pool, bufsize);


    // Retrieve every required tile and decompress it in the right place
    for (int y = int(tl.y); y < br.y; y++) for (int x = int(tl.x); x < br.x; x++) {
        char *sub_uri = apr_pstrcat(r->pool,
            (tl.z == 0) ?
            apr_psprintf(r->pool, "%s/%d/%d/%d", cfg->source, int(tl.l), y, x) :
            apr_psprintf(r->pool, "%s/%d/%d/%d/%d", cfg->source, int(tl.z), int(tl.l), y, x),
            cfg->postfix, NULL);

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

	storage_manager src = { rctx.buffer, rctx.size };
        apr_uint32_t sig;
        memcpy(&sig, rctx.buffer, sizeof(sig));

        switch (hton32(sig))
        {
        case JPEG_SIG:
            error_message = jpeg_stride_decode(params, cfg->inraster, src, b);
            break;
        case PNG_SIG:
            error_message = png_stride_decode(params, cfg->inraster, src, b);
            break;
        default:
            error_message = "Unsupported format received";
        }

        if (error_message != NULL) { // Something went wrong
            ap_log_rerror(APLOG_MARK, APLOG_ERR, 0, r, "%s :%s", error_message, sub_uri);
            return HTTP_NOT_FOUND;
        }
    }

    ap_remove_output_filter(rf);
    apr_table_clear(r->headers_out); // Clean up the headers set by subrequests

    return APR_SUCCESS;
}

// Interpolation line, contains the above line and the relative weight (never zero)
// These don't have to be bit fields, but it keeps them smaller
// w is weigth of next line *256, can be 0 but not 256.
// line is the higher line to be interpolated, always positive
struct iline {
    unsigned int w:8, line:24;
};

// Offset should be Out - In, center of first pixels, real world coordinates
// If this is negative, we got trouble?
static void init_ilines(double delta_in, double delta_out, double offset, iline *itable, int lines)
{
    for (int i = 0; i < lines; i++) {
        double pos = (offset + i * delta_out) / delta_in;
        // The high line
        itable[i].line = static_cast<int>(ceil(pos));
        // Weight of high line, under 256
        itable[i].w = static_cast<int>(floor(256.0 * (pos - floor(pos))));
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

// An 2D buffer
struct interpolation_buffer {
    void *buffer;       // Location of first value per line
    sz size;            // Describes the organization of the buffer
};


//
// Perform the actual interpolation, using the working type WT
// IMPROVE: interpolate could reuse lines of H values.
// IMPROVE: template over the colors, to optimize inner loop
//

template<typename T = apr_byte_t, typename WT = apr_int32_t> static void interpolate(
    const interpolation_buffer &src, interpolation_buffer &dst,
    iline *h = NULL, iline *v = NULL)
{
    ap_assert(src.size.c == dst.size.c); // Same number of colors
    T *data = reinterpret_cast<T *>(dst.buffer);
    T *s = reinterpret_cast<T *>(src.buffer);
    const int colors = static_cast<int>(dst.size.c);
    const int slw = static_cast<int>(src.size.x * colors);    // Source line size in pixels
    // const int dlw = dst.size.x * colors; // Destination line size in pixels
    for (int y = 0; y < dst.size.y; y++) {
        unsigned int vw = v[y].w;
        for (int x = 0; x < dst.size.x; x++)
        {
            unsigned int hw = h[x].w;
            int idx = slw * v[y].line + h[x].line * colors; // Top left index
            for (int c = 0; c < colors; c++) {
                WT hi = static_cast<WT>(s[idx + c]) * hw +
                    static_cast<WT>(s[idx + c - colors]) * (256 - hw);
                WT lo = static_cast<WT>(s[idx + c - slw]) * hw +
                    static_cast<WT>(s[idx + c - slw - colors]) * (256 - hw);
                // Then interpolate the high and low using vertical weight
                WT value = hi * vw + lo * (256 - vw);
                // The value is multipled by 256^2 because each interpolation is times 256
                // Make sure the working type is large enough to eliminate overflow
                *data++ = static_cast<T>(value / (256 * 256));
            }
        }
    }
}

//
// NearNb sampling, based no ilines
// Uses the weights to pick between two choices
//

template<typename T = apr_byte_t> static void interpolateNN(
    const interpolation_buffer &src, interpolation_buffer &dst,
    iline *h, iline *v)
{
    ap_assert(src.size.c == dst.size.c);
    T *data = reinterpret_cast<T *>(dst.buffer);
    T *s = reinterpret_cast<T *>(src.buffer);
    const int colors = static_cast<int>(dst.size.c);

    // Precompute the horizontal index table, the vertical is only done once
    std::vector<int> hidx(dst.size.x);
    for (int i = 0; i < hidx.size(); i++)
        hidx[i] = colors * (h[i].line - ((h[i].w < 128) ? 1 : 0));

    if (colors == 1) // faster code due to fewer tests
        for (int y = 0; y < static_cast<int>(dst.size.y); y++) {
            int vidx = src.size.x * (v[y].line - ((v[y].w < 128) ? 1 : 0));
            for (auto const &hid : hidx)
                *data++ = s[vidx + hid];
        }
    else
        for (int y = 0; y < static_cast<int>(dst.size.y); y++) {
            int vidx = colors * src.size.x * (v[y].line - ((v[y].w < 128) ? 1 : 0));
            for (auto const &hid : hidx)
                for (int c = 0; c < colors; c++)
                    *data++ = s[vidx + hid + c];
        }
}

// Calls the right interpolation on the right data type
void resample(const repro_conf *cfg, const interpolation_buffer &src, interpolation_buffer &dst,
    iline *h, iline *v)
{
    switch (cfg->raster.datatype) {
    case GDT_UInt16:
        if (cfg->nearNb)
            interpolateNN<apr_uint16_t>(src, dst, h, v);
        else
            interpolate<apr_uint16_t>(src, dst, h, v);
        break;
    case GDT_Int16:
        if (cfg->nearNb)
            interpolateNN<apr_int16_t>(src, dst, h, v);
        else
            interpolate<apr_int16_t>(src, dst, h, v);
        break;
    default: // Byte
        if (cfg->nearNb)
            interpolateNN(src, dst, h, v);
        else
            interpolate(src, dst, h, v);
    }
}

// Interpolate input to output
// TODO: Consider the ry
//
static int affine_interpolate(request_rec *r, const sz *out_tile, sz *tl, sz *br,
    void *buffer, storage_manager &dst)
{
    repro_conf *cfg = (repro_conf *)ap_get_module_config(r->per_dir_config, &reproject_module);

    const double out_rx = cfg->raster.rsets[out_tile->l].rx;
    const double out_ry = cfg->raster.rsets[out_tile->l].ry;
    const int input_l = input_level(cfg->inraster, out_rx, cfg->oversample);
    const double in_rx = cfg->inraster.rsets[input_l].rx;
    const double in_ry = cfg->inraster.rsets[input_l].ry;
    double offset;

    // Input and output interpolation buffers
    interpolation_buffer ib = {buffer, cfg->inraster.pagesize};
        // Input size is a multiple of input pagesize
    ib.size.x *= (br->x - tl->x);
    ib.size.y *= (br->y - tl->y);
    interpolation_buffer ob = {dst.buffer, cfg->raster.pagesize};

    // Compute the bounding boxes
    bbox_t out_bbox, in_bbox;
    tile_to_bbox(cfg->raster, out_tile, out_bbox);
    tl->l = input_l;
    tile_to_bbox(cfg->inraster, tl, in_bbox);

    // Use a single transient vector for the interpolation table
    std::vector<iline> table(ob.size.x + ob.size.y);
    iline *h_table = table.data();
    iline *v_table = h_table + ob.size.x;
    offset = out_bbox.xmin - in_bbox.xmin + (out_rx - in_rx) / 2.0;
    init_ilines(in_rx, out_rx, offset, h_table, int(ob.size.x));
    offset = in_bbox.ymax - out_bbox.ymax + (out_ry - in_ry) / 2.0;
    init_ilines(in_ry, out_ry, offset, v_table, int(ob.size.y));

    // Adjust the interpolation tables to avoid addressing pixels outside of the bounding box
    adjust_itable(h_table, int(ob.size.x), static_cast<unsigned int>(ib.size.x - 1));
    adjust_itable(v_table, int(ob.size.y), static_cast<unsigned int>(ib.size.y - 1));
    resample(cfg, ib, ob, h_table, v_table);

    return APR_SUCCESS;
}

// Web mercator X to longitude in degrees
static double wm2lon(double eres, double x) {
    return 360 * eres * x;
}

// Web mercator Y to latitude in degrees
static double wm2lat(double eres, double y) {
    return 90 * (1 - 4 / pi * atan(exp(eres * pi * 2 * -y)));
}

// Convert WM bbox to GCS bbox in degrees
static void bbox_wm2gcs(double eres, const bbox_t &wm_bbox, bbox_t &gcs_bbox) {
    gcs_bbox.xmin = wm2lon(eres, wm_bbox.xmin);
    gcs_bbox.ymin = wm2lat(eres, wm_bbox.ymin);
    gcs_bbox.xmax = wm2lon(eres, wm_bbox.xmax);
    gcs_bbox.ymax = wm2lat(eres, wm_bbox.ymax);
}

static double lon2wm(double eres, double lon) {
    return lon / eres / 360;
}

// Goes out of bounds close to the poles, valid latitude range is under 85.052
static double lat2wm(double eres, double lat) {
    if (abs(lat) < 85.052)
        return log(tan(pi / 4 * (1 + lat / 90))) / eres / 2 / pi;
    return (lat > 85) ? (0.5 / eres) : (-0.5 / eres);
}

// Convert GCS bbox to WM
static void bbox_gcs2wm(double eres, const bbox_t &gcs_bbox, bbox_t &wm_bbox) {
    wm_bbox.xmin = lon2wm(eres, gcs_bbox.xmin);
    wm_bbox.ymin = lat2wm(eres, gcs_bbox.ymin);
    wm_bbox.xmax = lon2wm(eres, gcs_bbox.xmax);
    wm_bbox.ymax = lat2wm(eres, gcs_bbox.ymax);
}

// Interpolate GCS input to WM output
//
static int gcs2wm_interpolate(request_rec *r, const sz *out_tile, sz *tl, sz *br,
    void *buffer, storage_manager &dst)
{
    repro_conf *cfg = (repro_conf *)ap_get_module_config(r->per_dir_config, &reproject_module);
    // Compute the bounding boxes
    bbox_t out_bbox, out_gcs_bbox, in_bbox;
    tile_to_bbox(cfg->raster, out_tile, out_bbox); // In web mercator meters
    bbox_wm2gcs(cfg->eres, out_bbox, out_gcs_bbox); // And in GCS degrees

    // Horizontal output resolution in degrees
    const double out_rx_gcs = (out_gcs_bbox.xmax - out_gcs_bbox.xmin) / cfg->raster.pagesize.x;
    const int input_l = input_level(cfg->inraster, out_rx_gcs, cfg->oversample);
    const double in_rx = cfg->inraster.rsets[input_l].rx;
    const double in_ry = cfg->inraster.rsets[input_l].ry;

    // Input and output interpolation buffers
    interpolation_buffer ib = {buffer, cfg->inraster.pagesize};
    // Input size is a multiple of input pagesize
    ib.size.x *= (br->x - tl->x);
    ib.size.y *= (br->y - tl->y);
    interpolation_buffer ob = {dst.buffer, cfg->raster.pagesize};

    tl->l = input_l;
    tile_to_bbox(cfg->inraster, tl, in_bbox); // In GCS degrees

    // Use a single transient vector for the interpolation table
    std::vector<iline> table(ob.size.x + ob.size.y);
    iline *h_table = table.data();
    iline *v_table = h_table + ob.size.x;

    // Horizontal is just scaling
    double offset = out_gcs_bbox.xmin - in_bbox.xmin + (out_rx_gcs - in_rx) / 2.0;
    init_ilines(in_rx, out_rx_gcs, offset, h_table, int(ob.size.x));
    // Adjust the interpolation tables to avoid addressing pixels outside of the bounding box
    adjust_itable(h_table, int(ob.size.x), static_cast<unsigned int>(ib.size.x - 1));

    // Non linear scaling on the vertical axis
    offset = in_bbox.ymax - in_ry / 2;
    const double out_ry = cfg->raster.rsets[out_tile->l].ry;
    for (int i = 0; i < int(ob.size.y); i++) {
        // Latitude of center pixel for this output line
        double lat = wm2lat(cfg->eres, out_bbox.ymax - out_ry * (i + 0.5));
        // Input line position
        double pos = (offset - lat) / in_ry;
        // Pick the higher line
        v_table[i].line = static_cast<int>(ceil(pos));
        // Weight of high line, under 256
        v_table[i].w = static_cast<int>(floor(256.0 * (pos - floor(pos))));
    }
    adjust_itable(v_table, int(ob.size.y), static_cast<unsigned int>(ib.size.y - 1));

    resample(cfg, ib, ob, h_table, v_table);
    return APR_SUCCESS;
}

// Interpolate WM input to GCS output
//
static int wm2gcs_interpolate(request_rec *r, const sz *out_tile, sz *tl, sz *br,
    void *buffer, storage_manager &dst)
{
    repro_conf *cfg = (repro_conf *)ap_get_module_config(r->per_dir_config, &reproject_module);

    // Compute the bounding boxes, the output both in native and input CS
    bbox_t out_bbox, out_bbox_equiv, in_bbox;
    tile_to_bbox(cfg->raster, out_tile, out_bbox); // In GCS degrees
    bbox_gcs2wm(cfg->eres, out_bbox, out_bbox_equiv); // And in WM meters

    // Horizontal output resolution in degrees
    const double out_rx_equiv = (out_bbox_equiv.xmax - out_bbox_equiv.xmin) / cfg->raster.pagesize.x;
    const int input_l = input_level(cfg->inraster, out_rx_equiv, cfg->oversample);
    const double in_rx = cfg->inraster.rsets[input_l].rx;
    const double in_ry = cfg->inraster.rsets[input_l].ry;

    // Input and output interpolation buffers
    interpolation_buffer ib = { buffer, cfg->inraster.pagesize };
    // Input size is a multiple of input pagesize
    ib.size.x *= (br->x - tl->x);
    ib.size.y *= (br->y - tl->y);
    interpolation_buffer ob = { dst.buffer, cfg->raster.pagesize };

    tl->l = input_l;
    tile_to_bbox(cfg->inraster, tl, in_bbox); // In GCS degrees

    // Use a single transient vector for the interpolation table
    std::vector<iline> table(ob.size.x + ob.size.y);
    iline *h_table = table.data();
    iline *v_table = h_table + ob.size.x;

    // Horizontal is just scaling
    double offset = out_bbox_equiv.xmin - in_bbox.xmin + (out_rx_equiv - in_rx) / 2.0;
    init_ilines(in_rx, out_rx_equiv, offset, h_table, int(ob.size.x));
    // Adjust the interpolation tables to avoid addressing pixels outside of the bounding box
    adjust_itable(h_table, int(ob.size.x), static_cast<unsigned int>(ib.size.x - 1));

    // Non linear scaling on the vertical axis
    offset = in_bbox.ymax - in_ry / 2;
    const double out_ry = cfg->raster.rsets[out_tile->l].ry;
    for (int i = 0; i < int(ob.size.y); i++) {
        // Latitude of center pixel for this output line
        double lat = lat2wm(cfg->eres, out_bbox.ymax - out_ry * (i + 0.5));
        // Input line position
        double pos = (offset - lat) / in_ry;
        // Pick the higher line
        v_table[i].line = static_cast<int>(ceil(pos));
        // Weight of high line, under 256
        v_table[i].w = static_cast<int>(floor(256.0 * (pos - floor(pos))));
    }
    adjust_itable(v_table, int(ob.size.y), static_cast<unsigned int>(ib.size.y - 1));

    resample(cfg, ib, ob, h_table, v_table);
    return APR_SUCCESS;
}

// Is the projection GCS
static bool is_gcs(const char *projection) {
    return !apr_strnatcasecmp(projection, "GCS") 
        || !apr_strnatcasecmp(projection, "EPSG:4326");
}

// Is the projection spherical mercator, include the Pseudo Mercator code
static bool is_wm(const char *projection) {
    return !apr_strnatcasecmp(projection, "WM") 
        || !apr_strnatcasecmp(projection, "EPSG:3857")
        || !apr_strnatcasecmp(projection, "EPSG:3785");
}

// Is the projection WGS84 based mercator
static bool is_mercator(const char *projection) {
    return !apr_strnatcasecmp(projection, "Mercator") 
        || !apr_strnatcasecmp(projection, "EPSG:3395");
}

// If projection is the same, the transformation is an affine scaling
#define IS_AFFINE_SCALING(cfg) (!apr_strnatcasecmp(cfg->inraster.projection, cfg->raster.projection))
#define IS_GCS2WM(cfg) (is_gcs(cfg->inraster.projection) && is_wm(cfg->raster.projection))
#define IS_WM2GCS(cfg) (is_wm(cfg->inraster.projection) && is_gcs(cfg->raster.projection))

// Projection is not changed, only scaling
// TODO: Consider ry
static int scaling_handler(request_rec *r, sz *tile) {
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

    // A buffer for outgoing raw tile
    storage_manager src;
    // Get a raw tile buffer
    int pixel_size = cfg->raster.pagesize.c * dt_size[cfg->raster.datatype];
    src.size = (int)(cfg->raster.pagesize.x * cfg->raster.pagesize.y * pixel_size);
    src.buffer = (char *)apr_palloc(r->pool, src.size);

    // Improvement: Skip the interpolation if input and output are identical. The savings are small
    // Create the output page
    affine_interpolate(r, tile, &tl_tile, &br_tile, buffer, src);

    // A buffer for outgoing compressed tile
    storage_manager dst;
    dst.size = cfg->max_output_size;
    dst.buffer = (char *)apr_palloc(r->pool, (apr_size_t)dst.size);
    const char *error_message = "Unknown output format requested";

    if (NULL == cfg->mime_type || 0 == apr_strnatcmp(cfg->mime_type, "image/jpeg")) {
        jpeg_params params;
        params.quality = static_cast<int>(cfg->quality);
        error_message = jpeg_encode(params, cfg->raster, src, dst);
    }
    else if (0 == apr_strnatcmp(cfg->mime_type, "image/png")) {
        png_params params;
        set_png_params(cfg->raster, &params);
        if (cfg->quality < 10) // Otherwise use the default of 6
            params.compression_level = static_cast<int>(cfg->quality);
        error_message = png_encode(params, cfg->raster, src, dst);
    }

    if (error_message) {
        ap_log_rerror(APLOG_MARK, APLOG_ERR, 0, r, "%s from :%s", error_message, r->uri);
        // Something went wrong if compression fails
        return HTTP_INTERNAL_SERVER_ERROR;
    }

    return send_image(r, (apr_uint32_t *)dst.buffer, dst.size, cfg->mime_type);
}

static int gcs2wm_handler(request_rec *r, sz *tile) {
    repro_conf *cfg = (repro_conf *)ap_get_module_config(r->per_dir_config, &reproject_module);
    // Compute the output tile tile bounding box, in WM
    bbox_t wm_bbox, gcs_bbox;
    tile_to_bbox(cfg->raster, tile, wm_bbox);

    // Convert bounding box to GCS, always valid
    bbox_wm2gcs(cfg->eres, wm_bbox, gcs_bbox);

    double out_rx_gcs = (gcs_bbox.xmax - gcs_bbox.xmin) / cfg->raster.pagesize.x;

    // Pick the level based on X resolution.
    int input_l = input_level(cfg->inraster, out_rx_gcs, cfg->oversample);

    // Range of input tiles
    sz tl_tile, br_tile;
    bbox_to_tile(cfg->inraster, input_l, gcs_bbox, &tl_tile, &br_tile);

    // Complete the tile setup, pass the z and adjust the level
    tl_tile.z = br_tile.z = tile->z;
    // Copy the c from input
    tl_tile.c = br_tile.c = cfg->inraster.pagesize.c;
    tl_tile.l = br_tile.l = input_l - cfg->inraster.skip;

    void *buffer = NULL;
    apr_status_t status = retrieve_source(r, tl_tile, br_tile, &buffer);

    if (status != APR_SUCCESS)
        return status;

    // A buffer for outgoing raw tile
    storage_manager src;
    // Get a raw tile buffer
    int pixel_size = cfg->raster.pagesize.c * DT_SIZE(cfg->raster.datatype);
    src.size = (int)(cfg->raster.pagesize.x * cfg->raster.pagesize.y * pixel_size);
    src.buffer = (char *)apr_palloc(r->pool, src.size);

    // Create the output page
    gcs2wm_interpolate(r, tile, &tl_tile, &br_tile, buffer, src);

    // A buffer for outgoing compressed tile
    storage_manager dst;
    dst.size = cfg->max_output_size;
    dst.buffer = (char *)apr_palloc(r->pool, (apr_size_t)dst.size);
    const char *error_message = "Unknown output format requested";

    if (NULL == cfg->mime_type || 0 == apr_strnatcmp(cfg->mime_type, "image/jpeg")) {
        jpeg_params params;
        params.quality = static_cast<int>(cfg->quality);
        error_message = jpeg_encode(params, cfg->raster, src, dst);
    }
    else if (0 == apr_strnatcmp(cfg->mime_type, "image/png")) {
        png_params params;
        set_png_params(cfg->raster, &params);
        if (cfg->quality < 10) // Otherwise use the default of 6
            params.compression_level = static_cast<int>(cfg->quality);
        error_message = png_encode(params, cfg->raster, src, dst);
    }

    if (error_message) {
        ap_log_rerror(APLOG_MARK, APLOG_ERR, 0, r, "%s from :%s", error_message, r->uri);
        // Something went wrong if compression fails
        return HTTP_INTERNAL_SERVER_ERROR;
    }

    return send_image(r, (apr_uint32_t *)dst.buffer, dst.size, cfg->mime_type);
}


// TODO: DEBUG
static int wm2gcs_handler(request_rec *r, sz *tile) {
    repro_conf *cfg = (repro_conf *)ap_get_module_config(r->per_dir_config, &reproject_module);
    double out_rx = cfg->raster.rsets[tile->l].rx;

    bbox_t wm_bbox, gcs_bbox;
    tile_to_bbox(cfg->raster, tile, gcs_bbox);
    bbox_gcs2wm(cfg->eres, gcs_bbox, wm_bbox);

    double out_rx_wm = (wm_bbox.xmax - wm_bbox.xmin) / cfg->raster.pagesize.x;
    // Pick the input level based on equivalend output x resolution
    int input_l = input_level(cfg->inraster, out_rx_wm, cfg->oversample);

    // Compute range of input tiles
    sz tl_tile, br_tile;
    bbox_to_tile(cfg->inraster, input_l, wm_bbox, &tl_tile, &br_tile);

    // Complete the tile setup, pass the z and adjust the level
    tl_tile.z = br_tile.z = tile->z;
    // Copy the c from input
    tl_tile.c = br_tile.c = cfg->inraster.pagesize.c;
    tl_tile.l = br_tile.l = input_l - cfg->inraster.skip;

    void *buffer = NULL;
    apr_status_t status = retrieve_source(r, tl_tile, br_tile, &buffer);

    if (status != APR_SUCCESS)
        return status;

    // A buffer for outgoing raw tile
    storage_manager src;
    // Get a raw tile buffer
    int pixel_size = cfg->raster.pagesize.c * DT_SIZE(cfg->raster.datatype);
    src.size = (int)(cfg->raster.pagesize.x * cfg->raster.pagesize.y * pixel_size);
    src.buffer = (char *)apr_palloc(r->pool, src.size);

    // Create the output page
    wm2gcs_interpolate(r, tile, &tl_tile, &br_tile, buffer, src);

    // A buffer for outgoing compressed tile
    storage_manager dst;
    dst.size = cfg->max_output_size;
    dst.buffer = (char *)apr_palloc(r->pool, (apr_size_t)dst.size);
    const char *error_message = "Unknown output format requested";

    if (NULL == cfg->mime_type || 0 == apr_strnatcmp(cfg->mime_type, "image/jpeg")) {
        jpeg_params params;
        params.quality = static_cast<int>(cfg->quality);
        error_message = jpeg_encode(params, cfg->raster, src, dst);
    }
    else if (0 == apr_strnatcmp(cfg->mime_type, "image/png")) {
        png_params params;
        set_png_params(cfg->raster, &params);
        if (cfg->quality < 10) // Otherwise use the default of 6
            params.compression_level = static_cast<int>(cfg->quality);
        error_message = png_encode(params, cfg->raster, src, dst);
    }

    if (error_message) {
        ap_log_rerror(APLOG_MARK, APLOG_ERR, 0, r, "%s from :%s", error_message, r->uri);
        // Something went wrong if compression fails
        return HTTP_INTERNAL_SERVER_ERROR;
    }

    return send_image(r, (apr_uint32_t *)dst.buffer, dst.size, cfg->mime_type);
}

static bool our_request(request_rec *r) {
    if (r->method_number != M_GET) return false;
//    if (r->args) return false; // Don't accept arguments

    repro_conf *cfg = (repro_conf *)ap_get_module_config(r->per_dir_config, &reproject_module);
    if (!cfg->enabled) return false;

    if (cfg->regexp) { // Check the guard regexps if they exist, matches agains URL
        char *url_to_match = r->args ? apr_pstrcat(r->pool, r->uri, "?", r->args, NULL) : r->uri;
        for (int i = 0; i < cfg->regexp->nelts; i++) {
            ap_regex_t *m = &APR_ARRAY_IDX(cfg->regexp, i, ap_regex_t);
            if (!ap_regexec(m, url_to_match, 0, NULL, 0)) return true; // Found
        }
    }
    return false;
}

static int handler(request_rec *r)
{
    // TODO: use r->header_only to verify ETags, assuming the subrequests are faster in that mode
    if (!our_request(r)) return DECLINED;

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

    repro_conf *cfg = (repro_conf *)ap_get_module_config(r->per_dir_config, &reproject_module);
    // We can ignore the error on this one, defaults to zero
    // The parameter before the level can't start with a digit for an extra-dimensional MRF
    if (cfg->raster.size.z != 1 && tokens->nelts)
        tile.z = apr_atoi64(*(char **)apr_array_pop(tokens));

    // Don't allow access to negative values, send the empty tile instead
    if (tile.l < 0 || tile.x < 0 || tile.y < 0)
        return send_empty_tile(r);

    // Adjust the level to what the input is
    tile.l += cfg->raster.skip;

    // Outside of bounds tile returns a not-found error
    if (tile.l >= cfg->raster.n_levels ||
        tile.x >= cfg->raster.rsets[tile.l].width ||
        tile.y >= cfg->raster.rsets[tile.l].height)
        return HTTP_NOT_FOUND;

    // Need to have mod_receive available
    SERR_IF(!ap_get_output_filter_handle("Receive"), "mod_receive not installed");

    // TODO: Replace with a defined callback set post-configuration
    if (IS_AFFINE_SCALING(cfg))
        return scaling_handler(r, &tile);
    else if (IS_GCS2WM(cfg))
        return gcs2wm_handler(r, &tile);
    else if (IS_WM2GCS(cfg))
        return wm2gcs_handler(r, &tile);

    SERR_IF(true, "incorrect reprojection setup");
}

#undef REQ_ERR_IF

static const command_rec cmds[] =
{
    AP_INIT_TAKE2(
    "Reproject_ConfigurationFiles",
    (cmd_func) read_config, // Callback
    0, // Self-pass argument
    ACCESS_CONF, // availability
    "Source and output configuration files"
    ),

    AP_INIT_TAKE1(
    "Reproject_RegExp",
    (cmd_func) set_regexp,
    0, // Self-pass argument
    ACCESS_CONF, // availability
    "Regular expression that the URL has to match.  At least one is required."),

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
