/*
 * JPEG_Codec.cpp
 * C++ Wrapper around libjpeg, providing encoding and decoding functions
 * uses C++ throw-catch instead of setjmp
 *
 * (C)Lucian Plesea 2016
 */

#include "mod_reproject.h"
#include <jpeglib.h>

static void emitMessage(j_common_ptr cinfo, int msgLevel);
static void errorExit(j_common_ptr cinfo);

struct ErrorMgr : public jpeg_error_mgr {
    inline ErrorMgr() {
        jpeg_std_error(this);
        error_exit = errorExit;
        emit_message = emitMessage;
    }
    // A place to hold a message
    char message[JMSG_LENGTH_MAX];
};

static void emitMessage(j_common_ptr cinfo, int msgLevel)
{
    ErrorMgr* err = (ErrorMgr *)cinfo->err;
    if (msgLevel > 0) return; // No trace msgs

    // There can be many warnings, just store the first one
    if (err->num_warnings++ >1) return;
    err->format_message(cinfo, err->message);
}

// No return to caller
static void errorExit(j_common_ptr cinfo)
{
    ErrorMgr* err = (ErrorMgr*)cinfo->err;
    err->format_message(cinfo, err->message);
    throw err->message;
}

/**
*\Brief Do nothing stub function for JPEG library, called
*/
static void stub_source_dec(j_decompress_ptr cinfo) {}

/**
*\Brief: Do nothing stub function for JPEG library
*/
static void skip_input_data_dec(j_decompress_ptr cinfo, long l) {}

// Destination should be already set up
static void init_or_terminate_destination(j_compress_ptr cinfo) {}

/**
*\Brief: Do nothing stub function for JPEG library, called?
*/
static boolean fill_input_buffer_dec(j_decompress_ptr cinfo) { return TRUE; }

// Called if the buffer provided is too small
static boolean empty_output_buffer(j_compress_ptr cinfo) { return FALSE; }

const char *jpeg_stride_decode(TiledRaster &raster, storage_manager &src, void *buffer, apr_uint32_t line_stride)
{
    jpeg_decompress_struct cinfo;
    char *message = NULL;
    ErrorMgr err;
    struct jpeg_source_mgr s = { (JOCTET *)src.buffer, src.size };

    cinfo.err = &err;
    s.term_source = s.init_source = stub_source_dec;
    s.skip_input_data = skip_input_data_dec;
    s.fill_input_buffer = fill_input_buffer_dec;
    s.resync_to_restart = jpeg_resync_to_restart;
    jpeg_create_decompress(&cinfo);

    try {
        cinfo.src = &s;
        jpeg_read_header(&cinfo, TRUE);
        cinfo.dct_method = JDCT_FLOAT;
        if (!(raster.pagesize.c == 1 || raster.pagesize.c == 3))
            throw "JPEG only handles one or three color components";

        cinfo.out_color_space = (raster.pagesize.c == 3) ? JCS_RGB : JCS_GRAYSCALE;
        jpeg_start_decompress(&cinfo);

        while (cinfo.output_scanline < cinfo.image_height) {
            char *rp[2]; // Two lines at a time
            rp[0] = (char *)buffer + line_stride * cinfo.output_scanline;
            rp[1] = rp[0] + line_stride;
            jpeg_read_scanlines(&cinfo, JSAMPARRAY(rp), 2);
        }
        jpeg_finish_decompress(&cinfo);

    }
    catch (char *error) { // Capture the error message
        message = error;
    }

    jpeg_destroy_decompress(&cinfo);
    return message;
}
