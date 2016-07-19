/*
* PNG_codec.cpp
* C++ Wrapper around libpng, providing encoding and decoding functions
*
* (C)Lucian Plesea 2016
*/

#include "mod_reproject.h"
#include <png.h>

const char *png_stride_decode(const TiledRaster &raster, storage_manager &src,
    void *buffer, apr_uint32_t line_stride)
{
    return NULL;
}

const char *png_encode(const TiledRaster &raster, storage_manager &src,
    storage_manager &dst, double quality)
{
    return NULL;
}