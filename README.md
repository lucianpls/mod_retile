# mod_reproject

# WORK IN PROGRESS
# LIMITED FUNCTIONALITY

An apache module that converts a geospatial tile service from one projection and tiling grid to another

# Building

Requires apache httpd, libapr-1, libjpeg, libpng and zlib to be available for linking and at runtime.
In linux this means the runtime and the development packages have to be installed.
In windows, the header files are expected to be in the zlib, png and jpeg subfolders in the same directory as the project files, and the httpd and apr in /Apache24/include.
The libraries for all the above packages should be available in \Apache24\lib and \Apache24\bin.

# Usage

Implements two apache configuration directives:
**Reproject_RegExp string**
Can be present more than once, one of the existing regular expressions has to match the request URL for the request to be considered

**Reproject_ConfigurationFiles source_configuration_file configuration_file**
The first file contains the source raster information, while the second the desired configuration for the output 

# Directives in both source and reproject configuration files

**Size X Y Z C**
  - Mandatory, at least x and y, the raster size in pixels, in both files

**PageSize X Y 1 C**
  - Optional, the pagesize in pixels, in both files, defaults to 512x512

**Projection String**
  - Optional, in both files

**DataType**
  - Required if not unsigned byte.  Valid values are Byte, Int16, UInt16, Int32, UInt32.  Case insensitive

**SkippedLevels N**
  - Optional, defaults to 0, counted from the top of the pyramid, in both files

**BoundingBox xmin,ymin,xmax,ymax**
  - Optional, WMS style bounding box, defaults to 0 to 1 in both x and y.  Floating point using decimal dot format, comma separated

# Directives only in the reproject configuration file

**SourcePath**
  - Mandatory, the location of the tile source, up to the numerical arguments, as a local web path suitable for a subrequest

**SourcePostfix**
  - Optional, a literal string that gets appended to the source URL tile requests

**EmptyTile**
  - Size Offset FileName

**MimeType**
  - Output mime type, defaults to input format.  image/jpeg or image/png.  If specified, forces the output type

**ETagSeed**
  - A base32 64bit number, to be used as a seed for ETag generation

**InputBufferSize**
  - Default is 1MB, should be larger than the maximum expected input tile size

**OutputBufferSize**
  - Default is 1MB, should be larger than the maximum expected output tile size

**Quality**
  - A floating point figure, format dependent.  Default for JPEG is 75.  Default for PNG is 6

**Oversample**
  - If on and the output resolution falls between two available input resolution levels, the lower resolution input will be chosen instead of the higher one

**MaxInTiles**
  - Limits the number of subrequests made for each output tiles.  Valid values are between 2 and 64, default is 16.  A larger number may improve output image quality, but will make some requests slower, for GCS to WM reprojection.

**Nearest**
  - If on, use nearest neighbor resampling instead of bilinear interpolation

**Radius**
  - The planet radius in meters.  Used in reprojection calculations. Defaults to earth radius

  # Ways to use

If the input and output size and alignment match, it can be used to change quality or the format
