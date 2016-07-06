# mod_reproject
An apache module that converts a geospatial tile service from one projection and tiling grid to another

Implements one apache configuration directive:
**Retile_ConfigurationFiles source_configuration_file configuration_file**
The first file contains the source raster information, while the second the desired configuration for the output 

Directives for both configuration files

**Size X Y Z C**
  - Mandatory, at least x and y, the raster size in pixels, in both files

**PageSize X Y 1 C**
  - Optional, the pagesize in pixels, in both files

**Projection String**
  - Optional, in both files

**SkippedLevels N**
  - Optional, defaults to 0, counted from the top of the pyramid, in both files

**Projection WKT**
  - Optional, defaults to WM, as in WebMercator

**BoundingBox xmin,ymin,xmax,ymax**
  - Optional, bounding box, defaults to 0 to 1 in both x and y

Directives only in the reproject configuration file

**EmptyTile**
  - Size Offset FileName

**MimeType**
  - Output mime type, defaults to input format

**RegExp**
  - One or more, guard regular expression, the URL has to match one of these if any is present

**ETagSeed**
  - A base32 64bit number, to be used as a seed for ETag generation
