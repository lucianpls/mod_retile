# mod_reproject
An apache module that converts a geospatial tile service from one projection and tiling grid to another

Implements one apache configuration directive:
**Retile_ConfigurationFiles source_configuration_file configuration_file**
The first file contains the source raster information, while the second the desired configuration for the output 

Known Directives, output only except stated otherwise

**Size X Y Z C**
  - Mandatory, at least x and y, the raster size in pixels, in both files

**PageSize X Y 1 C**
  - Optional, the pagesize in pixels, in both files

**Projection String**
  - Optional, in both files

**SkippedLevels N**
  - Optional, defaults to 0, counted from the top of the pyramid, in both files
