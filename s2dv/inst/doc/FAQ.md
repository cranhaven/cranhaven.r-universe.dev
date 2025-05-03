# FAQs

This document intends to be the first reference for any doubts that you may have regarding s2dv. If you do not find the information you need, please open an issue for your problem.

## Index
1. **How to**
   1. [Global Map with non-standard longitudinal boundaries](#1-global-map-with-non-standard-longitudinal-boundaries)
   2. [Plot a region boundary from shape file with PlotEquiMap](#2-plot-a-region-boundary-from-shape-file-with-plotequimap)

2. **Something goes wrong...**  
   1. [CDORemap() returns errors or warnings with specific module versions](#1-cdoremap-returns-errors-or-warnings-with-specific-module-versions)


## 1. How to

### 1. Global Map with non-standard longitudinal boundaries

Usually, the global maps are displayed:
  a) from -180 to 180 degrees Est being the Grenwich meridian in the center of the map or
  b) from 0 to 360 degrees Est being the Grenwich meridian in the left marging of the map.

You can run the following code to test both cases using PlotEquiMap:

```
library(s2dv)
a <- 1:(180*360)
dim(a) <- c(lat = 180, lon = 360)
PlotEquiMap(a, lon = -179.5 : 179.5, lat = -89.5 : 89.5) # case a)
PlotEquiMap(a, lon = 1 : 360, lat = -89.5 : 89.5) # case b)
```

What if I want to use different boundaries of the region? For instance, if I want to display Atlantic, Indic and Pacific Oceans being centered. Then, you should do some extra steps:

```
library(ClimProjDiags)
a <- Subset(a, along = 'lon', indices = c(20 : 360, 1 : 19))
layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths = c(3, 1))
PlotEquiMap(a, lon = 21 : 380, lat = -89.5 : 89.5, drawleg = FALSE,
            coast_width = 0.0, filled.continents = FALSE)
map("world", wrap = c(20, 390), add = TRUE)
ColorBar(var_limits = c(1, max(a)))
```

Note: You can adjust many parameters to visualize the plot, here we are just showing how to move the boundaries.

If you want to add other information to the plot (e.g.: hatching, points, countours, ...), you can add it just before ColorBar() function.


### 2. Plot a region boundary from shape file with PlotEquiMap()

PlotEquiMap() provides the option to plot the country boundaries by argument
`country.borders = TRUE`. If you want to plot specific area, you can save 
the shapefile data as an RDS file or read the shapefile as a list, and use 
argument `shapefile` to plot it. 

If you have more than one shapefile to plot, you have to plot it on top of the 
figure after PlotEquiMap(). 
The following script shows an example of plotting the Douro region on top of
default country boundaries. The data in this example is synthetic.
To use "rgdal" package, `module load GDAL` first (see wiki [R tips 3. How to load dependencies of R package rgdal](https://earth.bsc.es/wiki/doku.php?id=tools:Rtools&s%5B%5D=Rtools#r_tips) for details.)

```r
library(rgdal)
library(sp)
library(maps)
library(s2dv)

# Read the Douro shapefile and transform:
shp1 <- readOGR(dsn = '/esarchive/scratch/cchou/MEDGOLD/others/Shapefiles/', layer = 'RDD_ETRS89')
s1 <- spTransform(shp1, CRS("+proj=longlat"))
douro <- SpatialPolygons2map(s1)
str(douro)
#List of 4
# $ x    : num [1:37826] -7.77 -7.77 -7.77 -7.77 -7.77 ...
# $ y    : num [1:37826] 41.1 41.1 41.1 41.1 41.1 ...
# $ names: chr [1:10] "0:1" "0:2" "0:3" "0:4" ...
# $ range: num [1:4] -7.92 -6.75 40.92 41.53
# - attr(*, "class")= chr "map"

# Synthetic data
dat <- 1 : (21 * 10)
dim(dat) <- c(lon = 21, lat = 10)

# Plot Douro region using argument 'shapefile'
PlotEquiMap(dat, lon = c(1:10, 350:360), lat = 36 : 45, drawleg = FALSE, 
            filled.continents = FALSE, country.border = TRUE, 
            shapefile = douro, shapefile_col = 'blue')

# Enhance the border line and change the color
map(douro, interior = FALSE, add = TRUE, lwd = 2, col = "red")
```

## 2. Something goes wrong...  

### 1. CDORemap() returns errors or warnings with specific module versions  
CDORemap() uses cdo and ncdf4 inside, and the performance is impacted by those tools a lot. 
Some instances may work with a specific set of module combination but not with another. 
Since the incompatibility is not from the R code, it is hard to improve or prevent the failure. 
Here are some detected cases that specific versions need to be used.  
(1) The 'grid' parameter is a file  
- The workable version combination:  
CDO/1.9.8-foss-2015a  
R/3.6.1-foss-2015a-bare  
HDF5/1.8.14-foss-2015a  
- The unworkable version combination:
_It returns a warning about HDF5._  
CDO/1.6.3-foss-2015a  
R/3.6.1-foss-2015a-bare  
HDF5/1.10.5-foss-2015a  


