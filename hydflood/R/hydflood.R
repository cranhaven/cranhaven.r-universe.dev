#' @name hydflood
#' @docType package
#' @aliases hydflood-package
#' 
#' @title hydflood: Flood Extents and Durations along the Rivers Elbe and Rhine
#' 
#' @description Raster based flood modelling internally using \pkg{hyd1d}, an
#'   R package to interpolate 1d water level and gauging data. The package
#'   computes flood extent and durations through strategies originally developed
#'   for 'INFORM', an 'ArcGIS'-based hydro-ecological modelling framework. It
#'   does not provide a full, physical hydraulic modelling algorithm, but a
#'   simplified, near real time 'GIS' approach for flood extent and duration
#'   modelling. Computationally demanding annual flood durations have been
#'   computed already and data products were published by Weber (2022)
#'   <doi:10.1594/PANGAEA.948042>.
#' 
#' @import hyd1d
#' @importFrom Rdpack reprompt
#' @importFrom stats na.omit
#' @importFrom grDevices rgb
#' @importFrom raster canProcessInMemory
#' @importFrom raster blockSize
#' @importFrom raster pbCreate
#' @importFrom raster getValues
#' @importFrom raster writeValues
#' @importFrom raster pbStep
#' @importFrom raster setValues
#' @importFrom raster pbClose
#' @importFrom terra crs
#' @importFrom terra classify
#' @importFrom terra coltab
#' @importFrom terra minmax
#' @importFrom terra rast
#' @importFrom terra set.cats
#' @importFrom terra unique
#' @importFrom terra trim
#' @importFrom terra writeStart
#' @importFrom terra writeStop
#' @importFrom terra ext
#' @importFrom terra res
#' @importFrom terra crop
#' @importFrom terra intersect
#' @importFrom terra rasterize
#' @importFrom terra merge
#' @importFrom terra extend
#' @importFrom terra sprc
#' @importFrom terra vect
#' @importFrom terra writeRaster
#' @importFrom terra xmin
#' @importFrom terra xmax
#' @importFrom terra ymin
#' @importFrom terra ymax
#' @importFrom sf st_sf
#' @importFrom sf st_sfc
#' @importFrom sf st_crs
#' @importFrom sf st_as_sfc
#' @importFrom sf st_bbox
#' @importFrom sf st_as_sf
#' @importFrom sf st_coordinates
#' @importFrom sf st_transform
#' @importFrom sf st_join
#' 
NULL
