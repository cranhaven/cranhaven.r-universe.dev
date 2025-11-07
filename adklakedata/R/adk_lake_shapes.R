#' @title Return path to Lake Polygons Shapefile
#'
#' @description
#' Returns the path to the shapefile for the study Lake polygons.
#' The source is a locally stored shapefile
#' that can be used for mapping and analysis.
#'
#' @import httr
#' 
#' @examples 
#' library(sf)
#' bl = read_sf(adklakedata::adk_shape())
#' lakes = read_sf(adklakedata::adk_lake_shapes())
#' plot(st_geometry(bl))
#' plot(st_geometry(lakes), add=TRUE, col='blue')
#' 
#' @export
adk_lake_shapes = function(){
  return(system.file("extdata", "lake_polygons.shp", package = "adklakedata"))
}


#' @title Return path to Adirondack Park Shapefile
#'
#' @description
#' Returns the path to the shapefile for the Adirondack Park
#' outline (The "Blue Line"). Returns the path to a locally stored shapefile
#' that can be used for mapping and analysis.
#'
#' @import httr
#' 
#' @examples 
#' library(sf)
#' bl = read_sf(adklakedata::adk_shape())
#' lakes = read_sf(adklakedata::adk_lake_shapes())
#' plot(st_geometry(bl))
#' plot(st_geometry(lakes), add=TRUE, col='blue')
#' 
#' 
#' @export
adk_shape = function(){
  return(system.file("extdata", "BlueLine2014Poly.shp", package = "adklakedata"))
}