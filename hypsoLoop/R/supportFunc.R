#' Function to calculate areas of classes within a categorical raster
#'
#' This function takes a categorical raster object and calculates the areas covered by each class within the raster.
#' @param x An object of class RasterLayer.
#' @return A data frame with one column representing the values of the classes of the raster and the other variable representing corresponding areas in Ha.
#' @author Faustin Gashakamba
#' @details
#' The raster package's "area()" function normally returns the same raster where cell values have been replaced by their areas in Square Km.
#' Using this function, the values of all cells of same class are aggregated and the raster is transformed into a data frame.
#' If the CRS of the input raster is projected, then the area is calculated by multiplying the resolution of the raster by the count of cells for each class.
#' @seealso \code{\link{area}}
#' @seealso \code{\link{aggregate}}
#' @seealso \code{\link{values}}
#' @importFrom terra cellSize
#' @importFrom terra aggregate
#' @importFrom terra values
#' @importFrom terra rast
#' @importFrom terra as.data.frame
#' @examples
#' calc_areas(lulcYanze)
#' @export
#'
calc_areas <- function(x){
  if(class(x)[1] != "SpatRaster"){
    x <- terra::rast(x)
  }
  b <- terra::values(terra::cellSize(x, unit="ha")) #assign each cell by its area
  b <- terra::aggregate(b, by=list(terra::values(x)), sum, na.rm=TRUE)
  b <- terra::as.data.frame(b)
  names(b) <- c("CLASS","AREA")
  return(b)
}

#' Test if user-supplied inputs match the expected arguments type and form
#'
#' This function tests whether the input supplied by the user are of the expected type (class),
#' and are in the right form (overlap of extents and same projection).
#'
#' @param x The input provided by the user for the x argument.
#' @param y The input provided by the user for the y argument.
#' @return There is no return value. If any error is found, the execution is just halted.
#' @importFrom terra extract
#' @importFrom terra vect
#' @importFrom sf st_crs
#' @importFrom sf st_transform
#'
check_arguments <- function(x, y){
  if(!any(class(x)[1] %in% c("SpatialPolygonsDataFrame", "sf", "SpatVector"))){
    stop("X has to be a spatial object")
  }
  if(!any(class(y)[1] %in% c("SpatRaster", "RasterLayer", "stars"))){
    stop("y has to be a raster object")
  }
  if(is.null(terra::extract(y, terra::vect(x))) == TRUE){
    stop("the two inputs have to spatially overlap")
  }

  x <- sf::st_transform(x, crs = sf::st_crs(y)) #ensure x and Y use the same crs
}

#' Convert the input vector object into simple features if it's provided as a Spatial object
#'
#' This function tests whether the supplied vector input is a simple features (sf) object,
#' if not, the object is converted using the st_as-sf() function.
#'
#' @param x The input provided by the user for the x argument.
#' @return the converted simple features object
#' @importFrom sf st_as_sf
#'
Vector_conversion <- function(x){
  if(class(x)[1] != "sf"){
    x <- st_as_sf(x)
  }
  return(x)
}

#' Convert the input raster object into terra's SpatRaster object
#'
#' This function tests whether the supplied vector input is a simple features (sf) object,
#' if not, the object is converted using the st_as-sf() function.
#'
#' @param y The input provided by the user for the y argument.
#' @return the converted SpatRaster object
#' @importFrom terra rast
#'
raster_conversion <- function(y){
  if(class(y)[1] != "SpatRaster"){
    y <- terra::rast(y)
  }
  return(y)
}

