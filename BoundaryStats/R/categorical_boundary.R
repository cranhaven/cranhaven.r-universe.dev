#' @name categorical_boundary
#' @title Define the boundary elements of a SpatRaster with categorical data
#' @description Creates boundary element cells where patches of two categories meet.
#'
#' @param x A SpatRaster object.
#' @return A SpatRaster object with cell values 1 for boundary elements and 0 for other cells
#' 
#' @examples \donttest{
#' data(grassland)
#' grassland <- terra::rast(grassland_matrix, crs = grassland_crs)
#' terra::ext(grassland) <- grassland_ext
#'
#' grassland_boundaries <- categorical_boundary(grassland)
#' }
#'
#' @author Amy Luo
#' @export
categorical_boundary <- function(x) {
  x_poly <- terra::as.polygons(x)
  mask <- terra::aggregate(x_poly) %>%
    terra::as.lines(.) %>%
    terra::rasterize(., terra::rast(x))
  fill <- terra::aggregate(x_poly) %>%
    terra::rasterize(., terra::rast(x), field = 0)
  
  x_boundary <- terra::as.lines(x_poly) %>%
    terra::rasterize(., terra::rast(x)) %>%
    terra::merge(., fill) %>%
    terra::mask(., mask, maskvalues = 1, updatevalue = 0)
  
  terra::crs(x_boundary) <- terra::crs(x)
  return(x_boundary)
}