#' @name random_raster_sim
#' @title Stochastic neutral landscape model
#' @description Simulates a spatially stochastic neutral landscape of the same extent and resolution as the input
#' raster, with the same distribution of values.
#'
#' @param x A SpatRaster object.
#' @return A SpatRaster object with boundary elements.
#' 
#' @examples
#' data(grassland)
#' grassland <- terra::rast(grassland_matrix, crs = grassland_crs)
#' terra::ext(grassland) <- grassland_ext
#' 
#' simulation <- random_raster_sim(grassland)
#' terra::plot(simulation)
#' 
#' @author Amy Luo
#' @references
#' James, P. M. A., Fleming, R.A., & Fortin, M.-J. (2010) Identifying significant scale-specific spatial boundaries using wavelets and null models: Spruce budworm defoliation in Ontario, Canada as a case study. Landscape Ecology, 6, 873-887.
#' @export
random_raster_sim <- function (x) {
  values <- terra::values(x) %>%
    na.omit(.) %>%
    sample(.)
  cells_to_fill <- terra::cells(x) %>%
    terra::rowColFromCell(x, .) %>%
    t(.) %>%
    as.data.frame(.)
  
  x_sim <- terra::rast(nrow = terra::nrow(x), ncol = terra::ncol(x), crs = terra::crs(x), extent = terra::ext(x))
  index = 1
  for (i in cells_to_fill) {
    x_sim[i[1], i[2]] <- values[index]
    index = index + 1
  }

  return(x_sim)
  
}
