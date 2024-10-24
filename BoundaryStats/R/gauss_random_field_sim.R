#' @name gauss_random_field_sim
#' @title Gaussian random field neutral model
#' @description Simulates a gaussian random field as a neutral landscape of the same extent and resolution as the
#' input raster, using the same spatial autocorrelation range as the input
#'
#' @param x A SpatRaster object.
#' @param corr_range The range of spatial autocorrelation to simulate. Can be estimated using the lisa_clusters function.
#' @return A SpatRaster object with boundary elements.
#'
#' @examples \donttest{
#' #' data(grassland)
#' grassland <- terra::rast(grassland_matrix, crs = grassland_crs)
#' terra::ext(grassland) <- grassland_ext
#'
#' corr <- lisa_clusters(grassland)
#' simulation <- gauss_random_field_sim(grassland, corr)
#' terra::plot(simulation)
#' }
#'
#' @author Amy Luo
#' @references
#' James, P. M. A., Fleming, R.A., & Fortin, M.-J. (2010) Identifying significant scale-specific spatial boundaries using wavelets and null models: Spruce budworm defoliation in Ontario, Canada as a case study. Landscape Ecology, 6, 873-887.
#' @export
gauss_random_field_sim <- function (x, corr_range) {
  repeat {
    invisible(capture.output(
      x_sim <- try(list(1:terra::nrow(x), 1:terra::ncol(x)) %>%
                      fields::circulantEmbeddingSetup(., cov.args = list(p = 2, aRange = corr_range)) %>%
                      fields::circulantEmbedding(.) %>%
                      terra::rast(.),
                    silent = TRUE)
               ))

    if(is(x_sim) == 'try-error') {corr_range = corr_range * 0.9} else {break}
  }

  # make extent and projection match input data
  terra::crs(x_sim) <- terra::crs(x)
  terra::ext(x_sim) <- terra::ext(x)
  x_sim <- terra::mask(x_sim, x)

  # transform value range of simulated raster
  x_sim <- terra::values(x) %>%
    na.omit(.) %>%
    range(.) %>%
    scales::rescale(terra::as.matrix(x_sim, wide = TRUE), to = .) %>%
    matrix(., nrow = nrow(x_sim), ncol = ncol(x_sim)) %>%
    terra::rast(.)

  terra::ext(x_sim) <- terra::ext(x)
  terra::crs(x_sim) <- terra::crs(x)

  # if input values are all integers, make all simulated values integers
  if (all(na.omit(terra::values(x)) %% 1 == 0)) {terra::values(x_sim) <- round(terra::values(x_sim))}

  # output
  return(x_sim)

}
