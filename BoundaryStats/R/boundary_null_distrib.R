#' @name boundary_null_distrib
#' @title Null distribution for overlap statistics
#' @description
#' Creates custom probability distributions for two boundary statistics (number of subgraphs and length
#' of the longest subgraph). Given a SpatRaster object, simulates n iterations of random raster
#' surfaces from a neutral model.
#'
#' @param x A SpatRaster object.
#' @param convert TRUE if x contains numeric trait data that needs to be converted to boundary intensities. default = FALSE.
#' @param cat TRUE if the input SpatRaster contains a categorical variable. default = FALSE.
#' @param threshold A value between 0 and 1. The proportion of cells to keep as boundary elements. default = 0.2.
#' @param n_iterations An integer indicating the number of iterations for the function.
#' A value of 100 or 1000 is recommended to produce sufficient resolution for downstream
#' statistical tests. default = 10.
#' @param model Neutral model to use. Options: 'random' (stochastic), 'gaussian' (Gaussian random field),
#' and 'random_cluster' (modified random clusters method)
#' @param p If using modified random clusters, proportion of cells to be marked in percolated raster.Higher values of p
#' produce larger clusters. Default: p = 0.5
#' @param progress If progress = TRUE (default) a progress bar will be displayed.
#'
#' @return A list of two probability distribution functions for boundary statistics.
#' @examples \donttest{
#' data(T.cristatus)
#' T.cristatus <- terra::rast(T.cristatus_matrix, crs = T.cristatus_crs)
#' terra::ext(T.cristatus) <- T.cristatus_ext
#'
#' T.crist_bound_null <- boundary_null_distrib(T.cristatus, cat = TRUE, n_iterations = 100,
#' model = 'random_cluster')
#' }
#'
#' @author Amy Luo
#' @references Saura, S. & Martínez-Millán, J. (2000). Landscape patterns simulation with a modified random clusters method. Landscape Ecology, 15:661-678.
#' @export
boundary_null_distrib <- function(x, convert = FALSE, cat = FALSE, threshold = 0.2, n_iterations = 10, model = 'random', p = 0.5,
                                  progress = TRUE) {
  # progress bar
  if (progress == TRUE) {progress_bar = txtProgressBar(min = 0, max = n_iterations, initial = 0, char = '+', style = 3)}

  # make output vectors for repeat loop
  n_subgraph = c(); longest_subgraph = c()

  # if Gaussian is chosen, calculate spatial autocorrelation distance with LISA clusters
  if (model == 'gaussian') {corr <- lisa_clusters(x)}

  # n iterations of random boundaries + stats
  rep = 0
  repeat {
    # simulate random raster with boundary elements ----
    repeat {
      if (model == 'random') {
        x_sim <- random_raster_sim(x)
      } else if (model == 'gaussian') {
        x_sim <- gauss_random_field_sim(x, corr)
      } else if (model == 'random_cluster') {
        x_sim <- mod_random_clust_sim(x, p)
      }

      if (cat == F) {
        x_boundary <- define_boundary(x_sim, threshold, convert)
      } else {
        x_boundary <- categorical_boundary(x_sim)
      }

      if (sum(terra::values(x_boundary) == 1, na.rm = TRUE) >= 1) {break}
    }

    # number of subgraphs ----
    count <- terra::patches(x_boundary, directions = 8, zeroAsNA = TRUE) %>%
      terra::values(.) %>%
      max(., na.rm = TRUE)

    n_subgraph = append(n_subgraph, count)

    # maximum length of a subgraph ----
    xpolygon <- terra::subst(x_boundary, 0, NA) %>%
      terra::as.polygons(., na.rm = TRUE) %>%
      terra::buffer(., 0.001) %>%
      terra::disagg(.) %>%
      sf::st_as_sf(.)

    lengths <- c()
    for (i in 1:nrow(xpolygon)) {
      lengths <- sf::st_geometry(xpolygon[i,]) %>%
        sf::st_cast(., "POINT") %>%
        sf::st_distance(.) %>%
        max(.) %>%
        append(lengths, .) %>%
        as.numeric(.)
    }

    longest_subgraph <- max(lengths) %>%
      append(longest_subgraph, .)

    # loop count and break ----
    rep = rep + 1
    if (progress == TRUE) {setTxtProgressBar(progress_bar, rep)}
    if (rep >= n_iterations) {
      if (progress == TRUE) {close(progress_bar)}
      break
    }
  }

  # output
  n_subgraph <- pdqr::new_p(n_subgraph, type = 'continuous')
  longest_subgraph <- pdqr::new_p(longest_subgraph, type = 'continuous')
  distribs <- list(n_subgraph, longest_subgraph)
  names(distribs) <- c('n_subgraph', 'longest_subgraph')

  message('DONE')

  return(distribs)

}
