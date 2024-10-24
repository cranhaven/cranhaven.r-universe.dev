#' @name overlap_null_distrib
#' @title Null distribution for boundary overlap statistics
#' @description
#' Creates custom probability distributions for three boundary overlap statistics (directly overlapping
#' boundary elements, minimum distance between boundary elements in x to y, and minimum distance
#' between elements in x and y). Given two SpatRaster objects with the same extent, projection, and
#' resolution, simulates n iterations of random raster surfaces from neutral model(s).
#'
#' @param x A SpatRaster object. If rand_both = FALSE, only this raster will be modeled.
#' @param y A SpatRaster object. If rand_both = FALSE, this raster does not change.
#' @param rand_both TRUE if distribution of traits in x and y should be modeled.
#' @param x_convert TRUE if x contains numeric trait data that needs to be converted to boundary intensities. default = FALSE.
#' @param y_convert TRUE if y contains numeric trait data that needs to be converted to boundary intensities. default = FALSE.
#' @param x_cat TRUE if x contains a categorical variable. default = FALSE.
#' @param y_cat TRUE if y contains a categorical variable. default = FALSE.
#' @param threshold A value between 0 and 1. The proportion of cells to keep as
#' boundary elements. Default = 0.2.
#' @param n_iterations An integer indicating the number of iterations for the function. A value of 100 or 1000
#' is recommended to produce sufficient resolution for downstream statistical tests. default = 10.
#' @param x_model Neutral model to use. Options: 'random' (stochastic), 'gaussian' (Gaussian random field),
#' and 'random_cluster' (modified random clusters method)
#' @param y_model Neutral model to use for y.
#' @param px If using modified random clusters for x, proportion of cells to be marked in percolated raster. Higher values of
#' p produce larger clusters. Default = 0.5
#' @param py If using modified random clusters for y, proportion of cells to be marked in percolated raster. Higher values of
#' p produce larger clusters. Default = 0.5
#' @param progress If progress = TRUE (default) a progress bar will be displayed.
#'
#' @return A list of probability distribution functions for boundary overlap statistics.
#'
#' @examples \donttest{
#' data(T.cristatus)
#' T.cristatus <- terra::rast(T.cristatus_matrix, crs = T.cristatus_crs)
#' terra::ext(T.cristatus) <- T.cristatus_ext
#'
#' data(grassland)
#' grassland <- terra::rast(grassland_matrix, crs = grassland_crs)
#' terra::ext(grassland) <- grassland_ext
#'
#' Tcrist_ovlp_null <- overlap_null_distrib(T.cristatus, grassland, rand_both = FALSE,
#'   x_cat = TRUE, n_iterations = 100, x_model = 'random_cluster')
#' }
#'
#' @author Amy Luo
#' @references Saura, S. & Martínez-Millán, J. (2000). Landscape patterns simulation with a modified random clusters method. Landscape Ecology, 15:661-678.
#' @export
overlap_null_distrib <- function(x, y, rand_both, x_convert = FALSE, y_convert = FALSE, x_cat = FALSE, y_cat = FALSE,
                                 threshold = 0.2, n_iterations = 10, x_model = 'random', y_model = 'random',
                                 px = 0.5, py = 0.5, progress = TRUE) {
  # progress bar
  if (progress == TRUE) {progress_bar = txtProgressBar(min = 0, max = n_iterations, initial = 0, char = '+', style = 3)}

  # make output vectors for repeat loop
  Odirect = c(); Ox = c(); Oxy = c()

  # if not randomizing y model, find boundaries in y
  if (rand_both == FALSE) {
    if (y_cat == FALSE) {
      y_boundary <- define_boundary(y, threshold, y_convert)
    } else {
      y_boundary <- categorical_boundary(y)
    }
  }

  # if Gaussian is chosen, calculate spatial autocorrelation distance with LISA clusters
  if (x_model == 'gaussian') {corr_x <- lisa_clusters(x)}
  if (y_model == 'gaussian') {corr_y <- lisa_clusters(y)}

  # n iterations of random boundaries + stats
  rep = 0
  repeat {

    # make random rasters with boundary elements (only for x if not randomizing y) ----
    repeat {
      if (x_model == 'random') {
        x_sim <- random_raster_sim(x)
      } else if (x_model == 'gaussian') {
        x_sim <- gauss_random_field_sim(x, corr_x)
      } else if (x_model == 'random_cluster') {
        x_sim <- mod_random_clust_sim(x, px)
      }

      if (x_cat == FALSE) {
        x_boundary <- define_boundary(x_sim, threshold, x_convert)
      } else {
        x_boundary <- categorical_boundary(x_sim)
      }

      if (sum(terra::values(x_boundary) == 1, na.rm = TRUE) >= 1) {break}
    }

    if (rand_both == TRUE) {
      repeat {
        if (y_model == 'random') {
          y_sim <- random_raster_sim(y)
        } else if (y_model == 'gaussian') {
          y_sim <- gauss_random_field_sim(y, corr_y)
        } else if (y_model == 'random_cluster') {
          y_sim <- mod_random_clust_sim(y, py)
        }

        if (y_cat == FALSE) {
          y_boundary <- define_boundary(y_sim, threshold, y_convert)
        } else {
          y_boundary <- categorical_boundary(y_sim)
        }

        if (sum(terra::values(y_boundary) == 1, na.rm = TRUE) >= 1) {break}
      }
    }

    # direct overlap statistic ----
    xx <- terra::cells(x_boundary, 1)[[1]]
    yy <- terra::cells(y_boundary, 1)[[1]]
    count <- length(intersect(xx, yy))

    Odirect <- append(Odirect, count)

    # average minimum distance statistic (Ox; one boundary influences the other) ----
    x_min_distances <- c()

    x_bound_cells <- terra::xyFromCell(x_boundary, terra::cells(x_boundary, 1)[[1]])
    y_bound_cells <- terra::xyFromCell(y_boundary, terra::cells(y_boundary, 1)[[1]])
    dists <- terra::distance(x_bound_cells, y_bound_cells, lonlat = T)
    for (i in sequence(nrow(dists))) {x_min_distances <- append(x_min_distances, min(dists[i,]))} # for each x boundary cell, the minimum distance to a y boundary cell

    x_ave_min_dist <- mean(x_min_distances)

    Ox <- append(Ox, x_ave_min_dist)

    # average minimum distance statistic (Oxy; both boundaries influence each other) ----
    # distance matrix already calculated in last portion, just recalculate the average minimum distance
    xy_min_distances <- c()
    for (i in sequence(nrow(dists))) {xy_min_distances <- append(xy_min_distances, min(dists[i,]))} # for each x boundary cell, the minimum distance to a y boundary cell
    for (i in sequence(ncol(dists))) {xy_min_distances <- append(xy_min_distances, min(dists[,i]))} # for each x boundary cell, the minimum distance to a y boundary cell

    ave_min_dist <- mean(xy_min_distances)

    Oxy <- mean(xy_min_distances) %>%
      append(Oxy, .)

    # loop count and break ----
    rep = rep + 1
    if (progress == TRUE) {setTxtProgressBar(progress_bar, rep)}
    if (rep >= n_iterations) {
      if (progress == TRUE) {close(progress_bar)}
      break
    }
  }

  # output
  Odirect <- pdqr::new_p(Odirect, type = 'continuous')
  Ox <- pdqr::new_p(Ox, type = 'continuous')
  Oxy <- pdqr::new_p(Oxy, type = 'continuous')
  distribs <- list(Odirect, Ox, Oxy)
  names(distribs) <- c('Odirect', 'Ox', 'Oxy')

  message('DONE')

  return(distribs)

}
