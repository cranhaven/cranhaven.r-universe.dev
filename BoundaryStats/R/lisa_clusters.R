#' @name lisa_clusters
#' @title Identify LISA clusters
#' @description Uses local Moran's I and Monte Carlo simulations to identify LISA clusters
#'
#' @param x A SpatRaster object.
#' @return An sf polygons object with LISA clusters
#'
#' @author Amy Luo
#' @references
#' Anselin, L. (1995). Local Indicators of Spatial Association—LISA. Geographical Analysis, 27(2), 93–115.
#' @export
lisa_clusters <- function(x) {
  # Monte Carlo simulations of local Moran's I for each raster cell
  rep <- 1
  distribs <- data.frame(1:terra::ncell(x))[,-1]
  while (rep <= 100) {
    distribs <- BoundaryStats::random_raster_sim(x) %>%
      terra::autocor(., global = F) %>%
      terra::values() %>%
      as.data.frame() %>%
      dplyr::rename_with(~paste('iter', rep, sep = '_')) %>%
      cbind(distribs, .)
    rep = rep + 1
  }
  distribs <- distribs %>%
    na.omit %>%
    t %>%
    as.data.frame

  # empirical values of local Moran's I
  empirical_i <- terra::autocor(x, global = F)

  # p-values & significance for each cell
  pvals <- data.frame(cell = numeric(), p = numeric())
  for (i in 1:ncol(distribs)) {
    pvals <- stats::pnorm(terra::values(empirical_i)[as.numeric(names(distribs)[i])], mean(distribs[[i]]), stats::sd(distribs[[i]]), lower.tail = F) %>%
      data.frame(cell = as.numeric(names(distribs)[i]), p = .) %>%
      rbind(pvals, .)
  }
  pvals <- pvals %>%
    dplyr::mutate(cluster = ifelse(p < 0.05, 1, NA))

  # define LISA clusters
  x_cluster <- x
  for (i in 1:nrow(pvals)) {
    terra::values(x_cluster)[pvals[i,1]] <- pvals[i,3]
  }
  x_cluster <- x_cluster %>%
    terra::as.polygons(na.rm = T) %>%
    terra::buffer(., 0.01) %>%
    terra::disagg() %>%
    sf::st_as_sf()

  # median lengths of LISA clusters
  lengths <- c()
  for (i in 1:nrow(x_cluster)) {
    lengths <- sf::st_geometry(x_cluster[i,]) %>%
      sf::st_cast(., "POINT") %>%
      sf::st_distance() %>%
      max() %>%
      append(lengths, .) %>%
      as.numeric()
  }
  median_length <- median(lengths)

  cell_size <- terra::cellSize(x, transform = T) %>%
    terra::values() %>%
    mean
  corr_range <- median_length/sqrt(cell_size)

  # export
  return(corr_range)
}
