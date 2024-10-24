# Odirect ----
#' @name Odirect
#' @title Direct overlap between boundary elements.
#' @description Statistical test for the number of directly overlapping boundary elements of two traits.
#'
#' @param x A SpatRaster object with boundary elements.
#' @param y A SpatRaster object with boundary elements.
#' @param null_distrib A list of probability functions output from overlap_null_distrib().
#'
#' @return The number of directly overlapping boundary elements and a p-value.
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
#' Tcrist_boundaries <- categorical_boundary(T.cristatus)
#' grassland_boundaries <- define_boundary(grassland, 0.1)
#' 
#' Odirect(Tcrist_boundaries, grassland_boundaries, Tcrist_ovlp_null)
#' }
#'
#' @author Amy Luo
#' @references
#' Jacquez, G.M., Maruca,I S. & Fortin, M.-J. (2000) From fields to objects: A review of geographic boundary analysis. Journal of Geographical Systems, 3, 221, 241.
#' Fortin, M.-J., Drapeau, P. & Jacquez, G.M. (1996) Quantification of the Spatial Co-Occurrences of Ecological Boundaries. Oikos, 77, 51-60.
#' @export
Odirect <- function(x, y, null_distrib) {
  xx <- terra::cells(x, 1)[[1]]
  yy <- terra::cells(y, 1)[[1]]
  count <- length(intersect(xx, yy))

  p <- null_distrib$Odirect(count) %>%
    ifelse(. > 0.5, 1 - ., .) %>%
    as.numeric(.)
  names(p) <-'p-value'
  names(count) <- 'n overlapping boundary elements'

  return(c(count, p))
}

# Ox ----
#' @name Ox
#' @title Average minimum distance from x boundary elements to nearest y boundary element.
#' @description
#' Statistical test for the average minimum distance between each boundary element in raster x
#' and the nearest boundary element in raster y. Uses Euclidean distance. The boundaries of
#' trait x depend on the boundaries of trait y.
#'
#' @param x A SpatRaster object with boundary elements.
#' @param y A SpatRaster object with boundary elements.
#' @param null_distrib A list of probability functions output from overlap_null_distrib().
#'
#' @return The average minimum distance and a p-value.
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
#' Tcrist_boundaries <- categorical_boundary(T.cristatus)
#' grassland_boundaries <- define_boundary(grassland, 0.1)
#' 
#' Ox(Tcrist_boundaries, grassland_boundaries, Tcrist_ovlp_null)
#' }
#'
#' @author Amy Luo
#' @references
#' Jacquez, G.M., Maruca,I S. & Fortin,M.-J. (2000) From fields to objects: A review of geographic boundary analysis. Journal of Geographical Systems, 3, 221, 241.
#' Fortin, M.-J., Drapeau, P. & Jacquez, G.M. (1996) Quantification of the Spatial Co-Occurrences of Ecological Boundaries. Oikos, 77, 51-60.
#' @export
Ox <- function(x, y, null_distrib) {
  x_min_distances <- c()

  x_bound_cells <- terra::xyFromCell(x, terra::cells(x, 1)[[1]])
  y_bound_cells <- terra::xyFromCell(y, terra::cells(y, 1)[[1]])
  dists <- terra::distance(x_bound_cells, y_bound_cells, lonlat = T)
  for (i in sequence(nrow(dists))) {x_min_distances <- append(x_min_distances, min(dists[i,]))} # for each x boundary cell, the minimum distance to a y boundary cell

  ave_min_dist <- mean(x_min_distances)
  names(ave_min_dist) <- 'average minimum distance (x depends on y)'

  p <- null_distrib$Ox(ave_min_dist) %>%
    ifelse(. > 0.5, 1 - ., .) %>%
    as.numeric(.)
  names(p) <- 'p-value'
  return(c(ave_min_dist, p))
}

# Oxy ----
#' @name Oxy
#' @title Average minimum distance between boundary elements of two variables
#' @description
#' Statistical test for the average minimum distance between boundary elements in two raster layers.
#' Uses Euclidean distance. Boundaries for each trait affect one another reciprocally (x affects y
#' and y affects x).
#'
#' @param x A SpatRaster object with boundary elements.
#' @param y A SpatRaster object with boundary elements.
#' @param null_distrib A list of probability functions output from overlap_null_distrib().
#'
#' @return p-value
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
#' Tcrist_boundaries <- categorical_boundary(T.cristatus)
#' grassland_boundaries <- define_boundary(grassland, 0.1)
#' 
#' Oxy(Tcrist_boundaries, grassland_boundaries, Tcrist_ovlp_null)
#' }
#'
#' @author Amy Luo
#' @references
#' Jacquez, G.M., Maruca,I S. & Fortin, M.-J. (2000) From fields to objects: A review of geographic boundary analysis. Journal of Geographical Systems, 3, 221, 241.
#' Fortin, M.-J., Drapeau, P. & Jacquez, G.M. (1996) Quantification of the Spatial Co-Occurrences of Ecological Boundaries. Oikos, 77, 51-60.
#' @export
Oxy <- function(x, y, null_distrib) {
  min_distances <- c()
  
  x_bound_cells <- terra::xyFromCell(x, terra::cells(x, 1)[[1]])
  y_bound_cells <- terra::xyFromCell(y, terra::cells(y, 1)[[1]])
  dists <- terra::distance(x_bound_cells, y_bound_cells, lonlat = T)
  for (i in sequence(nrow(dists))) {min_distances <- append(min_distances, min(dists[i,]))} # for each x boundary cell, the minimum distance to a y boundary cell
  for (i in sequence(ncol(dists))) {min_distances <- append(min_distances, min(dists[,i]))} # for each x boundary cell, the minimum distance to a y boundary cell
  
  ave_min_dist <- mean(min_distances)
  names(ave_min_dist) <- 'average minimum distance'

  p <- null_distrib$Oxy(ave_min_dist) %>%
    ifelse(. > 0.5, 1 - ., .) %>%
    as.numeric(.)
  names(p) <- 'p-value'
  return(c(ave_min_dist, p))
}
