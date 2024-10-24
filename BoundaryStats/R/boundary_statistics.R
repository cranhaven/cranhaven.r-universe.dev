# number of subgraphs ----
#' @name n_subgraph
#' @title Number of subgraphs
#' @description Statistical test the for number of subgraphs, or sets of contiguous boundary elements, in the data.
#'
#' @param x A SpatRaster object with boundary elements.
#' @param null_distrib A list of probability functions output from boundary_null_distrib().
#'
#' @return The number of subgraphs in the raster and a p-value.
#' @examples \donttest{
#' data(T.cristatus)
#' T.cristatus <- terra::rast(T.cristatus_matrix, crs = T.cristatus_crs)
#' terra::ext(T.cristatus) <- T.cristatus_ext
#'
#' T.crist_boundaries <- categorical_boundary(T.cristatus)
#' T.crist_bound_null <- boundary_null_distrib(T.cristatus, cat = TRUE, n_iterations = 100,
#' model = 'random_cluster')
#'
#' n_subgraph(T.crist_boundaries, T.crist_bound_null)
#' }
#' 
#' @author Amy Luo
#' @references Jacquez, G.M., Maruca,I S. & Fortin M.-J. (2000) From fields to objects: A review of geographic boundary analysis. Journal of Geographical Systems, 3, 221, 241.
#' @export
n_subgraph <- function(x, null_distrib) {
  count <- terra::patches(x, directions = 8, zeroAsNA = TRUE) %>%
    terra::values(.) %>%
    max(., na.rm = TRUE)

  p <- null_distrib$n_subgraph(count) %>%
    ifelse(. > 0.5, 1 - ., .) %>%
    as.numeric(.)

  names(count) <- 'n subgraphs'
  names(p) <-'p-value'
  return(c(count, p))
}

#length of the longest subgraph ----
#' @name max_subgraph
#' @title Length of the longest subgraph
#' @description Statistical test for the length of the longest subgraph, or set of contiguous boundary elements.
#'
#' @param x A SpatRaster object with boundary elements.
#' @param null_distrib A list of probability functions output from boundary_null_distrib().
#'
#' @return The length of the longest subgraph and a p-value.
#' @examples \donttest{
#' data(T.cristatus)
#' T.cristatus <- terra::rast(T.cristatus_matrix, crs = T.cristatus_crs)
#' terra::ext(T.cristatus) <- T.cristatus_ext
#'
#' Tcrist_boundaries <- categorical_boundary(T.cristatus)
#' T.crist_bound_null <- boundary_null_distrib(T.cristatus, cat = TRUE, n_iterations = 100,
#' model = 'random_cluster')
#'
#' max_subgraph(Tcrist_boundaries, T.crist_bound_null)
#' }
#' 
#' @author Amy Luo
#' @references Jacquez, G.M., Maruca,I S. & Fortin M.-J. (2000) From fields to objects: A review of geographic boundary analysis. Journal of Geographical Systems, 3, 221, 241.
#' @export
max_subgraph <- function(x, null_distrib) {
  xpolygon <- terra::subst(x, 0, NA) %>%
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

  max_length <- max(lengths)

  # statistical test
  p <- null_distrib$longest_subgraph(max_length) %>%
    ifelse(. > 0.5, 1 - ., .) %>%
    as.numeric(.)

  names(p) <-'p-value'
  names(max_length) <- 'length of longest boundary'
  return(c(max_length, p))
}
