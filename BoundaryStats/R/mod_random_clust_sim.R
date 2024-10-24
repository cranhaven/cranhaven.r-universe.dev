#' @name mod_random_clust_sim
#' @title Modified random cluster neutral landscape model
#' @description Simulates a neutral landscape of the same extent and resolution as the input raster, with the same
#' distribution of values.
#'
#' @param x A SpatRaster object.
#' @param p The proportion of cells to be marked in percolated raster. Higher values of p produce larger clusters.
#' @return A SpatRaster object with boundary elements.
#' 
#' @examples
#' data(grassland)
#' grassland <- terra::rast(grassland_matrix, crs = grassland_crs)
#' terra::ext(grassland) <- grassland_ext
#' 
#' simulation <- mod_random_clust_sim(grassland, p = 0.6)
#' terra::plot(simulation)
#' 
#' @author Amy Luo
#' @references
#' Saura, S. & Martínez-Millán, J. (2000) Landscape patterns simulation with a modified random clusters method. Landscape Ecology, 15, 661 – 678.
#' @export
mod_random_clust_sim <- function (x, p) {
  # A: make percolated raster, where proportion of filled cells = p_cluster
  x_sim <- matrix(nrow = terra::nrow(x), ncol = terra::ncol(x))
  for (i in 1:length(x_sim)) {
    if (stats::runif(1) <= p) {x_sim[i] = 1} else {x_sim[i] = 0}
  }
  
  # B: find clusters in raster
  x_sim <- terra::rast(x_sim, crs = terra::crs(x), ext = terra::ext(x)) %>%
    terra::patches(., zeroAsNA = TRUE)
  x_sim[is.na(x_sim)] <- 0
  x_sim[x_sim != 0] <- x_sim[x_sim != 0] + nrow(terra::unique(x))

  # C: assign clusters to categories
  prop <- terra::freq(x, digits = 2) %>% # proportions of cells in category
    as.data.frame(.) %>%
    tibble::add_column(., p = .$count/sum(.$count)) %>%
    .[order(.$p),c(2, 4)]
  clump_selection_order <- sample(terra::unique(x_sim)[-1,])
  areas <- terra::cellSize(x_sim)
  total_area <- terra::expanse(x_sim)
  
  row = 1
  for (i in clump_selection_order) {
    assignment = prop[row, 1]; max_prop = prop[row, 2]
    terra::values(x_sim)[terra::values(x_sim) == i] <- assignment
    
    current_prop <- which(terra::values(x_sim) == assignment) %>%
      areas[.] %>%
      sum(.)/total_area %>%
      .[,2]
    if (current_prop >= max_prop) {row = row + 1}
    
    if (row > nrow(prop)) {break}
  }
  
  # D: fill in the rest of the cells
  cells_to_fill <- terra::cells(x_sim, 0)[[1]]
  for (i in cells_to_fill) {
    choices <- terra::adjacent(x_sim, i)[1,] %>%
      x_sim[.] %>%
      subset(., patches != 0) %>%
      table(.) %>%
      as.data.frame(.)

    if (length(choices != 0)) {
      choices <- as.vector(choices[choices$Freq == max(choices$Freq), 1])
      if (length(choices > 1)) {x_sim[i] = as.numeric(sample(choices, 1))} else {x_sim[1] = as.numeric(choices)}
    } else {
      x_sim[i] <- sample(prop[,1], 1, prob = prop[,2])
    }
  }
  # crop extent of filled values to input data range
  x_sim <- terra::mask(x_sim, x)
  
  return(x_sim)
  
}
