
#' find_neighbors2.
#' 
#' @description
#' find_neighbors2 was used to find the neighborhood of spot. 
#'
#' @details find_neighbors2 was used to find the neighborhood of spot. 
#' @param sce is a SingleCellExperiment object containing PCA and position informatin. 
#' @param platform is the name of spatial transcriptomic platform. Specify 'Visium' for hex lattice geometry or 'ST' for square lattice geometry. Specifying this parameter is optional as this information is included in their metadata. 
#' @return a sparse matrix recording the information of neighborhood. 
#' @examples 
#' data(sce)
#' platform = "ST"
#' Adj <- find_neighbors2(sce, platform)
#' @export
find_neighbors2 <- function(sce, platform) {
  if (platform == "Visium") {
    ## Spots to left and right, two above, two below
    offsets <- data.frame(x.offset=c(-2, 2, -1,  1, -1, 1),
                          y.offset=c( 0, 0, -1, -1,  1, 1))
  } else if (platform == "ST") {
    ## L1 radius of 1 (spots above, right, below, and left)
    offsets <- data.frame(x.offset=c( 0, 1, 0, -1),
                          y.offset=c(-1, 0, 1,  0))
  } else {
    stop(".find_neighbors: Unsupported platform \"", platform, "\".")
  }
  
  ## Get array coordinates (and label by index of spot in SCE)
  spot.positions <- SingleCellExperiment::colData(sce)[, c("col", "row")]
  spot.positions$spot.idx <- seq_len(nrow(spot.positions))
  
  ## Compute coordinates of each possible spot neighbor
  neighbor.positions <- merge(spot.positions, offsets)
  neighbor.positions$x.pos <- neighbor.positions$col + neighbor.positions$x.offset
  neighbor.positions$y.pos <- neighbor.positions$row + neighbor.positions$y.offset
  
  ## Select spots that exist at neighbor coordinates
  neighbors <- merge(as.data.frame(neighbor.positions), 
                     as.data.frame(spot.positions), 
                     by.x=c("x.pos", "y.pos"), by.y=c("col", "row"),
                     suffixes=c(".primary", ".neighbor"),
                     all.x=TRUE)
  
  ## Shift to zero-indexing for C++
  #neighbors$spot.idx.neighbor <- neighbors$spot.idx.neighbor - 1
  
  ## Group neighbor indices by spot 
  ## (sort first for consistency with older implementation)
  neighbors <- neighbors[order(neighbors$spot.idx.primary, 
                               neighbors$spot.idx.neighbor), ]
  df_j <- split(neighbors$spot.idx.neighbor, neighbors$spot.idx.primary)
  df_j <- unname(df_j)
  
  ## Discard neighboring spots without spot data
  ## This can be implemented by eliminating `all.x=TRUE` above, but
  ## this makes it easier to keep empty lists for spots with no neighbors
  ## (as expected by C++ code)
  df_j <- purrr::map(df_j, function(nbrs) purrr::discard(nbrs, function(x) is.na(x)))
  
  ## Log number of spots with neighbors
  n_with_neighbors <- length(purrr::keep(df_j, function(nbrs) length(nbrs) > 0))
  message("Neighbors were identified for ", n_with_neighbors, " out of ",
          ncol(sce), " spots.")
  
  n <- length(df_j) 
  ##D <- matrix(0,  nrow = n, ncol = n)
  D <- Matrix::sparseMatrix(i = 1:n, j = 1:n, x = 0)
  for (i in 1:n) {
    if(length(df_j[[i]]) != 0)
      D[i, df_j[[i]]] <- 1
  }
  D
}


