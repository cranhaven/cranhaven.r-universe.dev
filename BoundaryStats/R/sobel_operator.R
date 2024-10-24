#' @name sobel_operator
#' @title Sobel-Feldman operator for edge detection
#' @description Uses a Sobel-Feldman operator (3x3 kernel) to detect internal edges in a SpatRaster object.
#' 
#' @param x A SpatRaster object.
#' @return A SpatRaster object with boundary values.
#' 
#' @examples
#' data(T.cristatus)
#' T.cristatus <- terra::rast(T.cristatus_matrix, crs = T.cristatus_crs)
#' terra::ext(T.cristatus) <- T.cristatus_ext
#' 
#' edges <- sobel_operator(T.cristatus)
#' terra::plot(edges)
#' 
#' @author Amy Luo
#' @export
sobel_operator <- function(x) {
  x_mat <- terra::as.matrix(x, wide = TRUE)
  gx <- matrix(c(1, 2, 1, 0, 0, 0, -1, -2, -1), nrow = 3)
  gy <- matrix(c(1, 0, -1, 2, 0, -2, 1, 0, -1), nrow = 3)
  
  d <- matrix(nrow = nrow(x_mat), ncol = ncol(x_mat))
  
  a <- matrix(nrow = 3, ncol = 3)
  for (row in 1:nrow(x_mat)) {
    for (col in 1:ncol(x_mat)) {
      # values in raster to multiply by mask
      if(row - 1 > 0 & col - 1 > 0) {a[1,1] = x_mat[row - 1, col - 1]} else {a[1,1] = 0}
      if (row - 1 > 0) {a[1,2] = x_mat[row - 1, col]} else {a[1,2] = 0}
      if(row - 1 > 0 & col < ncol(x_mat)) {a[1,3] = x_mat[row - 1, col + 1]} else {a[1,3] = 0}
      
      if (col - 1 > 0) {a[2,1] = x_mat[row, col - 1]} else {a[2,1] = 0}
      a[2,2] = x_mat[row, col]
      if (col < ncol(x_mat)) {a[2,3] = x_mat[row, col + 1]} else {a[2,3] = 0}
      
      if (row < nrow(x_mat) & col - 1 > 0) {a[3,1] = x_mat[row + 1, col - 1]} else {a[3,1] = 0}
      if (row < nrow(x_mat)) {a[3,2] = x_mat[row + 1, col]} else {a[3,2] = 0}
      if (row < nrow(x_mat) & col < ncol(x_mat)) {a[3,3] = x_mat[row + 1, col + 1]} else {a[3,3] = 0}

      gxa <- sum(gx*a)
      gya <- sum(gy*a)
      d[row, col] = sqrt(gxa ^ 2 + gya ^ 2)
    }
  }

  out <- terra::rast(d)
  
  for (i in 1:length(out)) {
    if (length(terra::adjacent(out, i, 'queen')) < 8) {terra::values(out)[i] = 0}
  }
  
  terra::ext(out) <- terra::ext(x)
  terra::crs(out) <- terra::crs(x)

  return(out)
}
