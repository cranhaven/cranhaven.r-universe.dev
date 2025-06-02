#' Title matzero
#'
#' @param mat matrix of spectra
#' @param spks number of the peak selected as the starting point
#' @param x variable to be reset according to the position of the selected peak
#' @param colname name of the selected column
#' @param myby  varialbe selected for subsetting the matrix
#'
#' @description zeroes time (in seconds) according to peak given by the user
#'
matzero <- function(mat, spks=1, x=mat$time.seconds.zero, colname="v.check",myby="id_cycle") {
  pks <- NULL
  mat.zero <- mat[mat[, pks == spks]][, c(colname) := (x - min(x)), by = myby][]
  return(mat.zero)
}
