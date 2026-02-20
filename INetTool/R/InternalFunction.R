#' get_lower_tri_noDiag
#' @description
#' create a lower triangle of the matrix without the diagonal
#'
#'
#' @param cormat matrix
#' @keywords internal
#'
get_lower_tri_noDiag <- function(cormat){
  cormat[upper.tri(cormat)] <- NA
  diag(cormat) <- NA
  return(cormat)
}
