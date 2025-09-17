#' Convenience function for stacking matrices into an array.
#'
#' This function extends the abind function from the abind package.
#'
#'
#' @importFrom abind abind
#' @param ... Any number of matrices of equal dimension to stack together into
#'  a 3d matrix
#' 

abind3 = function(...) {
  
  abind(..., along=3)
  
}