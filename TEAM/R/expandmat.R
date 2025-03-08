#' Enumerate binomial support
#'
#' Enumerate possible counts for calculating binomial probability
#'
#' @param mat Matrix
#' @param vec Numeric Vector
#'
#' @return NULL
#'
expand.mat <- function(mat, vec) {
  out = matrix(0, nrow = as.numeric(nrow(mat)) * as.numeric(length(vec)),
               ncol = as.numeric(ncol(mat) + 1)) #deal with integer overflow
  for (i in 1:ncol(mat)) out[, i] = mat[, i]
  out[, ncol(mat) + 1] = rep(vec, each = nrow(mat))
  return(out)
}

