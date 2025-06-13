# General utility functions
# Arseniy Khvorov
# Created 2019/08/05
# Last edit 2019/10/15

#' Get paramter names
#'
#' @param x Model matrix
#' @param conventional_names If \code{TRUE} - x names and (Baseline). Otherwise
#'   lambda and betas 
#'
#' @noRd
get_par_names <- function(x, conventional_names = FALSE) {
  if (conventional_names) {
    lambda_name <- "(Baseline)"
    other_names <- colnames(x)
  } else {
    lambda_name <- "theta"
    other_names <- "beta_0"
    if (length(colnames(x)[-1]) > 0)
      other_names <- c(other_names, paste0("beta_", colnames(x)[-1]))
  }
  c(lambda_name, other_names)
}

#' Matrix of coefficients
#'
#' Creates a matrix of all products of pairwise multiplication of elements of x.
#'
#' @param x Model matrix
#'
#' @noRd
get_x_coeffs <- function(x) {
  n_x <- ncol(x)
  n_coefs <- choose(n_x, 2) + n_x
  x_coeffs <- matrix(0, ncol = n_coefs, nrow = nrow(x))
  cur_coeff <- 1
  for (i in 1:n_x) {
    for (j in 1:n_x) {
      if (j < i) next
      x_coeffs[, cur_coeff] <- x[, i] * x[, j]
      cur_coeff <- cur_coeff + 1
    }
  }
  x_coeffs
}

#' Symmetrical matrix
#' 
#' Creates a symmeterical matrix from vectors of unique triangles.
#'
#' @param obj A vector, a matrix or a dataframe of unique triangles.
#'
#' @noRd
build_symm_mat <- function(obj) {
  if (is.vector(obj)) return(build_symm_mat_one(obj))
  if (is.data.frame(obj)) obj <- as.matrix(obj)
  if (!is.matrix(obj))
    abort("obj needs to be a vector, a matrix or a dataframe")
  obj <- split(obj, seq(nrow(obj)))
  lapply(obj, build_symm_mat_one)
}

#' Symmetrical matrix
#' 
#' Creates a symmeterical matrix from a vector of the unique triangle.
#'
#' @param triang_vec A vector representing a unique triangle
#'
#' @noRd
build_symm_mat_one <- function(triang_vec) {
  matdim <- get_symm_dims(length(triang_vec))
  mat <- matrix(0, nrow = matdim, ncol = matdim)
  mat[lower.tri(mat, diag = TRUE)] <- triang_vec
  mat <- t(mat)
  mat[lower.tri(mat, diag = TRUE)] <- triang_vec
  mat
}

# Returns the dimensions of the symmetrical matrix given the number of
# elements in its triangle
get_symm_dims <- function(len) {
  rtexp <- 1 + 8 * len
  rt <- as.integer(sqrt(rtexp))
  if (abs(rt^2 - rtexp) > 10^(-7)) 
    abort(paste0(len, " is not a valid length of triangle"))
  (-1 + rt) / 2
}
