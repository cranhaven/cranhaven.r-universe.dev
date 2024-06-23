#' @title Generate the correlation matrix for the clusteded data
#'
#' @description
#' \code{genCorMat} generate the data with specified correlation matrix.
#'
#' @details
#' genCorMat returns the corresponding correlation matrix according to the value
#' of the corstr argument.
#' @param corstr A character string specifying the correlation structure for the
#'   clusters. Allowed structures are: "independence", "exchangeable" and "ar1".
#' @param rho  A numeric parameter in correlation structure for the
#'   autocorrelation coefficient.
#' @param size A numeric number indicating the size of the matrix.

#' @return a matrix which represents the diffetent correlation matrix.

genCorMat <- function(corstr, rho, size) {
  cormat_fun_ar1 <- function(rho, m) {
    cormat <- diag(m)
    cormat <- rho^abs(row(cormat) - col(cormat))
    return(cormat)
  }

  cormat_fun_exch <- function(rho, m) {
    cormat <- matrix(rep(rho, m^2), c(m, m))
    diag(cormat) <- rep(1, m)
    return(cormat)
  }

  cormat_fun_indep <- function(rho, m) {
    cormat <- diag(m)
    return(cormat)
  }

  f <- switch(corstr,
              "ar1" = cormat_fun_ar1,
              "exchangeable" = cormat_fun_exch,
              "independence" = cormat_fun_indep)
  if (is.null(rho) || is.null(size)) {
    return(f)
  } else {
    return(f(rho, size))
  }
}
