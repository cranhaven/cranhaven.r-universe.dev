#' @title Generate the correlated binary response data for discrete case
#'
#' @description
#' \code{genBin} generate the data used for discrete case
#'
#' @details
#' genBin returns the correlated binary response according to the value of the
#' corstr argument.
#' @param corstr A character string specifying the correlation structure for the
#'   clusters. Allowed structures are: "independence", "exchangeable" and "ar1".
#' @param mu  A numeric parameter denotes the value of the link function
#' @param size A numeric number indicating the size of the matrix.
#' @return a function to get the correlated binary response data


genBin <- function(corstr, mu=NULL, size=NULL) {
  genBin_ar1 <- function(pvec, rho) {
    sign.rho <- sign(rho)
    n <- length(pvec)
    theta <- stats::qnorm(pvec)
    eps <- stats::rnorm(n)
    z <- eps # only eps[1] will be kept
    u <- stats::rbinom(n, 1, abs(rho))
    for (i in 2:n){
      z[i] <- sign.rho * u[i] * z[i-1] + (1 - u[i]) * eps[i]
    }
    (z <= theta) + 0 # (coerce logical vector to arithmetic vector)
  }

  genBin_CS <- function(pvec,rho) { #pr should be positive
    n <- length(pvec)
    theta <- stats::qnorm(pvec)
    eps0 <- stats::rnorm(1)
    eps <- stats::rnorm(n)
    u <- stats::rbinom(n, 1, sqrt(rho))
    z <- u * eps0 + (1 - u) *eps
    (z <= theta)+0 # (coerce logical vector to arithmetic vector)
  }

  genBin_IN <- function(pvec, rho=NULL) {
    sapply(pvec, stats::rbinom, n=1, size=1)
  }

  f <- switch(corstr,
              "ar1" = genBin_ar1,
              "exchangeable" = genBin_CS,
              "independence" = genBin_IN)
  if (is.null(mu) || is.null(size)) {
    return(f)
  } else {
    return(f(mu, size))
  }
}
