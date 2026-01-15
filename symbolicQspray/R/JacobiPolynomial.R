#' @title Jacobi polynomial
#' @description Computes the n-th Jacobi polynomial as a
#'   \code{symbolicQspray}.
#'
#' @param n index (corresponding to the degree), a positive integer
#'
#' @return A \code{symbolicQspray} object representing the n-th Jacobi
#'   polynomial.
#' @export
#' @importFrom qspray qlone
#'
#' @details The Jacobi polynomials are univariate polynomials whose
#'   coefficients depend on two parameters.
#'
#' @examples
#' JP1 <- JacobiPolynomial(1)
#' showSymbolicQsprayOption(JP1, "showRatioOfQsprays") <-
#'   showRatioOfQspraysXYZ(c("alpha", "beta"))
#' JP1
JacobiPolynomial <- function(n) {
  stopifnot(isPositiveInteger(n))
  if(n == 0L) {
    Qone()
  } else if(n == 1L) {
    X     <- Qlone(1)
    alpha <- qlone(1)
    beta  <- qlone(2)
    (alpha + 1L) + (alpha + beta + 2L) * (X - 1L)/2L
  } else {
    X     <- Qlone(1)
    alpha <- qlone(1)
    beta  <- qlone(2)
    a <- n + alpha
    b <- n + beta
    c <- a + b
    K <- 2L * n * (c - n) * (c - 2L)
    lambda1 <- ((c - 1L) * (c * (c - 2L) * X + (a - b) * (c - 2L*n))) / K
    lambda2 <- (2L * (a - 1L) * (b - 1L) * c) / K
    (lambda1 * JacobiPolynomial(n - 1L) - lambda2 * JacobiPolynomial(n - 2L))
  }
}
