#' Hessian matrix
#'
#' Creates the hessian matrix for a given likelihood function.
#'
#' @param lik function
#' @param theta kx1 matrix
#' @param ... other parameters passed to \code{lik} function.
#'
#' @return Hessian kxk matrix where k is the number of parameters
#' included in the theta matrix
#'
#' @examples
#' lik <- function(theta) {
#'  return(theta[1]^2 + theta[2]^2)
#' }
#'
#' hessian(lik, c(1, 1))
#'
#' @export
hessian <- function(lik, theta, ...) {
  k <- length(theta)
  hessi <- optimbase::zeros(k, k)
  h <- 1e-3

  for (jc in 1:k) {
    for (jr in 1:jc) {
      x1 <- theta
      x2 <- theta
      x3 <- theta
      x4 <- theta
      x1[jr] <- theta[jr] + h
      x1[jc] <- x1[jc] + h
      x2[jr] <- theta[jr] + h
      x2[jc] <- x2[jc] - h
      x3[jr] <- theta[jr] - h
      x3[jc] <- x3[jc] + h
      x4[jr] <- theta[jr] - h
      x4[jc] <- x4[jc] - h
      hessi[jr, jc] <- -(lik(x1, ...) - lik(x2, ...) - lik(x3, ...) + lik(x4, ...)) / (4 * h^2) # the second symmetric derivative has different formula #
      hessi[jc, jr] <- hessi[jr, jc]
    }
  }
  return(hessi)
}
