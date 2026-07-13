#' Vectorized wrapped normal density function
#'
#'  This is a modified version of `dwrpnorm` in the `CircStats` package
#'  to allow for multiple angles at once (i.e., vectorized on `theta` and `mu`).
#'
#' @param theta	value at which to evaluate the density function, measured in radians.
#' @param mu	mean direction of distribution, measured in radians.
#' @param rho	mean resultant length of distribution.
#' @param sd different way of select rho, see details below.
#' @param acc	parameter defining the accuracy of the estimation of the density.
#'            Terms are added to the infinite summation that defines the density function
#'            until successive estimates are within acc of each other.
#' @param tol same as `acc`.
#'
#' @author Eliot McIntire
#' @export
#' @rdname dwrpnorm2
#'
#' @examples
#' # Values for which to evaluate density
#' theta <- c(1:500) * 2 * pi / 500
#' # Compute wrapped normal density function
#' density <- c(1:500)
#' for(i in 1:500) {
#'   density[i] <- dwrpnorm2(theta[i], pi, .75)
#' }
#'
#' if (interactive()) {
#'   plot(theta, density)
#' }
#'
#' # Approximate area under density curve
#' sum(density * 2 * pi / 500)
#'
dwrpnorm2 <- function(theta, mu, rho, sd = 1, acc = 1e-05, tol = acc) {
  if (missing(rho)) {
    rho <- exp(-sd ^ 2 / 2)
  }
  len <- max(length(theta), length(mu), length(rho))
  if (length(mu) != len)
    mu <- rep(mu, len)
  if (length(var) != len)
    var <- rep(var, len)
  if (length(theta) != len)
    theta <- rep(theta, len)

  if (rho < 0 || rho > 1)
    stop("rho must be between 0 and 1")

  var <- -2 * log(rho)
  k <- 0
  Next <- .term(theta, mu, var, k)
  Last <- Next
  delta <- rep(1, length(Last))
  while (any(compareNA(delta > tol, TRUE))) {
    keep <- delta > tol
    keep <- compareNA(keep, TRUE)
    k <- k + 1
    Last[keep] <- Next[keep]
    Next[keep] <- Last[keep] + .term(theta[keep], mu[keep], var, k) +
      .term(theta[keep], mu[keep], var, -k)
    delta[keep] <- abs(Next[keep] - Last[keep])
  }
  return(Next)
}

#' @keywords internal
.term <- function(theta, mu, var, k) {
  1 / sqrt(var * 2 * pi) * exp(-((theta - mu + 2 * pi * k) ^ 2) / (2 * var))
}
