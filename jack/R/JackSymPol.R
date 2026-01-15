#' Jack polynomial with symbolic Jack parameter
#'
#' Returns the Jack polynomial with a symbolic Jack parameter.
#'
#' @param n number of variables, a positive integer
#' @param lambda an integer partition, given as a vector of decreasing
#'   integers
#' @param which which Jack polynomial, \code{"J"}, \code{"P"}, \code{"Q"},
#'  or \code{"C"}
#'
#' @return A \code{symbolicQspray} object.
#'
#' @export
#' @importFrom symbolicQspray symbolicQspray_from_list Qzero Qone
#'
#' @examples
#' JackSymPol(3, lambda = c(3, 1))
JackSymPol <- function(n, lambda, which = "J") {
  stopifnot(isPositiveInteger(n), isPartition(lambda))
  lambda <- as.integer(removeTrailingZeros(lambda))
  which <- match.arg(which, c("J", "P", "Q", "C"))
  if(length(lambda) == 0L) {
    return(Qone())
  }
  if(n == 0L){
    return(Qzero())
  }
  JackPolynomial <- symbolicQspray_from_list(
    JackSymPolRcpp(as.integer(n), lambda)
  )
  if(which != "J") {
    if(which == "C") {
      K <- symbolicJackCcoefficient(lambda)
      JackPolynomial <- K * JackPolynomial
    } else {
      invK <- switch(
        which,
        "P" = symbolicJackPcoefficientInverse(lambda),
        "Q" = symbolicJackQcoefficientInverse(lambda)
      )
      JackPolynomial <- JackPolynomial / invK
    }
  }
  JackPolynomial
}
