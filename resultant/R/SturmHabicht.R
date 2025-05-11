#' @title Sturm-Habicht sequence of a polynomial
#' @description Sturm-Habicht sequence of a polynomial with rational
#'   coefficients.
#'
#' @param qspray a \code{qspray} polynomial having at most nine variables
#' @param var index of the variable with respect to which the Sturm-Habicht
#'   sequence will be computed
#'
#' @return A list of \code{qspray} polynomials, the Sturm-Habicht sequence of
#'   \code{qspray}, starting with the \code{0}-th Sturm-Habicht polynomial.
#'
#' @export
#' @importFrom qspray qsprayMaker numberOfVariables permuteVariables showQsprayXYZ showQsprayX1X2X3 showQsprayOption<-
#'
#' @examples
#' library(resultant)
#' x <- qlone(1)
#' y <- qlone(2)
#' SturmHabicht(x^3*y^2 + 2*x*y + 1)
SturmHabicht <- function(qspray, var = 1) {
  n <- max(1L, numberOfVariables(qspray))
  if(n >= 10L) {
    stop(
      "Only polynomials with at most nine variables are allowed."
    )
  }
  stopifnot(isPositiveInteger(var))
  if(var > n) {
    stop("Too large value of `var`.")
  }
  var <- as.integer(var)
  permutation <- makePermutation(n, var)
  coeffs <- qspray@coeffs
  pows <- vapply(qspray@powers, function(pwrs) {
    out <- integer(n)
    out[seq_along(pwrs)] <- pwrs
    out
  }, integer(n))
  if(n == 1L) {
    SHsequence <- SturmHabichtCPP1(
      rbind(pows), coeffs, var - 1L
    )
  } else if(n == 2L) {
    SHsequence <- SturmHabichtCPP2(
      pows, coeffs, var - 1L
    )
  } else if(n == 3L) {
    SHsequence <- SturmHabichtCPP3(
      pows, coeffs, var - 1L
    )
  } else if(n == 4L) {
    SHsequence <- SturmHabichtCPP4(
      pows, coeffs, var - 1L
    )
  } else if(n == 5L) {
    SHsequence <- SturmHabichtCPP5(
      pows, coeffs, var - 1L
    )
  } else if(n == 6L) {
    SHsequence <- SturmHabichtCPP6(
      pows, coeffs, var - 1L
    )
  } else if(n == 7L) {
    SHsequence <- SturmHabichtCPP7(
      pows, coeffs, var - 1L
    )
  } else if(n == 8L) {
    SHsequence <- SturmHabichtCPP8(
      pows, coeffs, var - 1L
    )
  } else if(n == 9L) {
    SHsequence <- SturmHabichtCPP9(
      pows, coeffs, var - 1L
    )
  }
  if(n <= 3L) {
    showFunc <- showQsprayXYZ()
  } else {
    showFunc <- showQsprayX1X2X3()
  }
  lapply(SHsequence, function(sh) {
    p <- qsprayMaker(
      powers = Columns(sh[["Powers"]]),
      coeffs = sh[["Coeffs"]]
    )
    p <- permuteVariables(p, permutation)
    showQsprayOption(p, "showQspray") <- showFunc
    p
  })
}
