#' @title Principal Sturm-Habicht sequence of a polynomial
#' @description Principal Sturm-Habicht sequence of a polynomial with rational
#'   coefficients.
#'
#' @param qspray a \code{qspray} polynomial with at most nine variables
#' @param var integer indicating with respect to which variable the resultant
#'   is desired (e.g. \code{1} for \code{x} and \code{2} for \code{y})
#'
#' @return For a univariate polynomial, this returns a vector of \code{bigq}
#'   rational numbers.
#'   For a multivariate polynomial, this returns a list of \code{qspray}
#'   polynomials that do not involve the \code{var}-th variable.
#' @export
#' @importFrom qspray qsprayMaker numberOfVariables permuteVariables showQsprayXYZ showQsprayX1X2X3 showQsprayOption<-
#' @importFrom gmp as.bigq
#'
#' @examples
#' library(resultant)
#' x <- qlone(1)
#' y <- qlone(2)
#' qspray <- x^3*y^2 - 5*x*y^2 + 7*x - 2
#' principalSturmHabicht(qspray, var = 1)
#' principalSturmHabicht(qspray, var = 2)
principalSturmHabicht <- function(qspray, var = 1) {
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
  coeffs <- qspray@coeffs
  pows <- vapply(qspray@powers, function(pwrs) {
    out <- integer(n)
    out[seq_along(pwrs)] <- pwrs
    out
  }, integer(n))
  if(n == 1L) {
    as.bigq(principalSturmHabichtCPP1(
      pows, coeffs
    ))
  } else {
    permutation <- makePermutation(n, var)
    ipermutation <- inversePermutation(permutation) - 1L
    if(n == 2L) {
      PSHsequence <- principalSturmHabichtCPP2(
        pows, coeffs,
        ipermutation
      )
    } else if(n == 3L) {
      PSHsequence <- principalSturmHabichtCPP3(
        pows, coeffs,
        ipermutation
      )
    } else if(n == 4L) {
      PSHsequence <- principalSturmHabichtCPP4(
        pows, coeffs,
        ipermutation
      )
    } else if(n == 5L) {
      PSHsequence <- principalSturmHabichtCPP5(
        pows, coeffs,
        ipermutation
      )
    } else if(n == 6L) {
      PSHsequence <- principalSturmHabichtCPP6(
        pows, coeffs,
        ipermutation
      )
    } else if(n == 7L) {
      PSHsequence <- principalSturmHabichtCPP7(
        pows, coeffs,
        ipermutation
      )
    } else if(n == 8L) {
      PSHsequence <- principalSturmHabichtCPP8(
        pows, coeffs,
        ipermutation
      )
    } else if(n == 9L) {
      PSHsequence <- principalSturmHabichtCPP9(
        pows, coeffs,
        ipermutation
      )
    }
    if(n <= 3L) {
      showFunc <- showQsprayXYZ()
    } else {
      showFunc <- showQsprayX1X2X3()
    }
    lapply(PSHsequence, function(psh) {
      p <- qsprayMaker(
        powers = Columns(psh[["Powers"]]),
        coeffs = psh[["Coeffs"]]
      )
      p <- permuteVariables(p, permutation)
      showQsprayOption(p, "showQspray") <- showFunc
      p
    })
  }
}
