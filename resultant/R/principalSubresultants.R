#' @title Principal subresultants of two polynomials
#' @description Principal subresultants of two polynomials with rational
#'   coefficients.
#'
#' @param qspray1,qspray2 two \code{qspray} polynomials with at most nine
#'   variables
#' @param var integer indicating with respect to which variable the
#'   subresultants are desired (e.g. \code{1} for \code{x} and \code{2} for
#'   \code{y})
#'
#' @return If both \code{qspray1} and \code{qspray2} are univariate polynomials,
#'   the function returns a vector of \code{bigq} rational numbers.
#'   Otherwise, it returns a list of \code{qspray} polynomials that do not
#'   involve the \code{var}-th variable.
#' @export
#' @importFrom qspray qsprayMaker numberOfVariables permuteVariables showQsprayXYZ showQsprayX1X2X3 showQsprayOption<-
#' @importFrom gmp as.bigq
#'
#' @examples
#' library(resultant)
#' x <- qlone(1)
#' y <- qlone(2)
#' p <- x^2 * y * (y^2 - 5*x + 6)
#' q <- x^2 * y * (3*y + 2)
#' principalSubresultants(p, q, var = 1) # should be 0, 0, non-zero, ...
#' principalSubresultants(p, q, var = 2) # should be 0, non-zero, ...
principalSubresultants <- function(qspray1, qspray2, var = 1) {
  n1 <- numberOfVariables(qspray1)
  n2 <- numberOfVariables(qspray2)
  n <- max(1L, n1, n2)
  if(n >= 10L) {
    stop(
      "Only polynomials with at most nine variables are allowed."
    )
  }
  stopifnot(isPositiveInteger(var))
  if(var > n) {
    stop("Too large value of `var`.")
  }
  coeffs1 <- qspray1@coeffs
  coeffs2 <- qspray2@coeffs
  pows1 <- vapply(qspray1@powers, function(pwrs) {
    out <- integer(n)
    out[seq_along(pwrs)] <- pwrs
    out
  }, integer(n))
  pows2 <- vapply(qspray2@powers, function(pwrs) {
    out <- integer(n)
    out[seq_along(pwrs)] <- pwrs
    out
  }, integer(n))
  if(n == 1L) {
    as.bigq(principalSubresultantsCPP1(
      pows1, coeffs1,
      pows2, coeffs2
    ))
  } else {
    permutation <- makePermutation(n, var)
    ipermutation <- inversePermutation(permutation) - 1L
    if(n == 2L) {
      subres <- principalSubresultantsCPP2(
        pows1, coeffs1,
        pows2, coeffs2,
        var == 1
      )
    } else if(n == 3L) {
      subres <- principalSubresultantsCPP3(
        pows1, coeffs1,
        pows2, coeffs2,
        ipermutation
      )
    } else if(n == 4L) {
      subres <- principalSubresultantsCPP4(
        pows1, coeffs1,
        pows2, coeffs2,
        ipermutation
      )
    } else if(n == 5L) {
      subres <- principalSubresultantsCPP5(
        pows1, coeffs1,
        pows2, coeffs2,
        ipermutation
      )
    } else if(n == 6L) {
      subres <- principalSubresultantsCPP6(
        pows1, coeffs1,
        pows2, coeffs2,
        ipermutation
      )
    } else if(n == 7L) {
      subres <- principalSubresultantsCPP7(
        pows1, coeffs1,
        pows2, coeffs2,
        ipermutation
      )
    } else if(n == 8L) {
      subres <- principalSubresultantsCPP8(
        pows1, coeffs1,
        pows2, coeffs2,
        ipermutation
      )
    } else if(n == 9L) {
      subres <- principalSubresultantsCPP9(
        pows1, coeffs1,
        pows2, coeffs2,
        ipermutation
      )
    }
    if(n <= 3L) {
      showFunc <- showQsprayXYZ()
    } else {
      showFunc <- showQsprayX1X2X3()
    }
    lapply(subres, function(S) {
      p <- qsprayMaker(
        powers = Columns(S[["Powers"]]),
        coeffs = S[["Coeffs"]]
      )
      p <- permuteVariables(p, permutation)
      showQsprayOption(p, "showQspray") <- showFunc
      p
    })
  }
}
