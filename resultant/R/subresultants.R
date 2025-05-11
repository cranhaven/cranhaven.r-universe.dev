#' @title Subresultants of two polynomials
#' @description Subresultants of two polynomials with rational coefficients.
#'
#' @param qspray1,qspray2 two \code{qspray} polynomials having at most nine
#'   variables
#' @param var integer indicating with respect to which variable the
#'   subresultants will be computed (e.g. \code{1} for \code{x} and \code{2}
#'   for \code{y})
#'
#' @return A list of \code{qspray} polynomials.
#' @export
#' @importFrom qspray qsprayMaker numberOfVariables showQsprayXYZ showQsprayX1X2X3 showQsprayOption<-
#' @importFrom gmp as.bigq
#'
#' @examples
#' library(resultant)
#' x <- qlone(1)
#' y <- qlone(2)
#' p <- x^2 * y * (y^2 - 5*x + 6)
#' q <- x^2 * y * (3*y + 2)
#' subresultants(p, q, var = 1)
#' subresultants(p, q, var = 2)
subresultants <- function(qspray1, qspray2, var = 1) {
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
  var <- as.integer(var) - 1L
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
    subres <- subresultantsCPP1(
      rbind(pows1), coeffs1,
      rbind(pows2), coeffs2,
      var
    )
  } else {
    if(n == 2L) {
      subres <- subresultantsCPP2(
        pows1, coeffs1,
        pows2, coeffs2,
        var
      )
    } else if(n == 3L) {
      subres <- subresultantsCPP3(
        pows1, coeffs1,
        pows2, coeffs2,
        var
      )
    } else if(n == 4L) {
      subres <- subresultantsCPP4(
        pows1, coeffs1,
        pows2, coeffs2,
        var
      )
    } else if(n == 5L) {
      subres <- subresultantsCPP5(
        pows1, coeffs1,
        pows2, coeffs2,
        var
      )
    } else if(n == 6L) {
      subres <- subresultantsCPP6(
        pows1, coeffs1,
        pows2, coeffs2,
        var
      )
    } else if(n == 7L) {
      subres <- subresultantsCPP7(
        pows1, coeffs1,
        pows2, coeffs2,
        var
      )
    } else if(n == 8L) {
      subres <- subresultantsCPP8(
        pows1, coeffs1,
        pows2, coeffs2,
        var
      )
    } else if(n == 9L) {
      subres <- subresultantsCPP9(
        pows1, coeffs1,
        pows2, coeffs2,
        var
      )
    }
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
    showQsprayOption(p, "showQspray") <- showFunc
    p
  })
}
