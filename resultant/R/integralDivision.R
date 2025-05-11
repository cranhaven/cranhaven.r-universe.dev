#' @title Integral division of two polynomials
#' @description Integral division (division without remainder) of two
#'   polynomials with rational coefficients.
#'
#' @param qspray1,qspray2 two \code{qspray} polynomials having at most nine
#'   variables
#' @param check Boolean, whether to check that \code{qspray2} divides
#'   \code{qspray1}
#'
#' @return If \code{check=TRUE}, this returns \code{NULL} if \code{qspray2}
#'   does not divide \code{qspray1}, otherwise this returns a \code{qspray}
#'   polynomial, the quotient of \code{qspray1} by \code{qspray2}.
#'   If \code{check=FALSE}, this always returns a \code{qspray} polynomial,
#'   which is the quotient of \code{qspray1} by \code{qspray2} if
#'   \code{qspray2} divides \code{qspray1}, otherwise it is an undefined
#'   polynomial. So you can use \code{check=FALSE} only when you are sure that
#'   \code{qspray2} divides \code{qspray1}.
#'
#' @export
#' @importFrom qspray qsprayMaker numberOfVariables
#'
#' @seealso \code{\link{univariateDivision}},
#'   \code{\link[qspray]{qsprayDivision}}.
#'
#' @examples
#' library(resultant)
#' x <- qlone(1)
#' y <- qlone(2)
#' q <- x^2 + 2*x*y + 1
#' qspray1 <- q * (x^4 + y^2 + 2)
#' qspray2 <- x^4 + y^2 + 2
#' integralDivision(qspray1, qspray2) == q # should be TRUE
integralDivision <- function(qspray1, qspray2, check = TRUE) {
  n1 <- numberOfVariables(qspray1)
  n2 <- numberOfVariables(qspray2)
  n <- max(1L, n1, n2)
  if(n >= 10L) {
    stop(
      "Only polynomials with at most nine variables are allowed."
    )
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
    Q <- integralDivisionCPP1(
      rbind(pows1), coeffs1,
      rbind(pows2), coeffs2,
      check
    )
  } else if(n == 2L) {
    Q <- integralDivisionCPP2(
      pows1, coeffs1,
      pows2, coeffs2,
      check
    )
  } else if(n == 3L) {
    Q <- integralDivisionCPP3(
      pows1, coeffs1,
      pows2, coeffs2,
      check
    )
  } else if(n == 4L) {
    Q <- integralDivisionCPP4(
      pows1, coeffs1,
      pows2, coeffs2,
      check
    )
  } else if(n == 5L) {
    Q <- integralDivisionCPP5(
      pows1, coeffs1,
      pows2, coeffs2,
      check
    )
  } else if(n == 6L) {
    Q <- integralDivisionCPP6(
      pows1, coeffs1,
      pows2, coeffs2,
      check
    )
  } else if(n == 7L) {
    Q <- integralDivisionCPP7(
      pows1, coeffs1,
      pows2, coeffs2,
      check
    )
  } else if(n == 8L) {
    Q <- integralDivisionCPP8(
      pows1, coeffs1,
      pows2, coeffs2,
      check
    )
  } else if(n == 9L) {
    Q <- integralDivisionCPP9(
      pows1, coeffs1,
      pows2, coeffs2,
      check
    )
  }
  if(length(Q) == 0L) {
    NULL
  } else {
    qsprayMaker(
      powers = Columns(Q[["Powers"]]),
      coeffs = Q[["Coeffs"]]
    )
  }
}
