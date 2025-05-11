#' @title Division of univariate polynomials
#' @description Division with remainder of univariate polynomials with rational
#'   coefficients.
#'
#' @param qspray1,qspray2 two univariate \code{qspray} polynomials
#'
#' @return A list of two univariate \code{qspray} polynomials, the quotient
#'   of the division in the field \code{Q} of the list, and the remainder in
#'   the field \code{R}.
#' @export
#' @importFrom qspray isUnivariate isQzero
#'
#' @seealso \code{\link{integralDivision}},
#'   \code{\link[qspray]{qsprayDivision}}.
#'
#' @examples
#' library(resultant)
#' x <- qlone(1)
#' qspray1 <- 2*x^4 + x^3 - 3*x^2 - x + 1
#' qspray2 <- x^2 - 5*x + 10
#' division <- univariateDivision(qspray1, qspray2)
#' Q <- division[["Q"]]; R <- division[["R"]]
#' qspray1 == Q*qspray2 + R # should be TRUE
univariateDivision <- function(qspray1, qspray2) {
  if(isUnivariate(qspray1) && isUnivariate(qspray2)) {
    if(isQzero(qspray2)) {
      stop("Division by zero.")
    }
    coeffs1 <- qspray1@coeffs
    coeffs2 <- qspray2@coeffs
    pows1 <- vapply(qspray1@powers, function(pwrs) {
      out <- integer(1L)
      out[seq_along(pwrs)] <- pwrs
      out
    }, integer(1L))
    pows2 <- vapply(qspray2@powers, function(pwrs) {
      out <- integer(1L)
      out[seq_along(pwrs)] <- pwrs
      out
    }, integer(1L))
    QR <- divModCPP1(pows1, coeffs1, pows2, coeffs2)
    Q <- QR[["Q"]]
    R <- QR[["R"]]
    list(
      "Q" = qsprayMaker(
        powers = Columns(Q[["Powers"]]),
        coeffs = Q[["Coeffs"]]
      ),
      "R" = qsprayMaker(
        powers = Columns(R[["Powers"]]),
        coeffs = R[["Coeffs"]]
      )
    )
  } else {
    stop("The polynomials are not univariate.")
  }
}
