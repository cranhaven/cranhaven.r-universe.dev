#' @title Division of two polynomials
#'
#' @param qsprayA a \code{qspray} object, the dividend 
#' @param qsprayB a \code{qspray} object, the divisor 
#'
#' @return A list with two \code{qspray} objects, the quotient and the 
#'   remainder.
#' @export
#'
#' @examples
#' library(qspray)
#' x <- qlone(1)
#' y <- qlone(2)
#' z <- qlone(3)
#' B <- x*y^2 + z*x^2 + 1
#' A <- B * (x^2*y^2*z^2 - 3) + x*y
#' divis <- qsprayDivision(A, B)
#' B * divis[["Q"]] + divis[["R"]] == A # should be TRUE
qsprayDivision <- function(qsprayA, qsprayB) {
  if(isQzero(qsprayB)) {
    stop("Division by zero.")
  }
  divis <- qsprayDivisionRcpp(
    qsprayA@powers, qsprayA@coeffs, qsprayB@powers, qsprayB@coeffs
  ) 
  Q <- qspray_from_list(divis[["Q"]])
  R <- qspray_from_list(divis[["R"]])
  list("Q" = Q, "R" = R)
}