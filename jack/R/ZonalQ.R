#' Evaluation of quaternionic zonal polynomials
#'
#' Evaluates a quaternionic (or symplectic) zonal polynomial.
#'
#' @param x numeric or complex vector or \link[gmp]{bigq} vector
#' @param lambda an integer partition, given as a vector of decreasing
#' integers
#' @param algorithm the algorithm used, either \code{"DK"} (Demmel-Koev)
#' or \code{"naive"}
#'
#' @return A numeric or complex scalar or a \code{bigq} rational number.
#' @export
#'
#' @seealso \code{\link{ZonalQPolR}}
#'
#' @references F. Li, Y. Xue. \emph{Zonal polynomials and hypergeometric
#' functions of quaternion matrix argument}.
#' Comm. Statist. Theory Methods, 38 (8), 1184-1206, 2009
#'
#' @examples lambda <- c(2,2)
#' ZonalQR(c(3,1), lambda)
#' ZonalQR(c(gmp::as.bigq(3),gmp::as.bigq(1)), lambda)
#' ##
#' x <- c(3,1)
#' ZonalQR(x, c(1,1)) + ZonalQR(x, 2) # sum(x)^2
#' ZonalQR(x, 3) + ZonalQR(x, c(2,1)) + ZonalQR(x, c(1,1,1)) # sum(x)^3
ZonalQR <- function(x, lambda, algorithm = "DK"){
  algorithm <- match.arg(algorithm, c("DK", "naive"))
  stopifnot(
    is.vector(x) || is.bigq(x),
    is.numeric(x) || is.complex(x) || is.bigq(x)
  )
  stopifnot(isPartition(lambda))
  lambda <- lambda[lambda != 0]
  if(algorithm == "DK"){
    ZonalQEval(x, lambda)
  }else{
    ZonalQEvalNaive(x, lambda)
  }
}
