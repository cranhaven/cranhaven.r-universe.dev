#' Evaluation of Schur polynomials
#'
#' Evaluates a Schur polynomial.
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
#' @seealso \code{\link{SchurPolR}}
#'
#' @references J. Demmel & P. Koev.
#' \emph{Accurate and efficient evaluation of Schur and Jack functions}.
#' Mathematics of computations, vol. 75, n. 253, 223-229, 2005.
#'
#' @examples x <- c(2,3,4)
#' SchurR(x, c(2,1,1))
#' prod(x) * sum(x)
SchurR <- function(x, lambda, algorithm = "DK"){
  algorithm <- match.arg(algorithm, c("DK", "naive"))
  stopifnot(
    is.vector(x) || is.bigq(x),
    is.numeric(x) || is.complex(x) || is.bigq(x)
  )
  stopifnot(isPartition(lambda))
  lambda <- lambda[lambda != 0]
  if(algorithm == "DK"){
    SchurEval(x, lambda)
  }else{
    SchurEvalNaive(x, lambda)
  }
}

