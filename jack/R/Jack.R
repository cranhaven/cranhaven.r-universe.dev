#' Evaluation of Jack polynomials
#'
#' Evaluates a Jack polynomial.
#'
#' @param x numeric or complex vector or \code{\link[gmp]{bigq}} vector
#' @param lambda an integer partition, given as a vector of decreasing
#' integers
#' @param alpha ordinary number or \code{bigq} rational number
#' @param algorithm the algorithm used, either \code{"DK"} (Demmel-Koev)
#' or \code{"naive"}
#'
#' @return A numeric or complex scalar or a \code{bigq} rational number.
#' @export
#' @importFrom gmp factorialZ is.bigq as.bigq
#'
#' @seealso \code{\link{JackPolR}}
#'
#' @references \itemize{
#' \item I.G. Macdonald.
#' \emph{Symmetric Functions and Hall Polynomials}.
#' Oxford Mathematical Monographs.
#' The Clarendon Press Oxford University Press,
#' New York, second edition, 1995.
#' \item J. Demmel & P. Koev.
#' \emph{Accurate and efficient evaluation of Schur and Jack functions}.
#' Mathematics of computations, vol. 75, n. 253, 223-229, 2005.
#' \item \emph{Jack polynomials}.
#' \url{https://www.symmetricfunctions.com/jack.htm}
#' }
#'
#' @examples lambda <- c(2,1,1)
#' JackR(c(1/2, 2/3, 1), lambda, alpha = 3)
#' # exact value:
#' JackR(c(gmp::as.bigq(1,2), gmp::as.bigq(2,3), gmp::as.bigq(1)), lambda,
#'      alpha = gmp::as.bigq(3))
JackR <- function(x, lambda, alpha, algorithm = "DK"){
  stopifnot(
    is.vector(x) || is.bigq(x),
    is.numeric(x) || is.complex(x) || is.bigq(x),
    is.numeric(alpha) || is.bigq(alpha),
    length(alpha) == 1L
  )
  stopifnot(isPartition(lambda))
  lambda <- lambda[lambda != 0]
  if(alpha == 0){
    gmp <- is.bigq(x)
    if(length(lambda) == 0L){
      if(gmp){
        return(as.bigq(1L))
      }else{
        return(1)
      }
    }
    lambdaPrime <- dualPartition(lambda)
    if(gmp){
      f <- as.bigq(prod(factorialZ(lambdaPrime)))
    }else{
      f <- prod(factorial(lambdaPrime))
    }
    return(f * ESF(x, lambdaPrime))
  }
  algorithm <- match.arg(algorithm, c("DK", "naive"))
  if(algorithm == "DK"){
    JackEval(x, lambda, alpha)
  }else{
    JackEvalNaive(x, lambda, alpha)
  }
}

