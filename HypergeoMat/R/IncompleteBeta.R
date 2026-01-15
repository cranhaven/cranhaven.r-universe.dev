#' Incomplete Beta function of a matrix argument
#'
#' @description Evaluates the incomplete Beta function of a matrix argument.
#'
#' @param m truncation weight of the summation, a positive integer
#' @param a,b real or complex parameters with \code{Re(a)>(p-1)/2} and
#'   \code{Re(b)>(p-1)/2}, where \code{p} is the dimension (the order of the
#'   matrix)
#' @param x either a real positive symmetric matrix or a complex positive
#'   Hermitian matrix "smaller" than the identity matrix (i.e. \code{I-x} is
#'   positive), or a numeric or complex vector, the eigenvalues of the matrix
#'
#' @return A real or a complex number.
#' @importFrom EigenR Eigen_det
#' @export
#'
#' @note The eigenvalues of a real symmetric matrix or a complex Hermitian
#'   matrix are always real numbers, and moreover they are positive under the
#'   constraints on \code{x}.
#'   However we allow to input a numeric or complex vector \code{x}
#'   because the definition of the function makes sense for such a \code{x}.
#'
#' @references A. K. Gupta and D. K. Nagar.
#'   \emph{Matrix variate distributions}. Chapman and Hall, 1999.
#'
#' @examples # for a scalar x, this is the incomplete Beta function:
#' a <- 2; b <- 3
#' x <- 0.75
#' IncBeta(m = 15, a, b, x)
#' gsl::beta_inc(a, b, x)
#' pbeta(x, a, b)
IncBeta <- function(m, a, b, x){
  if(isSquareMatrix(x)){
    stopifnot(!anyNA(x))
    stopifnot(isSymmetricPositive(x))
    p <- nrow(x)
    stopifnot(isSymmetricPositive(diag(p)-x))
  }else if(isNumericOrComplex(x)){
    stopifnot(!anyNA(x))
    stopifnot((p <- length(x)) != 0L)
  }else{
    stop("Invalid `x` argument.")
  }
  stopifnot(
    isPositiveInteger(m),
    isScalar(a),
    Re(a) > (p-1)/2,
    isScalar(b),
    Re(b) > (p-1)/2
  )
  if(is.matrix(x)){
    DET <- Eigen_det(x)
  }else{
    DET <- prod(x)
  }
  DET^a / .mvbeta(a, b, p) / a *
    .hypergeomPFQ(m, c(a, -b+(p+1)/2), a+(p+1)/2, x, alpha = 2)
}
