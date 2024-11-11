#' Martingale Difference Divergence
#'
#' \code{mdd} measures conditional mean dependence of \code{Y} given \code{X},
#' where each contains one variable (univariate) or more variables (multivariate).
#'
#' @param X A vector, matrix or data frame, where rows represent samples, and columns represent variables.
#' @param Y A vector, matrix or data frame, where rows represent samples, and columns represent variables.
#' @param compute The method for computation, including
#' \itemize{
#'   \item \code{C}: computation implemented in C code;
#'   \item \code{R}: computation implemented in R code.
#' }
#' @param center The approach for centering, including
#' \itemize{
#'   \item \code{U}: U-centering which leads to an unbiased estimator;
#'   \item \code{D}: double-centering which leads to a biased estimator.
#' }
#'
#' @return \code{mdd} returns the squared martingale difference divergence of \code{Y} given \code{X}.
#'
#' @references Shao, X., and Zhang, J. (2014).
#'   Martingale difference correlation and its use in high-dimensional variable screening.
#'   Journal of the American Statistical Association, 109(507), 1302-1318.
#'   \url{http://dx.doi.org/10.1080/01621459.2014.887012}.
#' @references Park, T., Shao, X., and Yao, S. (2015).
#'   Partial martingale difference correlation.
#'   Electronic Journal of Statistics, 9(1), 1492-1517.
#'   \url{http://dx.doi.org/10.1214/15-EJS1047}.
#'
#' @importFrom stats dist
#'
#' @include cmdm_functions.R
#'
#' @export
#'
#' @examples
#' # X, Y are vectors with 10 samples and 1 variable
#' X <- rnorm(10)
#' Y <- rnorm(10)
#'
#' mdd(X, Y, compute = "C")
#' mdd(X, Y, compute = "R")
#'
#' # X, Y are 10 x 2 matrices with 10 samples and 2 variables
#' X <- matrix(rnorm(10 * 2), 10, 2)
#' Y <- matrix(rnorm(10 * 2), 10, 2)
#'
#' mdd(X, Y, center = "U")
#' mdd(X, Y, center = "D")

mdd <- function(X, Y, compute = "C", center = "U") {
  X <- as.matrix(X)
  Y <- as.matrix(Y)

  n <- nrow(X)
  if (n != nrow(Y)) {
    stop("The dimensions of X and Y do not agree.")
  }

  p <- ncol(X)
  q <- ncol(Y)

  if (compute == "C") {
    X <- as.vector(X)
    Y <- as.vector(Y)

    if (center == "U") {
      mdd <- .C("MDD_UCenter",
                N = as.integer(n),
                P = as.integer(p),
                Q = as.integer(q),
                X = as.double(X),
                Y = as.double(Y),
                V = as.double(numeric(1)),
                PACKAGE = "EDMeasure")$V
    } else if (center == "D") {
      mdd <- .C("MDD_DCenter",
                N = as.integer(n),
                P = as.integer(p),
                Q = as.integer(q),
                X = as.double(X),
                Y = as.double(Y),
                V = as.double(numeric(1)),
                PACKAGE = "EDMeasure")$V
    } else {
      stop("Invalid center. Read ?mdd for proper syntax.")
    }
  } else if (compute == "R") {
    if (center == "U") {
      A <- u.center(X)
      B <- u.center(0.5 * as.matrix(dist(Y))^2)
      mdd <- u.inner(A, B)
    } else if (center == "D") {
      A <- d.center(X)
      B <- d.center(0.5 * as.matrix(dist(Y))^2)
      mdd <- d.inner(A, B)
    } else {
      stop("Invalid center. Read ?mdd for proper syntax.")
    }
  } else {
    stop("Invalid compute. Read ?mdd for proper syntax.")
  }

  return(mdd)
}




