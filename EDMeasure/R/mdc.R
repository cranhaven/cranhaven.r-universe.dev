#' Martingale Difference Correlation
#'
#' \code{mdc} measures conditional mean dependence of \code{Y} given \code{X},
#' where each contains one variable (univariate) or more variables (multivariate).
#'
#' @param X A vector, matrix or data frame, where rows represent samples, and columns represent variables.
#' @param Y A vector, matrix or data frame, where rows represent samples, and columns represent variables.
#' @param center The approach for centering, including
#' \itemize{
#'   \item \code{U}: U-centering which leads to an unbiased estimator;
#'   \item \code{D}: double-centering which leads to a biased estimator. 
#' }
#'
#' @return \code{mdc} returns the squared martingale difference correlation of \code{Y} given \code{X}.
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
#' # X, Y are 10 x 2 matrices with 10 samples and 2 variables
#' X <- matrix(rnorm(10 * 2), 10, 2)
#' Y <- matrix(rnorm(10 * 2), 10, 2)
#'
#' mdc(X, Y, center = "U")
#' mdc(X, Y, center = "D")

mdc <- function(X, Y, center = "U") {
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  
  n <- nrow(X)
  if (n != nrow(Y)) {
    stop("The dimensions of X and Y do not agree.")
  }

  p <- ncol(X)
  q <- ncol(Y)

  if (center == "U") {
    A <- u.center(X)
    B <- u.center(0.5 * as.matrix(dist(Y))^2)
    mdc <- u.inner(A, B) / sqrt(u.inner(A, A) * u.inner(B, B))
  } else if (center == "D") {
    A <- d.center(X)
    B <- d.center(0.5 * as.matrix(dist(Y))^2)    
    mdc <- d.inner(A, B) / sqrt(d.inner(A, A) * d.inner(B, B))
  } else {
    stop("Invalid center. Read ?mdc for proper syntax.")
  }

  return(mdc)
}




