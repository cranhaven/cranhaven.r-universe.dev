#' Martingale Difference Divergence Matrix
#'
#' \code{mddm} extends martingale difference divergence from a scalar to a matrix.
#' It encodes the linear combinations of all univariate components in \code{Y}
#' that are conditionally mean independent of \code{X}.
#' Only the double-centering approach is applied.
#'
#' @param X A vector, matrix or data frame, where rows represent samples, and columns represent variables.
#' @param Y A vector, matrix or data frame, where rows represent samples, and columns represent variables.
#' @param compute The method for computation, including
#' \itemize{
#'   \item \code{C}: computation implemented in C code;
#'   \item \code{R}: computation implemented in R code.
#' }
#'
#' @return \code{mddm} returns the martingale difference divergence matrix of \code{Y} given \code{X}.
#'
#' @references Lee, C. E., and Shao, X. (2017).
#'   Martingale Difference Divergence Matrix and Its Application to Dimension Reduction for
#'   Stationary Multivariate Time Series.
#'   Journal of the American Statistical Association, 1-14.
#'   \url{http://dx.doi.org/10.1080/01621459.2016.1240083}.
#'
#' @export
#'
#' @examples
#' # X, Y are vectors with 10 samples and 1 variable
#' X <- rnorm(10)
#' Y <- rnorm(10)
#'
#' mddm(X, Y, compute = "C")
#' mddm(X, Y, compute = "R")
#'
#' # X, Y are 10 x 2 matrices with 10 samples and 2 variables
#' X <- matrix(rnorm(10 * 2), 10, 2)
#' Y <- matrix(rnorm(10 * 2), 10, 2)
#'
#' mddm(X, Y, compute = "C")
#' mddm(X, Y, compute = "R")

mddm <- function(X, Y, compute = "C") {
  X <- as.matrix(X)
  Y <- as.matrix(Y)

  n <- nrow(X)
  if (n != nrow(Y)) {
    stop("The dimensions of X and Y do not agree.")
  }

  p <- ncol(X)
  q <- ncol(Y)

  if (compute == "C") {
    X <- as.vector(t(X))
    Y <- as.vector(t(Y))

    mddm <- .C("MDDM",
               N = as.integer(n),
               P = as.integer(p),
               Q = as.integer(q),
               X = as.double(X),
               Y = as.double(Y),
               M = as.double(numeric(q * q)),
               PACKAGE = "EDMeasure")$M
    dim(mddm) <- c(q, q)
  } else if (compute == "R") {
    mddm <- matrix(0, q, q)

    Y_bar <- apply(Y, 2, mean)
    # Y <- Y - matrix(replicate(n, Y_bar), nrow = n, ncol = q, byrow = TRUE)
    Y <- Y - rep(1, n) %*% t(Y_bar)

    for (i in 1:n) {
      if (p == 1) {
        X_dist <- abs(X[i] - X)
      } else {
        X_dist <- sqrt(apply((t(X[i, ] - t(X)))^2, 1, sum))
      }

      mddm <- mddm + Y[i, ] %*% (t(X_dist) %*% Y)
    }

    mddm <- -mddm / (n^2)
  } else {
    stop("Invalid compute. Read ?mddm for proper syntax.")
  }

  return(mddm)
}

