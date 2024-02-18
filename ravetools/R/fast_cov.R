#' @title Calculate massive covariance matrix in parallel
#'
#' @description Speed up covariance calculation for large matrices. The
#' default behavior is the same as \code{\link{cov}} (\code{'pearson'},
#' no \code{NA} handling).
#'
#' @param x a numeric vector, matrix or data frame; a matrix is highly
#' recommended to maximize the performance
#' @param y NULL (default) or a vector, matrix or data frame with compatible
#' dimensions to x; the default is equivalent to \code{y = x}
#' @param col_x integers indicating the subset indices (columns) of \code{x} to
#' calculate the covariance, or \code{NULL} to include all the columns; default
#' is \code{NULL}
#' @param col_y integers indicating the subset indices (columns) of \code{y} to
#' calculate the covariance, or \code{NULL} to include all the columns; default
#' is \code{NULL}
#' @param df a scalar indicating the degrees of freedom; default is
#' \code{nrow(x)-1}
#'
#' @returns A covariance matrix of \code{x} and \code{y}. Note that there is no
#' \code{NA} handling. Any missing values will lead to \code{NA} in the
#' resulting covariance matrices.
#'
#' @examples
#'
#' # Set ncores = 2 to comply to CRAN policy. Please don't run this line
#' ravetools_threads(n_threads = 2L)
#'
#' x <- matrix(rnorm(400), nrow = 100)
#'
#' # Call `cov(x)` to compare
#' fast_cov(x)
#'
#' # Calculate covariance of subsets
#' fast_cov(x, col_x = 1, col_y = 1:2)
#'
#' if(interactive()){
#'
#' # Speed comparison, better to use multiple cores (4, 8, or more)
#' # to show the differences.
#'
#' ravetools_threads(n_threads = -1)
#' x <- matrix(rnorm(100000), nrow = 1000)
#' microbenchmark::microbenchmark(
#'   fast_cov = {
#'     fast_cov(x, col_x = 1:50, col_y = 51:100)
#'   },
#'   cov = {
#'     cov(x[,1:50], x[,51:100])
#'   },
#'   unit = 'ms', times = 10
#' )
#'
#' }
#'
#'
#' @export
fast_cov <- function(x, y = NULL, col_x = NULL, col_y = NULL, df = NA){
  if(!is.matrix(x)){
    x <- as.matrix(x)
  }
  stopifnot(is.matrix(x) && (is.numeric(x) || is.logical(x)))
  if(is.null(y)){
    y <- x
  } else {
    if(!is.matrix(y)){
      y <- as.matrix(y)
    }
    stopifnot(is.matrix(x) && (is.numeric(x) || is.logical(x)))
  }
  if(!is.null(col_x)){
    col_x <- as.integer(col_x)
  }
  if(!is.null(col_y)){
    col_y <- as.integer(col_y)
  }
  if(!is.finite(df) || df <= 0){
    df <- nrow(x) - 1
  }
  re <- fastcov(x1 = x, x2 = y, col1 = col_x, col2 = col_y, df = df)
  if(inherits(re, "ravetools_error")){
    stop(re)
  }
  re
}
