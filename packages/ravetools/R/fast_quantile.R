#' @name fast_quantile
#' @title Compute quantiles
#' @param x numerical-value vector for \code{fast_quantile} and
#' \code{fast_median}, and column-major matrix for \code{fast_mvquantile} and
#' \code{fast_mvmedian}
#' @param prob a probability with value from 0 to 1
#' @param na.rm logical; if true, any \code{NA} are removed from \code{x}
#' before the quantiles are computed
#' @param ... reserved for future use
#' @returns \code{fast_quantile} and \code{fast_median} calculate univariate
#' quantiles (single-value return); \code{fast_mvquantile} and \code{fast_mvmedian}
#' calculate multivariate quantiles (for each column, result lengths equal to
#' the number of columns).
#'
#' @examples
#'
#' fast_quantile(runif(1000), 0.1)
#' fast_median(1:100)
#'
#' x <- matrix(rnorm(100), ncol = 2)
#' fast_mvquantile(x, 0.2)
#' fast_mvmedian(x)
#'
#' # Compare speed for vectors (usually 30% faster)
#' x <- rnorm(10000)
#' microbenchmark::microbenchmark(
#'   fast_median = fast_median(x),
#'   base_median = median(x),
#'   # bioc_median = Biobase::rowMedians(matrix(x, nrow = 1)),
#'   times = 100, unit = "milliseconds"
#' )
#'
#' # Multivariate cases
#' # (5~7x faster than base R)
#' # (3~5x faster than Biobase rowMedians)
#' x <- matrix(rnorm(100000), ncol = 20)
#' microbenchmark::microbenchmark(
#'   fast_median = fast_mvmedian(x),
#'   base_median = apply(x, 2, median),
#'   # bioc_median = Biobase::rowMedians(t(x)),
#'   times = 10, unit = "milliseconds"
#' )
#'
#' @export
fast_quantile <- function(x, prob = 0.5, na.rm = FALSE, ...) {
  if(length(prob) != 1L) {
    stop("`fast_quantile`: `prob` length can only be one")
  }
  .Call(`_ravetools_quickQuantile`, x, prob, isTRUE(as.logical(na.rm)))
}

#' @rdname fast_quantile
#' @export
fast_median <- function(x, na.rm = FALSE, ...) {
  .Call(`_ravetools_quickMedian`, x, isTRUE(as.logical(na.rm)))
}

#' @rdname fast_quantile
#' @export
fast_mvquantile <- function(x, prob = 0.5, na.rm = FALSE, ...) {
  if(length(prob) != 1L) {
    stop("`fast_quantile`: `prob` length can only be one")
  }
  .Call(`_ravetools_columnQuantile`, x, prob, isTRUE(as.logical(na.rm)))
}

#' @rdname fast_quantile
#' @export
fast_mvmedian <- function(x, na.rm = FALSE, ...) {
  .Call(`_ravetools_columnMedian`, x, isTRUE(as.logical(na.rm)))
}
