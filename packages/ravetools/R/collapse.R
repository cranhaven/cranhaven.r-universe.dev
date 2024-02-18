#' @title Collapse array
#' @param x A numeric multi-mode tensor (array), without \code{NA}
#' @param keep Which dimension to keep
#' @param average collapse to sum or mean
#' @param transform transform on the data before applying collapsing;
#' choices are \code{'asis'} (no change), \code{'10log10'} (used to calculate
#' decibel), \code{'square'} (sum-squared), \code{'sqrt'} (square-root and
#' collapse)
#' @param ... passed to other methods
#'
#' @returns a collapsed array with values to be mean or summation along
#' collapsing dimensions
#'
#'
#' @examples
#'
#' # Set ncores = 2 to comply to CRAN policy. Please don't run this line
#' ravetools_threads(n_threads = 2L)
#'
#' # Example 1
#' x = matrix(1:16, 4)
#'
#' # Keep the first dimension and calculate sums along the rest
#' collapse(x, keep = 1)
#' rowMeans(x)  # Should yield the same result
#'
#' # Example 2
#' x = array(1:120, dim = c(2,3,4,5))
#' result = collapse(x, keep = c(3,2))
#' compare = apply(x, c(3,2), mean)
#' sum(abs(result - compare)) # The same, yield 0 or very small number (1e-10)
#'
#'
#' if(interactive()){
#' ravetools_threads(n_threads = -1)
#'
#' # Example 3 (performance)
#'
#' # Small data, no big difference
#' x = array(rnorm(240), dim = c(4,5,6,2))
#' microbenchmark::microbenchmark(
#'   result = collapse(x, keep = c(3,2)),
#'   compare = apply(x, c(3,2), mean),
#'   times = 1L, check = function(v){
#'     max(abs(range(do.call('-', v)))) < 1e-10
#'   }
#' )
#'
#' # large data big difference
#' x = array(rnorm(prod(300,200,105)), c(300,200,105,1))
#' microbenchmark::microbenchmark(
#'   result = collapse(x, keep = c(3,2)),
#'   compare = apply(x, c(3,2), mean),
#'   times = 1L , check = function(v){
#'     max(abs(range(do.call('-', v)))) < 1e-10
#'   })
#'
#' }
#'
#' @export
collapse <- function(x, keep, ...) {
  UseMethod("collapse")
}

#' @rdname collapse
#' @export
collapse.array <- function(
  x, keep, average = TRUE,
  transform = c("asis", "10log10", "square", "sqrt"), ...){

  transform <- match.arg(transform)

  dim_x <- dim(x)
  keep <- as.integer(keep)
  if(!length(keep)){
    stop("collapse.array: `keep` length must be positive")
  } else if(any(!is.finite(keep) | duplicated(keep))){
    stop("collapse.array: `keep` cannot have duplicated margin indices or NAs")
  } else if (any(keep < 1 | keep > length(dim_x))){
    stop("collapse.array: `keep` must be margin indices (from 1 to max margin)")
  }

  is_complex <- is.complex(x)
  mode_re <- "double"
  if(is_complex && transform == "asis"){
    mode_re <- "complex"
  }

  dim_keep <- dim_x[keep]
  if(!length(x)){
    if(average){
      v <- NaN
    } else {
      v <- 0
    }
    storage.mode(v) <- mode_re
    if(length(dim_keep) >= 2){
      return(array(v, dim_keep))
    } else {
      return(rep(v, dim_keep[[1]]))
    }

  }

  method <- which(c("asis", "10log10", "square", "sqrt") == transform)
  average <- as.integer(as.logical(average))

  if(is_complex){
    re <- collapser_cplx(x = x, keep = keep, method = method, average = average)
    if(transform != "asis"){
      re <- Re(re)
    }
  } else {
    re <- collapser_real(x = x, keep = keep, method = method, average = average)
  }

  if(inherits(re, "ravetools_error")){
    stop(re)
  }

  return(re)

}

