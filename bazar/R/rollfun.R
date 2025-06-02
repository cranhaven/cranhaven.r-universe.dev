#' @title 
#' Moving windows with custom function
#' 
#' @description 
#' Windowed / rolling operations on a vector, with a custom function \code{fun} 
#' provided as input. 
#' 
#' @param x
#' A vector. 
#' 
#' @param k
#' integer. Width of moving window; must be an integer between one and 
#' \code{length(x)}. 
#' 
#' @param fun
#' character or function. The function to be applied on moving subvectors of 
#' \code{x}. 
#' 
#' @param ...
#' Additional arguments to be passed to \code{fun}. 
#' 
#' @param .idx
#' integer. A vector of indices that can be precalculated with the function 
#' \code{make_idx}. 
#' 
#' @param n
#' integer. Length of the input vector \code{x}. 
#' 
#' @export
#'
#' @seealso 
#' Functions \code{roll_mean} and others in package \pkg{RcppRoll} for a more 
#' efficient implementation of \code{rollfun} to specific values of \code{fun}. 
#' 
#' Similarly, see functions \code{rollmean} and others in package \pkg{zoo} and 
#' functions \code{runmean} and others in package \pkg{caTools}. 
#' 
#' @examples
#' set.seed(1)
#' x <- sample(1:10)
#' rollfun(x, k = 3)
#' rollfun(x, k = 3, fun = max)
#'
rollfun <-
function(x,
         k,
         fun = "mean",
         ...,
         .idx = NULL)
{
  nx <- length(x)
  if (nx==0L) return(x)

  fun <- as.fun(fun)
  x <- as.matrix(x)
  if (is.null(.idx)) {
    .idx <- make_idx(k, nx)
  }
  apply(.idx, 1L, function(y) fun(x[y[1L]:y[2L],], ...))
}


#' @export
#' @rdname rollfun 
#' 
make_idx <- 
function(k, 
         n)
{
  stopifnot(k >= 1L)
  y1 <- pmax(1L, (2L-k):(n - k + 1L))
  y2 <- pmin(y1+k, 1:n)
  cbind(y1, y2)
}
