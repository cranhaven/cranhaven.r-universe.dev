#' @title Geometric Mean
#' @description Calculates the geometric mean of a vector.
#' 
#' @param x a numeric vector.
#' @param w an optional numerical vector of weights the same length as \code{x}.
#' @param na.rm a logical value indicating whether NA values should be stripped 
#'   before the computation proceeds.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' x <- rlnorm(100)
#' mean(x)
#' median(x)
#' geometric.mean(x)
#' 
#' @export
#' 
geometric.mean <- function(x, w = NULL, na.rm = FALSE) {
  if(length(x) == 0) return(NA)
  if(is.null(w)) w <- rep(1, length(x))
  if(length(w) != length(x)) stop("length of 'x' and 'w' must be the same")
  if(na.rm) {
    i <- !is.na(x)
    if(!any(i)) return(NA)
    x <- x[i]
    w <- w[i]
  }
  exp(sum(w * log(x)) / sum(w))
}