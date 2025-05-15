#' @title Harmonic Mean
#' @description Calculate the harmonic mean of a set of numbers.
#' 
#' @param x a numeric vector.
#' @param w an optional numerical vector of weights the same length as \code{x}.
#' @param na.rm a logical value indicating whether NA values should be stripped 
#'   before the computation proceeds.
#'   
#' @note If zeroes are present in \code{x}, function will return approximation
#'   with a warning. In this case, weights will not be used.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'  
#' @examples
#' x <- rlnorm(100)
#' mean(x)
#' median(x)
#' harmonic.mean(x)
#' 
#' @export
#' 
harmonic.mean <- function(x, w = NULL, na.rm = FALSE) {
  if(is.null(w)) w <- rep(1, length(x))
  if(length(w) != length(x)) stop("length of 'x' and 'w' must be the same")
  if(na.rm) {
    i <- !is.na(x)
    if(!any(i)) return(NA)
    x <- x[i]
    w <- w[i]
  }
  
  hm <- if(all(x > 0)) {
    sum(w) / sum(w / x)
  } else {
    warning("some values are <= 0, using approximation")
    inv.mean.x <- 1 / mean(x)
    var.x <- stats::var(x)
    1 / (inv.mean.x + var.x * inv.mean.x ^ 3)
  }
  ifelse(is.nan(hm), NA, hm)
}
