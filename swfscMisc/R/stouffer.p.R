#' @title Stouffer's Method p-value
#' @description Calculate Fisher's method p-value to summarize a vector of 
#'   p-values based on a chi-squared distribution.
#' 
#' @param p vector of p-values.
#' @param w vector weights.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @export
#' 
stouffer.p <- function(p, w = NULL) {
  if(is.null(w)) w <- rep(1, length(p))
  z <- stats::qnorm(p)
  denom <- sqrt(sum(w, na.rm = TRUE))
  if(denom == 0) return(NA)
  Z <- sum(z * w, na.rm = TRUE) / denom
  1 - stats::pnorm(Z)
}