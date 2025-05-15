#' @title Weighted Fisher's Method p-value
#' @description Calculate weighted Fisher's method p-value to summarize a vector of 
#'   p-values based on a chi-squared distribution.
#' 
#' @param p vector of p-values.
#' @param w vector weights.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @export
#' 
weighted.fisher.p <- function(p, w = NULL) {
  if(is.null(w)) w <- rep(1, length(p))
  g <- stats::qgamma(p, shape = w, scale = 2)
  stats::dgamma(sum(g), shape = sum(w), scale = 2)
}