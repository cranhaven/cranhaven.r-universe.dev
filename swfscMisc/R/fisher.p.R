#' @title Fisher's Method p-value
#' @description Calculate Fisher's method p-value to summarize a vector of 
#'   p-values based on a chi-squared distribution.
#' 
#' @param p vector of p-values.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @export
#' 
fisher.p <- function(p) {
  p <- p[p > 0]
  chi2 <- -2 * sum(log(p))
  1 - stats::pchisq(chi2, 2 * length(p))
}
