#########1#########2#########3#########4#########5#########6#########7#########8
#' Calculate the Population Variance
#'
#' Calculate the variance of a numeric vector if the data constitutes the
#' whole population. Note that missing values are excluded.
#'
#' @param x numeric vector
#' @return The population variance of the entries in \code{x}
#' @examples
#' pop.var(c(1:6,NA,7:10))
#' @export
################################################################################
pop.var<-function(x) {
  if (!inherits(x,c("integer","numeric"))) stop("x must be numeric")
  (sum(x^2,na.rm=TRUE)-length(stats::na.omit(x))*mean(x,na.rm=TRUE)^2)/
    length(stats::na.omit(x))
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Calculate the Population Standard Deviation
#'
#' Calculate the standard deviation of a numeric vector if the data constitutes
#' the whole population. Note that missing values are excluded.
#'
#' @param x numeric vector
#' @return The population standard deviation of the entries in \code{x}
#' @examples
#' pop.sd(c(1:6,NA,7:10))
#' @export
################################################################################
pop.sd<-function(x) {
  if (!inherits(x,c("integer","numeric"))) stop("x must be numeric")
  sqrt(pop.var(x))
}
