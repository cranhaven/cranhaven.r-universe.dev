#########1#########2#########3#########4#########5#########6#########7#########8
#' Line through a Half-Normal Plot
#'
#' Plot a line through the first and third quantile of a halfnormal line
#'
#' @param x numeric vector
#' @return No return value, called for side effects
#' @examples
#' z=rnorm(100)
#' faraway::halfnorm(z)
#' qqlineHalf(z)
#' @import stats
#' @export
################################################################################
qqlineHalf<-function(x) {
  if (!inherits(x,c("integer","numeric"))) stop("x must be numeric")
  #First and third quantile of the halfnormal
  (z=stats::qnorm(c(.625,.875)))
  #First and third quantile of x
  (y=stats::quantile(abs(x),c(.25,.75)))
  b=(y[2]-y[1])/(z[2]-z[1])
  graphics::abline(y[1]-b*z[1],b)
}
