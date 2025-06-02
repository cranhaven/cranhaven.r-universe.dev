#' Title dadx
#'
#' @param x  denominator variable for calculating da
#' @param a  numerator variable for calculating dt
#' @param d.step step of differentiation
#'
#' @description calculates the ratio of two differential according to the value of d.step
#' @return ratio of two differential of the tw0 input variables
#' @export
#'
#' @examples npoints=100
#' seed=42
#' x1=round(runif(npoints,0,1), 2)
#' seed=1234
#' x2=round(runif(npoints,0,1), 2)
#' xdiff <- dadx(x1,x2)

dadx <- function(x, a, d.step=2) {
  dadx <- 0
  dadx <- diff(a, d.step) / diff(x, d.step)
  dadx[dadx < 0] <- 0
  dadx <- c(rep(0, d.step), dadx)
  return("dadx" = dadx)
}
