#' Convert Cohen's d to U3
#'
#' This function simply returns the result of [pnorm()] for
#' Cohen's d.
#'
#' @param d Cohen's d.
#'
#' @return An unnames numeric vector with the U3 values.
#' @export
#'
#' @examples convert.d.to.U3(.5);
convert.d.to.U3 <- function(d) {
  return(stats::pnorm(d));
}
