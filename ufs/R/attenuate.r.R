#' Attenuate a Pearson's r estimate for unreliability in the measurements
#'
#' @param r The (disattenuated) value of Pearson's r
#' @param reliability1,reliability2 The reliabilities of the two variables
#'
#' @return The attenuated value of Pearson's r
#' @export
#'
#' @examples
#' attenuate.r(.5, .8, .9);
attenuate.r <- function(r, reliability1, reliability2) {
  return(r * sqrt(reliability1*reliability2));
}
