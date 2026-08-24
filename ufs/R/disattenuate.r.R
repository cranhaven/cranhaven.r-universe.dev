#' Disattentuate a Pearson's r estimate for unreliability
#'
#' @param r The (attenuated) value of Pearson's r
#' @param reliability1,reliability2 The reliabilities of the two variables
#'
#' @return The disattenuated value of Pearson's r
#' @export
#'
#' @examples
#' disattenuate.r(.5, .8, .9);
disattenuate.r <- function(r, reliability1, reliability2) {
  return(r / sqrt(reliability1*reliability2));
}
