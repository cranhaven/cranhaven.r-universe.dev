#' @title Random 'ratioOfQsprays'
#' @description Generates a random \code{ratioOfQsprays} object.
#'
#' @param allow.zero Boolean, whether to allow to get a null
#'   \code{ratioOfQsprays}
#'
#' @return A \code{ratioOfQsprays} object.
#' @export
#' @importFrom qspray rQspray isQzero
rRatioOfQsprays <- function(allow.zero = TRUE) {
  numerator   <- rQspray()
  if(!allow.zero) {
    while(isQzero(numerator)) {
      numerator <- rQspray()
    }
  }
  denominator <- rQspray()
  while(isQzero(denominator)) {
    denominator <- rQspray()
  }
  numerator / denominator
}
