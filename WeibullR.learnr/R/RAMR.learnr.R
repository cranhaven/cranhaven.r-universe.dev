#' Reliability, Availability, and Maintainability
#'
#' RAMR.learnr is an interactive introduction to RAM analysis.
#' @import learnr
#' @import WeibullR
#' @export
#' @seealso \url{https://paulgovan.github.io/WeibullR.learnr/}
#' @return This function does not return a value.
#' @examples
#' if (interactive()) {
#'   RAMR.learnr()
#' }
RAMR.learnr <- function() {
  learnr::run_tutorial('RAMRlearnr', package = 'WeibullR.learnr')
}
