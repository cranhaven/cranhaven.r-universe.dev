#' An Interactive Introduction to Reliability Testing
#'
#' TestR.learnr is an interactive introduction to Reliability Testing.
#' @import learnr ReliaGrowR WeibullR.ALT
#' @export
#' @seealso \url{https://paulgovan.github.io/WeibullR.learnr/}
#' @return This function does not return a value.
#' @examples
#' if (interactive()) {
#'   TestR.learnr()
#' }
TestR.learnr <- function() {
  learnr::run_tutorial('TestRlearnr', package = 'WeibullR.learnr')
}
