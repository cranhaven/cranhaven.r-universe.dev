#' A Shiny Weibull Analysis App.
#'
#' WeibullR.shiny is a Shiny web app for Weibull Analysis from WeibullR.
#' @import ReliaGrowR
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @import WeibullR
#' @import WeibullR.plotly
#' @export
#' @seealso \url{https://paulgovan.github.io/WeibullR.shiny/}
#' @return This function does not return a value.
#' @examples
#' if (interactive()) {
#'   WeibullR.shiny()
#' }
WeibullR.shiny <- function() {
  shiny::runApp(system.file('app', package = 'WeibullR.shiny'))
}
