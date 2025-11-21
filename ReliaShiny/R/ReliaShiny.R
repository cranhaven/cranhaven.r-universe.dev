#' A Shiny Reliability Analysis App.
#'
#' This function launches a Shiny application for reliability analysis.
#'
#' @import ReliaGrowR
#' @import ReliaPlotR
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @import WeibullR
#' @export
#' @seealso \url{https://paulgovan.github.io/ReliaShiny/}
#' @return This function does not return a value.
#' @examples
#' if (interactive()) {
#'   ReliaShiny()
#' }
ReliaShiny <- function() {
  shiny::runApp(system.file("app", package = "ReliaShiny"))
}
