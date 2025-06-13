#' Run the Shiny app.
#' @description Meant to be used as an example showcasing the visualizations.
#' Uses the EX Person instance used throughout this package.
#' @export
#' @importFrom shiny runApp
#' @examples
#' lifelogrApp
lifelogrApp <- function() {
  shiny::runApp(system.file('shinyApp', package = 'lifelogr'))
}
