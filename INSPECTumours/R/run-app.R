#' Run the Shiny Application
#' @param ... additional options passed to shinyApp()
#' @export
#'
#' @return No return value, called for the shiny app interface
#'
#' @importFrom shiny shinyApp
run_app <- function(...) {
  shinyApp(ui, server, options = list(launch.browser = TRUE), ...)
}
