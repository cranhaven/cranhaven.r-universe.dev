#' Run the shiny app from the wildviz package
#' @return No return value, called for side effects
#' @description Runs the wildviz shiny app for data visualization and exploration. Runs on the master, aqi, wildfires, and climate datasets provided within the wildviz package, which are specific to California and whose dates range from 2011 through 2015.
#' @export
#' @importFrom shiny runApp
wildvizApp <- function() {
  shiny::runApp(system.file('shinyApp', package = 'wildviz'))
}
