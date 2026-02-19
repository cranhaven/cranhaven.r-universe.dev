#' @name launchApp
#'
#' @title Launch the MDMAPR app.
#'
#' @description This function runs the MDMAPR Shiny web application. Please update the
#'  dbInstance function first to specify if you are running the application with or
#'  without a database connection. If you are running the application with a database,
#'  then after updating the dbInstance function, run the dbVariables function to input
#'  the variables needed to establish a connection to your MDMAPR MySQL database
#'  instance. Then run launchApp() to launch app.
#'
#' @export launchApp
#'
#' @return shiny application object
#'
#' @usage launchApp()
#'


# wrapper for shiny::shinyApp()
launchApp <- function() {
  shinyApp(ui = shinyAppUI, server = shinyAppServer)
}
