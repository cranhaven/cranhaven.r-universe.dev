#' Run GLOSSA Shiny App
#'
#' This function launches the GLOSSA Shiny web application.
#'
#' @param request_size_mb Maximum request size for file uploads, in megabytes. Default is 2000 MB.
#' @param launch.browser Logical indicating whether to launch the app in the browser (default is TRUE).
#' @param port Port number for the Shiny app. Uses the port specified by `getOption("shiny.port")` by default.
#'
#' @details The GLOSSA Shiny app provides an interactive interface for users to access GLOSSA functionalities.
#'
#' @return No return value, called to launch the GLOSSA app.
#' @export
#' @examples
#' if(interactive()) {
#' run_glossa()
#' }
run_glossa <- function(request_size_mb = 2000, launch.browser = TRUE, port = getOption("shiny.port")) {
  options(shiny.maxRequestSize = request_size_mb * (1024^2))
  on.exit(rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv))
  return(shiny::runApp(appDir = system.file("app", package = "glossa"), launch.browser = launch.browser, port = port))
}
