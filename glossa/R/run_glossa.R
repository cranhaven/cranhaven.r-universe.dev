#' Run GLOSSA Shiny App
#'
#' This function launches the GLOSSA Shiny web application.
#'
#' @param request_size_mb Maximum request size for file uploads, in megabytes. Default is 2000 MB.
#' @param launch.browser Logical indicating whether to launch the app in the browser (default is TRUE).
#' @param port Port number for the Shiny app. Uses the port specified by `getOption("shiny.port")` by default.
#' @param clear_global_env Logical. If TRUE, clears the global environment after the app exits.
#'
#' @details The GLOSSA Shiny app provides an interactive interface for users to access GLOSSA functionalities.
#'
#' @note Use `clear_global_env = TRUE` cautiously, as it removes all objects from your R environment after the app exits.
#'
#' @return No return value, called to launch the GLOSSA app.
#' @export
#' @examples
#' if(interactive()) {
#' run_glossa()
#' run_glossa(clear_global_env = TRUE)  # clears all global objects
#' }
run_glossa <- function(request_size_mb = 2000, launch.browser = TRUE, port = getOption("shiny.port"), clear_global_env = FALSE) {
  options(shiny.maxRequestSize = request_size_mb * (1024^2), glossa.clear_env_on_exit = clear_global_env)
  if (isTRUE(clear_global_env)) on.exit(rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv))
  return(shiny::runApp(appDir = system.file("app", package = "glossa"), launch.browser = launch.browser, port = port))
}
