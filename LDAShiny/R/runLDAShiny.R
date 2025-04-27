#' Shiny UI for LDAShiny package
#'@import shiny
#'@import shinydashboard
#' @param launch.browser If true, the system's default web browser will be launched automatically
#' after the app is started. Defaults to true in interactive sessions only. This value of
#' this parameter can also be a function to call with the application's URL.
#' @param port is the TCP port that the application should listen on. If the port is not specified,
#' and the shiny.port option is set (with options(shiny.port = XX)), then that port will be used.
#' Otherwise, use a random port.
#' @param host The IPv4 address that the application should listen on.
#' Defaults to the shiny.host option, if set, or "127.0.0.1" if not.
#' @return No return value,just start the GUI
#' @examples
#'if (interactive()) {
#'runLDAShiny()
#'}
#'

#' @export
runLDAShiny <- function(host = "127.0.0.1", port = NULL,launch.browser = TRUE) {
  appDir <- system.file("shinyLDA", package = "LDAShiny")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `LDAShiny`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal",launch.browser = launch.browser,port = port,host = getOption("shiny.host", host))
}
