#' Run Arena App
#'
#' This function will launch a Shiny App allowing you to analyse Arena results without writing R code.
#' @export
runArenaApp <- function() {

  appDir <- system.file("shiny-examples", "arenaapp", package = "arena2r")

  if (appDir == "") {
    stop("Could not find example directory. Try re-installing arena2r.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
