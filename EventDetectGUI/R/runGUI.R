#' runGUI
#'
#' Run the starting command of the EventDetectGUI. Opens the graphical shiny application through which the user
#' can acess the algorithms and visualizations.
#'
#' @usage
#' runGUI()
#'
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyjs enable
#' @importFrom shinyjs disable
#' @import XML
#' @rawNamespace import(ggplot2, except = last_plot)
#' @import plotly
#' @import shinyBS
#' @importFrom tools Rd_db
#' @importFrom stats sd
#' @importFrom utils read.csv
#' @import EventDetectR
#' @importFrom utils write.csv
#'
#' @export
#' @return None
runGUI <- function() {
    appDir <- system.file(package = "EventDetectGUI")
    if (appDir == "") {
        stop("Could not find app directory. Try re-installing `EventDetectGUI`.", call. = FALSE)
    }
    shiny::runApp(appDir, display.mode = "normal")
}
