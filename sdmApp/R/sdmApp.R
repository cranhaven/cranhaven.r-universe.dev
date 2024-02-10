#' starts the graphical user interface developed with shiny.
#'
#' @param maxRequestSize (numeric) number defining the maximum allowed file size (in megabytes) for uploaded files, defaults to 50MB
#' @param debug logical if TRUE, set shiny-debugging options
#' @param theme select style sheet for the interface.
#
#' @param ... arguments (e.g host) that are passed through \code{\link{runApp}} when starting the shiny application
#' @param shiny.server Setting this parameter to TRUE will return the app in the form of an object rather than invoking it. This is useful for deploying sdmApp via shiny-server.
#'
#' @return starts the interactive graphical user interface.
#' @export
#' @import shiny
#' @import raster
#' @import sp
#' @import sf
#' @import grid
#' @import rhandsontable
#' @import haven
#' @import shinyBS
#' @import data.table
#' @import ggplot2
#' @import dismo
#' @import DT
#' @import readxl
#' @import shinyFiles
#' @import shinydashboard
#' @import SSDM
#' @import automap
#' @import blockCV
#' @import CENFA
#' @import rJava
#' @import randomForest
#' @import kernlab
#'
#' @examples
#' if(interactive()){
#' #load the package
#' library(sdmApp)
#' sdmApp()
#' }
sdmApp<-function (maxRequestSize = 50, debug = FALSE, theme = "IHSN",
          ..., shiny.server = FALSE)
{
  if (!shiny.server)
    shiny::runApp(sdmApp(maxRequestSize, debug, theme, ..., shiny.server = TRUE))
  if (!is.numeric(maxRequestSize)) {
    stop("argument 'maxRequestSize' must be numeric!\n")
  }
  if (maxRequestSize < 1) {
    maxRequestSize <- 10
  }
  appDir <- system.file("shiny", "sdmApp", package = "sdmApp")
  #appDir <- "C:/Users/DELLDRAMOMO/Dropbox/Package/sdmApp/shiny/sdmApp"
  if (appDir == "") {
    stop("Could not find directory.",
         call. = FALSE)
  }
  options(shiny.maxRequestSize = ceiling(maxRequestSize) *
            1024^2)
  options(shiny.fullstacktrace = debug)
  options(shiny.trace = debug)
  shiny::shinyOptions(.startdir = getwd())
  shiny::shinyOptions(.appDir = appDir)
  if (!theme %in% c("yeti", "journal", "flatly",
                    "IHSN")) {
    stop("Invalid value for argument 'theme'\n")
  }
  if (theme == "yeti") {
    shiny::shinyOptions(.guitheme = "bootswatch_yeti.css")
    shiny::shinyOptions(.guijsfile = NULL)
  }
  if (theme == "journal") {
    shiny::shinyOptions(.guitheme = "bootswatch_journal.css")
    shiny::shinyOptions(.guijsfile = NULL)
  }
  if (theme == "flatly") {
    shiny::shinyOptions(.guitheme = "bootswatch_flatly.css")
    shiny::shinyOptions(.guijsfile = NULL)
  }
  if (theme == "IHSN") {
    shiny::shinyOptions(.guitheme = "ihsn-root.css")
    #shiny::shinyOptions(.guijsfile = "js/ihsn-style.js")
  }
  source_from_appdir <- function(filename) {
    source(file.path(appDir, filename), local = parent.frame(),
           chdir = TRUE)$value
  }
  shiny::shinyOptions(sdcAppInvoked = TRUE)
  source_from_appdir("global.R")
  shiny::shinyOptions(sdcAppInvoked = NULL)
  shiny::shinyApp(ui = source_from_appdir("ui.R"), server = source_from_appdir("server.R"),
                  options = list(launch.browser = TRUE, ...))
}
