#' edar
#' edar()
#' @export
edar <- function() {

  edar_pack <- c("psych", "Hmisc", "PerformanceAnalytics")

  for (i in 1:length(edar_pack)){
    requireNamespace(edar_pack[i], quietly = TRUE)
  }

  appDir <- system.file("shiny-apps", "EDAR", package = "Statsomat")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing Statsomat.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}


