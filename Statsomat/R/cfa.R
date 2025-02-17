#' cfa
#' cfa()
#' @export
cfa <- function() {

  cfa_pack <- c("fastDummies", "semTools", "semPlot")

  for (i in 1:length(cfa_pack)){
    requireNamespace(cfa_pack[i], quietly = TRUE)
  }

  appDir <- system.file("shiny-apps", "CFA", package = "Statsomat")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing Statsomat.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}


