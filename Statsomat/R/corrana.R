#' corrana
#' corrana()
#' @export
corrana <- function() {

  corrana_pack <- c("MASS", "boot", "nortest", "lmtest", "DescTools")

  for (i in 1:length(corrana_pack)){
    requireNamespace(corrana_pack[i], quietly = TRUE)
  }

  appDir <- system.file("shiny-apps", "CORRANA", package = "Statsomat")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing Statsomat.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}


