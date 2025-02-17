#' edapy
#' edapy()
#' @export
edapy <- function() {

  requireNamespace("reticulate", quietly = TRUE)

  appDir <- system.file("shiny-apps", "EDAPY", package = "Statsomat")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing Statsomat.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}


