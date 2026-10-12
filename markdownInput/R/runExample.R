
#' Run a Shiny application presenting the "markdownInput" package.
#' @export
runExample <- function() {
  appDir <- system.file("ShinyExample", package = "markdownInput")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `markdownInput`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "showcase")
}
