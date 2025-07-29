#' Allows running \code{shiny} application with
#' \code{nprcgenekeepr::runGeneKeepR()}
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return Returns the error condition of the Shiny application when it
#' terminates.
#'
#' @importFrom shiny runApp
#' @export
#' @examples
#' if (interactive()) {
#'   library(nprcgenekeepr)
#'   runGeneKeepR()
#' }
runGeneKeepR <- function() {
  appDir <- system.file("application", package = "nprcgenekeepr")
  if (appDir == "") {
    stop(
        "Could not find application directory. ",
        "Try re-installing `nprcgenekeepr`.",
      call. = FALSE
    )
  }

  shiny::runApp(appDir, display.mode = "normal", port = 6012L)
}
