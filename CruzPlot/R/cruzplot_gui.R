#' Open CruzPlot
#'
#' Open the CruzPlot utility program, an R Shiny application
#'
#' @param launch.browser Logical with default of \code{TRUE};
#'   passed to \code{launch.browser} argument of \code{\link[shiny]{runApp}}
#'
#' @examples
#' if (interactive()) cruzplot_gui(launch.browser = TRUE)
#'
#' @export
cruzplot_gui <- function(launch.browser = TRUE) {
  appDir <- system.file("shiny", package = "CruzPlot")
  if (appDir == "") {
    stop("There was an error opening CruzPlot; try re-installing 'CruzPlot'",
         call. = FALSE)
  }
  runApp(appDir, launch.browser = launch.browser, display.mode = "normal")
}
