#' A Shiny-based GUI for the rs_IDs function
#'
#' Launches a (possibly buggy) Shiny app that acts as a graphical user interface for the \code{\link{rs_IDs}} function. It's a bit hacky, so its performance is not guaranteed.
#'
#' @author Andrew Burchill, \email{andrew.burchill@asu.edu}
#' @references Burchill, A. T., & Pavlic, T. P. (2019). Dude, where's my mark? Creating robust animal identification schemes informed by communication theory. \emph{Animal Behaviour}, 154, 203-208. \href{https://doi.org/10.1016/j.anbehav.2019.05.013}{doi:10.1016/j.anbehav.2019.05.013}
#' @seealso \code{\link{rs_IDs}} and the vignette \href{../doc/loosebirdtag.html}{\code{loosebirdtag}}.
#' @examples
#' \dontrun{
#' exampleGUI()  #yeah, just run it.
#' }
#' @export
#' @importFrom shiny runApp
#'
exampleGUI <- function() {
  appDir <- system.file("gui-example", package = "rabi")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `rabi`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
