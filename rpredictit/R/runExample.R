# common way to print error messages
errMsg <- function(x) {
  stop(sprintf("rpredictit: %s", x), call. = FALSE)
}

#' Run rpredictit examples
#'
#' Launch a \code{rpredictit} example Shiny app that shows how to easily use
#' \code{rpredictit} in an app.\cr\cr Run without any arguments to see a list of
#' available example apps.
#'
#' @param example The app to launch
#' @return None. Runs a demo Shiny application. This function normally does not
#'   return; interrupt R to stop the application.
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   # List all available example apps
#'   runExample()
#'
#'   runExample("demo")
#' }
#' @export
runExample <- function(example) {

  validExamples <-
    paste0(
      'Valid examples are: "',
      paste(list.files(system.file("examples", package = "rpredictit")),
            collapse = '", "'),
      '"')

  if (missing(example) || !nzchar(example)) {
    message(
      'Please run `runExample()` with a valid example app as an argument.\n',
      validExamples)
    return(invisible(NULL))
  }

  appDir <- system.file("examples", example,
                        package = "rpredictit")
  if (appDir == "") {
    errMsg(sprintf("could not find example app `%s`\n%s",
                   example, validExamples))
  }

  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
