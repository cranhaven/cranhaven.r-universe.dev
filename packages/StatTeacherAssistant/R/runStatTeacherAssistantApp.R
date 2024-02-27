#' Run the StatTeacherAssistant Shiny application
#'
#' @description Runs the StatTeacherAssistant Shiny application.
#'
#' @return There is no return value.
#'
#' @author
#' Christopher Casement \cr
#' Department of Mathematics \cr
#' Fairfield University \cr
#' \email{casementc@@gmail.com}
#'
#' Laura McSweeney \cr
#' Department of Mathematics \cr
#' Fairfield University
#'
#' @examples
#' ## only run the app in an interactive R session
#' if (interactive()) {runStatTeacherAssistantApp()}
#'
#' @export
runStatTeacherAssistantApp <- function() {

  # find and launch app
  run_app_R_script <- '
    appDir <- system.file("StatTeacherAssistant", package = "StatTeacherAssistant")

    if (appDir == "") {
      stop("Could not find the StatTeacherAssistant directory. Try re-installing
        the `StatTeacherAssistant` package.", call. = FALSE
      )
    }

    shiny::runApp(appDir, launch.browser = TRUE, display.mode = "normal")
  '

  eval(parse(text = run_app_R_script))
}
