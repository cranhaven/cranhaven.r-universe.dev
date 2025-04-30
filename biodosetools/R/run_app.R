#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @return Used for side-effect.
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(...) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server
    ),
    golem_opts = list(...)
  )
}
