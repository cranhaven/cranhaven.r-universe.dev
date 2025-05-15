#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @import traineR
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(...) {
  Sys.setenv("LANGUAGE" = "ES")
  if(toupper(.Platform$OS.type) != "WINDOWS") {
    options(encoding = "utf8")
  } else {
    options(encoding = "UTF-8")
  }
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server,
      options = list(launch.browser = T)
    ), 
    golem_opts = list(...)
  )
}
