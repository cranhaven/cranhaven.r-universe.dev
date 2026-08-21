#' Run the Shiny Application
#'
#' @description This function launches the Shiny application.
#' It uses default options for `shinyApp` and golem.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function() {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = NULL,         
      options = list(),        
      enableBookmarking = NULL, 
      uiPattern = "/"         
    ),
    golem_opts = list()
  )
}