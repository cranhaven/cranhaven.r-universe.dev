#' Shiny application for GenoGeoGrapher
#' @param db_list A named list of databases of reference populations. 
#' Each component is expected to be returned from \code{pops_to_DB}.
#' @param reporting_panel Logical. Should report generate and download be available after sample analysis.
#' @export
app_genogeo <- function(db_list = NULL, reporting_panel = TRUE){
  ui <- ui_fct()
  server <- function(input, output, session) server_fct(input = input, output = output, session = session)
  shinyApp(ui, server)
}