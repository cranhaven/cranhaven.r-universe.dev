#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  shinyhelper::observe_helpers(help_dir = app_sys("/app/www/helpfiles"))

  #removeKmzFiles()

  mod_control_form_server("control_form", r)
  mod_leaflet_map_server("leaflet_map", r)
  mod_360_image_server("pano360_image", r)
}
