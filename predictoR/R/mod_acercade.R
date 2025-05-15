#' acercade UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_acercade_ui <- function(id){
  ns <- NS(id)
  tagList(
    img(src = "img/logo.png",
        style = paste0("padding-bottom:20px;margin-left: auto;",
                       "margin-right: auto;display: block;width: 50%;")),
    infoBoxPROMiDAT(
      labelInput("copyright"), "PROMiDAT S.A.", icono = icon("copyright")
    ),
    
    infoBoxPROMiDAT(
      labelInput("info"), tags$a(
        href = "https://www.promidat.website/", style = "color:white;",
        target = "_blank", "https://www.promidat.website"), icono = icon("info")
    ),
    
    infoBoxPROMiDAT(
      labelInput("version"), "4.1.1", icono = icon("file-code"))
  )
}
    
#' acercade Server Function
#' @noRd 
mod_acercade_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_acercade_ui("acercade_ui_1")
    
## To be copied in the server
# callModule(mod_acercade_server, "acercade_ui_1")
 
