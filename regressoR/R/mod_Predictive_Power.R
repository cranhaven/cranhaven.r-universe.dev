#' Predictive_Power UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Predictive_Power_ui <- function(id){
  ns <- NS(id)
  
  # PREDICTIVE POWER PAGE ---------------------------------------------------------------------------------------------------


  power.plot.pairs <- tabPanel(title = labelInput("pares"), value = "predpares",
                               withLoader(plotOutput(ns('plot_pairs_poder'), height = "75vh"),
                                          type = "html", loader = "loader4"))
  
  pagina.poder <- tabItem(tabName = "poderPred",
                          tabBoxPrmdt(id = ns("BoxPodPred"), 
                                 power.plot.pairs))
  
  
  tagList(
    pagina.poder
  )
}
    
#' Predictive_Power Server Function
#'
#' @noRd 
mod_Predictive_Power_server <- function(input, output, session, updateData, codedioma){
  ns <- session$ns
  # Show the graph of numerical predictive power
  output$plot_pairs_poder <- renderPlot({
    tryCatch({
      updateData$datos.aprendizaje
      isolate(codedioma$code <- append(codedioma$code, paste0("### pares\n", "pairs_power(datos)")))
      if (ncol(var.numericas(updateData$datos)) >= 2) {
        if(ncol(var.numericas(updateData$datos)) <= 25){
          pairs_power(updateData$datos, decimals = updateData$decimals)
        }else{
          showNotification(tr("bigPlot",codedioma$idioma), duration = 10, type = "message")
          NULL
        }
      }else{
        NULL
      }
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e),duration = 10,type = "error")
      NULL
    })
  })
}
    
## To be copied in the UI
# mod_Predictive_Power_ui("Predictive_Power_ui_1")
    
## To be copied in the server
# callModule(mod_Predictive_Power_server, "Predictive_Power_ui_1")
 
