app_ui <- function(){
  shiny::addResourcePath("spatialCatalogueViewer", system.file("R", package="spatialCatalogueViewer"))
  
  ui <- shinyUI(  
    fluidPage(
      tags$head(
        tags$style(
          HTML(".shiny-notification {
                   position:fixed;
                   top: calc(50%);
                   left: calc(50%);
             }
             "
          )
        )),
      theme = shinythemes::shinytheme(getShinyOption("theme")),
      HTML(
        getShinyOption("text.title"),
      ),
      uiOutput("tab.contents")
    ) 
  )
} 



