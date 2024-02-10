library(shiny)

addResourcePath("sdmwww", file.path(
  getShinyOption(".appDir", getwd()),
  "www")
)
shinyUI(
         navbarPage(id="cirad","SDMs GUI",theme = paste0("sdmwww/", getShinyOption(".guitheme")),
           tabPanel("Help/About", uiOutput("ui_about")),
           tabPanel("Data Upload",uiOutput("ui_import_data")),
           tabPanel("Spatial Analysis",uiOutput("ui_preparation")),
           tabPanel("Modeling",uiOutput("ui_Models")),
           tabPanel("R-Code",uiOutput("ui_code")),
           tags$head(tags$script(
             src = paste0("sdmwww/", getShinyOption(".guijsfile"))
           )))
)
