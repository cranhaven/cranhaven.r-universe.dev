library(shiny)
library(shinythemes)

shinyUI(fluidPage(tags$head(includeHTML("ga.html"), 
                            tags$link(rel = "stylesheet", type = "text/css", href = "progress.css")),
                  title = "Cancergram",
                  theme = shinytheme("spacelab"),
                  headerPanel(""),

                  sidebarLayout(
                    sidebarPanel(style = "background-color: #fff; border-color: #00578a;border-width: .25rem",
                                 includeMarkdown("readme.md"),
                                 uiOutput("dynamic_ui")
                    ),

                    mainPanel(
                      uiOutput("dynamic_tabset")
                    )
                  )))
