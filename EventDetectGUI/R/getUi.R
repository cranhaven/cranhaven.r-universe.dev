#' Defines the shiny UI-part of the GUI
#'
#' Generates the UI part of the GUI.
#' This method is used internally in the starting process,
#' manual use of this function is not advised.
#'
#' @export
#' @return None
getUIPage <- function(){
    dashboardPage(
        # Theme Color Setting
        skin = "blue",
        dashboardHeader(title = "EventDetectGUI"),

        # Creation of Main Tabs
        dashboardSidebar(
            sidebarMenu(id = "tabs",
                        menuItem("Home", tabName = "home"),
                        menuItem("Data Selection", tabName = "dataSelection"),
                        menuItem("Data Visualization", tabName = "visu"),
                        menuItem("Algorithms Config", tabName = "algConfig"),
                        menuItem("Run Event Detection", tabName = "runMode"),
                        menuItem("Result Visualization", tabName = "resView"),
                        menuItem("Export Results", tabName = "exports")
            )
        ),

        # Body Contents of each tab
        dashboardBody(
            # Use Shinyjs for locking and unlocking buttons
            shinyjs::useShinyjs(),

            tabItems(
                tabItem(tabName = "home",
                        h2("Welcome to the EventDetectGUI package"),
                        p("This is an interface for the EventDetectR package that enables detection of events/ anomalies in multivariate time-series data. It combines multiple well-known R packages like 'forecast' to deliver an easily configurable tool for event detection.")
                ),

                # Objective function configuration tab
                tabItem(tabName = "dataSelection",
                        fluidRow(
                            wellPanel(
                                tags$div(title="Select a data file to import",
                                         fileInput("inputFile", "Data File Selection",
                                                   multiple = TRUE,
                                                   accept = c(".rds",".csv", ".RData", ".rda"))),
                                checkboxInput("csvAdvancedConfig", "Specify advanced CSV read options", FALSE),
                                conditionalPanel(
                                    condition = "input.csvAdvancedConfig == true",
                                    tags$hr(),

                                    # Input: Checkbox if CSV input file has header
                                    checkboxInput("csvUseHeader", "Header", TRUE),
                                    fluidRow(
                                        column(4,
                                        # Input: Select separator for csv interpretation
                                        radioButtons("csvSep", "Data Separator",
                                                     choices = c(Comma = ",",
                                                                 Semicolon = ";",
                                                                 Tab = "\t"),
                                                     selected = ",")),
                                        column(4,
                                        # Input: Select quotes style for csv input
                                        radioButtons("csvQuote", "How are quotes handled?",
                                                     choices = c(None = "",
                                                                 "Double Quote" = '"',
                                                                 "Single Quote" = "'"),
                                                     selected = '"'))
                                    )
                                )
                            ),
                            uiOutput("outDataSelection"),
                            DT::dataTableOutput("outDataHead")
                        )
                ),

                tabItem(tabName = "visu",
                        wellPanel(
                            fluidRow(
                                column(4,uiOutput("visuDataSlider")),
                                column(4,uiOutput("visuDataInput"))
                            ),
                            uiOutput("uiOutPlotSelectColumnsVisu")
                        ),
                        plotlyOutput("plotVisu")
                ),

                # Configuration Tab for Algorithm settings
                tabItem(tabName = "algConfig",
                        fluidRow(
                            column(6,
                                   wellPanel(
                                       h4("General Settings"),
                                       uiOutput("generalUI")
                                   ),
                                   wellPanel(
                                       h4("Preprocessor Selection"),
                                       uiOutput("preProcessSelector"),
                                       uiOutput("preProcessUI")
                                   )
                            ),
                            column(6,
                                   wellPanel(
                                       h4("Event Detection Algorithm"),
                                       uiOutput("algorithmSelector"),
                                       uiOutput("algorithmUI")
                                   ),
                                   wellPanel(
                                       h4("Postprocess Settings"),
                                       uiOutput("postProcessSelector"),
                                       uiOutput("postProcessUI")
                                   )
                            )
                        )
                ),

                tabItem(tabName = "runMode",
                        actionButton(inputId = "runEDS", label = "Run Event Detection"),
                        p("After executing the run event detection, kindly wait until the algorithm is executed!",style = "color:red"),
                        DT::dataTableOutput("edsResultTable")
                ),

                tabItem(tabName = "resView",
                        h4("Results of Event Detection"),
                      #  plotlyOutput("edsResult",width="100%")
                      wellPanel(
                          uiOutput("uiOutPlotSelectColumnsResVisu")
                      ),
                      plotlyOutput("edsResult"),
                ),

                tabItem(tabName = "exports",
                        h4("Export the Results"),
                        wellPanel(
                            downloadButton("ExportResults", "Export")

                        )
                )
            )
        )
    )
}
