`%then%` <- function(a, b) {
    if (is.null(a)) b else a
}

ui <- shinydashboard::dashboardPage(
    skin = "red",
    shinydashboard::dashboardHeader(title = "WeibullR.shiny"),

    ## Sidebar content
    shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
            shinydashboard::menuItem("Landing", tabName = "landing", icon = icon("helicopter-symbol")),
            shinydashboard::menuItem("Time-to-Failure", tabName = "ttf", icon = icon("th"),
              shinydashboard::menuSubItem("Data", tabName = "data", icon = shiny::icon("table")),
              shinydashboard::menuSubItem("Model", tabName = "model", icon = icon("chart-line"))
            ),
            shinydashboard::menuItem("Reliability Growth", tabName = "rg", icon = icon("th"),
              shinydashboard::menuSubItem("Data", tabName = "growthData", icon = shiny::icon("table")),
              shinydashboard::menuSubItem("Model", tabName = "growthModel", icon = icon("chart-line"))
            ),
            shiny::br(),
            shinydashboard::menuItem("Help", icon = icon("info-circle"), href = "https://paulgovan.github.io/WeibullR.shiny/"),
            shinydashboard::menuItem("Source Code", icon = icon("github"), href = "https://github.com/paulgovan/WeibullR.shiny"),
            shiny::br(),
            shiny::bookmarkButton()
        )
    ),

    ## Body content
    shinydashboard::dashboardBody(shinydashboard::tabItems(
        # First tab content
        shinydashboard::tabItem(tabName = "landing",
                                shiny::fluidRow(
                                    shinydashboard::box(
                                        title = "",
                                        width = 12,
                                        shiny::img(
                                            src = "WeibullR.png",
                                            height = 200,
                                            width = 173
                                        ),
                                        shiny::h2("WeibullR.shiny"),
                                        shiny::h3(
                                            "A ",
                                            shiny::a(href = 'http://shiny.rstudio.com', 'Shiny'),
                                            "app for Weibull Analysis"
                                            ),
                                        shiny::br(),
                                        shiny::h4(
                                            "For help getting started, view the ",
                                            shiny::a(href = 'https://paulgovan.github.io/WeibullR.shiny/', 'Project Site')
                                            ),
                                        shiny::h4(
                                            "To download an example data set, choose from the following: ",
                                            tags$ul(
                                                tags$li(shiny::downloadLink('failure_data', 'Time-to-Failure Data')),
                                                tags$li(shiny::downloadLink('censored_data', 'Right Censored Data'))
                                            )
                                        ),
                                        shiny::h4(
                                            "To upload your own data set, click",
                                            shiny::a("Data", href = "#shiny-tab-data", "data-toggle" = "tab"),
                                            " in the sidepanel to get started"
                                        ),
                                        shiny::br(),
                                        shiny::h4(
                                            shiny::HTML('&copy'),
                                            '2023 By Paul Govan. ',
                                            shiny::a(href = 'http://www.apache.org/licenses/LICENSE-2.0', 'Terms of Use.')
                                        ),
                                    )
                                )),

        # Weibull data tab content
        shinydashboard::tabItem(tabName = "data",
                                shiny::fluidRow(
                                  shiny::column(
                                    width = 3,
                                    shinydashboard::box(
                                      title = "Data Input",
                                      width = NULL,
                                      collapsible = TRUE,
                                      shiny::helpText("Select a sample data set or upload your Time-to-Failure data:"),

                                      shinyWidgets::radioGroupButtons(inputId = "dataInput",
                                                                      choices = c("Sample Data" = 1,
                                                                                  "Upload Data" = 2),
                                                                      selected = 1,
                                                                      justified = TRUE
                                      ),

                                      # Conditional panel for sample data selection
                                      shiny::conditionalPanel(
                                        condition = "input.dataInput == 1",

                                        # Demo network input select
                                        shiny::selectInput(
                                          inputId = "dataSelect",
                                          h5("Time-to-Failure Data:"),
                                          c("End-of-Life Data" = 1,
                                            "Right Censored Data" = 2
                                          )
                                        )
                                      ),

                                      # Conditional panel for file input selection
                                      shiny::conditionalPanel(
                                        condition = "input.dataInput == 2",

                                        shiny::helpText("Your data must be either a csv file containing at least 'time'
                                                and 'event' columns and optionally a 'qty' column or a csv file
                                                of interval data containing 'left' and 'right' columns."
                                        ),

                                        # File input
                                        shiny::fileInput(
                                          'file',
                                          strong('File Input:'),
                                          accept = c('text/csv',
                                                     'text/comma-separated-values',
                                                     'text/tab-separated-values',
                                                     'text/plain',
                                                     '.csv',
                                                     '.tsv'
                                          )
                                        )
                                      )
                                    ),
                                    shinydashboard::box(
                                      title = "Data Selection",
                                      width = NULL,
                                      collapsible = TRUE,
                                      shiny::helpText("Arrange your data for analysis:"),
                                      # Suspensions checkbox
                                      shiny::checkboxInput("suspensions",
                                                           label = "My data table contains suspensions"),
                                      # Intervals checkbox
                                      shiny::checkboxInput("intervals",
                                                           label = "My data table contains intervals"),
                                      # Groups checkbox
                                      shiny::checkboxInput("groups",
                                                           label = "My data table contains groups"),

                                      # Conditional panel for time column
                                      shiny::conditionalPanel(
                                        condition = "input.intervals == 0",

                                        # Time column
                                        shiny::selectizeInput(
                                          inputId = "time",
                                          h5("Time to failure column:"),
                                          c(""),
                                          selected = 1
                                        ),
                                        shiny::helpText("Time-to-Failure column must contain postive numbers (1, 2, 3 ...)")
                                      ),

                                      # Conditional panel for suspensions
                                      shiny::conditionalPanel(
                                        condition = "input.suspensions == 1",

                                        # Event column
                                        shiny::selectizeInput(
                                          inputId = "event",
                                          h5("Event type column:"),
                                          c(""),
                                          selected = 2
                                        ),
                                        shiny::helpText("Event column must be binary (e.g. 1 for Failure, 0 for Suspension)")

                                        # Failure code
                                        # shiny::selectizeInput(
                                        #   inputId = "fail_code",
                                        #   h5("Failure code:"),
                                        #   c(""),
                                        #   selected = 2
                                        # ),
                                        # shiny::helpText("Common failure codes include 1, `F`, etc.")
                                      ),

                                      # Conditional panel for groups
                                      shiny::conditionalPanel(
                                        condition = "input.groups == 1",

                                        # Quantity column
                                        shiny::selectizeInput(
                                          inputId = "qty",
                                          h5("Group column:"),
                                          c(""),
                                          selected = 3
                                        ),
                                        shiny::helpText("Group column must contain positive values (1, 2, 3 ...)")
                                      ),

                                      # Conditional panel for intervals
                                      shiny::conditionalPanel(
                                        condition = "input.intervals == 1",

                                        # Left interval column
                                        shiny::selectizeInput(
                                          inputId = "left",
                                          h5("Left-interval column:"),
                                          c(""),
                                          selected = 1
                                        ),
                                        shiny::helpText("Left column must contain positive values (1, 2, 3 ...)"),

                                        # Right interval column
                                        shiny::selectizeInput(
                                          inputId = "right",
                                          h5("Right-interval column:"),
                                          c(""),
                                          selected = 2
                                        ),
                                        shiny::helpText("Right column must contain positive values (1, 2, 3 ...)")
                                      )
                                    )
                                  ),
                                  shiny::column(
                                    width = 6,
                                    shinydashboard::box(
                                      title = "Data Table",
                                      width = NULL,
                                      collapsible = TRUE,
                                      shiny::tableOutput("table")
                                    )
                                  ),
                                  shiny::column(
                                    width = 3,
                                    # Events value box
                                    shiny::uiOutput("eventBox"),
                                    # Failures value box
                                    shiny::uiOutput("failBox"),
                                    # Suspensions value box
                                    shiny::uiOutput("suspBox")
                                  )
                                )),

        # Weibull model tab content
        shinydashboard::tabItem(tabName = "model",
                                shiny::fluidRow(
                                  shinydashboard::box(
                                    title = "Model Selection",
                                    width = 3,
                                    collapsible = TRUE,
                                    shiny::helpText("Select the type of model to perform:"),
                                    # Distribution input
                                    shiny::selectInput(
                                      inputId = "dist",
                                      h5("Distribution:"),
                                      c(
                                        # "Weibayes" = "weibayes",
                                        "Weibull 2P" = "weibull",
                                        "Weibull 3P" = "weibull3p",
                                        "Lognormal" = "lognormal"
                                      ),
                                      selected = "weibull"
                                    ),
                                    # Conditional panel for 1P Weibull
                                    shiny::conditionalPanel(
                                      condition = "input.dist == 'weibayes'",

                                      # 1P Weibull Beta
                                      shiny::numericInput(
                                        inputId = "beta",
                                        h5("Beta:"),
                                        value = 1,
                                        min = 0.1,
                                        max = 10,
                                        step = 0.1
                                      )
                                    ),
                                    # Method input
                                    shiny::selectInput(
                                      inputId = "meth",
                                      h5("Estimation Method:"),
                                      c(
                                        "Maximum Likelihood" = "mle",
                                        # "Rank Regression YX" = "rr-yonx,"
                                        "Rank Regression XY" = "rr-xony"
                                      )
                                    ),
                                    # Plotting position
                                    shiny::selectInput(
                                      inputId = "pp",
                                      h5("Plotting Position Method:"),
                                      c(
                                        "Median" = "median",
                                        # "Bernard" = "bernard",
                                        "Hazen" = "hazen",
                                        "Mean" = "mean",
                                        "Kaplan-Meier" = "kaplan-meier",
                                        "Blom" = "blom"
                                      )
                                    ),
                                    # Confidence Method
                                    # Conditional Panel for MLE
                                    shiny::conditionalPanel(
                                      condition = "input.meth == 'mle'",
                                      shiny::selectInput(inputId = "mleConf",
                                                         h5("Confidence Method:"),
                                                         c("LRB" = "lrb",
                                                           "FM" = "fm",
                                                           "FMbounds" = "fmbounds"))
                                    ),
                                    # Conditional Panel for RR
                                    shiny::conditionalPanel(
                                      condition = "input.meth == 'rr-xony'",
                                      shiny::selectInput(inputId = "rrConf",
                                                         h5("Confidence Method:"),
                                                         c("Pivotal-RR" = "pivotal-rr"))
                                    ),
                                    shiny::sliderInput(inputId = "cl", h5("Confidence Level: "),
                                                       min = 0, max = 0.99, value = 0.9, step = 0.1)
                                  ),
                                  shinydashboard::tabBox(
                                    title = "Model Results",
                                    # The id lets us use input$tabset1 on the server to find the current tab
                                    id = "tabset1",
                                    width = 9,
                                    shiny::tabPanel(
                                      "Probability Plot",
                                      shiny::fluidRow(
                                        shiny::column(width = 12,
                                                      shinyWidgets::dropdownButton(
                                                        h3("Plot Options"),
                                                        # Probability color
                                                        shiny::selectInput(
                                                          inputId = "probcol",
                                                          h5("Probability Points Color:"),
                                                          c("black",
                                                            "blue",
                                                            "red",
                                                            "yellow",
                                                            "green",
                                                            "orange",
                                                            "violet"
                                                          ),
                                                          selected = "black"
                                                        ),
                                                        # Fit color
                                                        shiny::selectInput(
                                                          inputId = "fitcol",
                                                          h5("Fit Color:"),
                                                          c("blue",
                                                            "red",
                                                            "yellow",
                                                            "green",
                                                            "orange",
                                                            "violet"
                                                          ),
                                                          selected = "blue"
                                                        ),
                                                        # CB color
                                                        shiny::selectInput(
                                                          inputId = "confcol",
                                                          h5("Confidence Bounds Color:"),
                                                          c("blue",
                                                            "red",
                                                            "yellow",
                                                            "green",
                                                            "orange",
                                                            "violet"
                                                          ),
                                                          selected = "blue"
                                                        ),
                                                        # Grid color
                                                        shiny::selectInput(
                                                          inputId = "gridcol",
                                                          h5("Grid Color:"),
                                                          c("lightgray",
                                                            "black",
                                                            "blue",
                                                            "red",
                                                            "yellow",
                                                            "green",
                                                            "orange",
                                                            "violet"
                                                          ),
                                                          selected = "lightgray"
                                                        ),
                                                        # Main title
                                                        shiny::textInput(inputId = "main",
                                                                         h5("Title:"),
                                                                         value = "Probability Plot"),
                                                        # Xlab
                                                        shiny::textInput(inputId = "xlab",
                                                                         h5("X-axis Label:"),
                                                                         value = "Time to Failure"),
                                                        # Ylab
                                                        shiny::textInput(inputId = "ylab",
                                                                         h5("Y-axis Label:"),
                                                                         value = "Unreliability (%)"),
                                                        # Significant digits
                                                        shiny::numericInput(
                                                          inputId = "signif",
                                                          h5("Significant Digits:"),
                                                          value = 3
                                                        ),
                                                        # Show CB
                                                        # shiny::checkboxInput("confBounds",
                                                        #                      label = "Show confidence bounds?",
                                                        #                      value = TRUE),
                                                        # Show suspensions plot
                                                        shiny::checkboxInput("suspPlot",
                                                                             label = "Show suspensions plot?",
                                                                             value = TRUE),
                                                        # Show results table
                                                        shiny::checkboxInput("resTab",
                                                                             label = "Show results table?",
                                                                             value = TRUE),
                                                        # Show grid
                                                        shiny::checkboxInput("grid",
                                                                             label = "Show grid?",
                                                                             value = TRUE),
                                                      circle = TRUE,
                                                      status = "danger",
                                                      icon = icon("gear")
                                        ),
                                        plotly::plotlyOutput('probPlot')
                                        )
                                      )
                                    ),
                                    shiny::tabPanel("Contour Plot",
                                                    shiny::fluidRow(
                                                      shiny::column(width = 12,
                                                                    shinyWidgets::dropdownButton(
                                                                      h3("Plot Options"),
                                                                      # Plot color
                                                                      shiny::selectInput(
                                                                        inputId = "col2",
                                                                        h5("Plot Color:"),
                                                                        c("black",
                                                                          "blue",
                                                                          "red",
                                                                          "yellow",
                                                                          "green",
                                                                          "orange",
                                                                          "violet"
                                                                        ),
                                                                        selected = "blue"
                                                                      ),
                                                                      # Grid color
                                                                      shiny::selectInput(
                                                                        inputId = "gridcol2",
                                                                        h5("Grid Color:"),
                                                                        c("lightgray",
                                                                          "black",
                                                                          "blue",
                                                                          "red",
                                                                          "yellow",
                                                                          "green",
                                                                          "orange",
                                                                          "violet"
                                                                        ),
                                                                        selected = "lightgray"
                                                                      ),
                                                                      # Show grid
                                                                      shiny::checkboxInput("grid2",
                                                                                           label = "Show grid?",
                                                                                           value = TRUE),
                                                                      # Main title
                                                                      shiny::textInput(inputId = "main2",
                                                                                       h5("Title:"),
                                                                                       value = "Contour Plot"),
                                                                      # Xlab
                                                                      shiny::textInput(inputId = "xlab2",
                                                                                       h5("X-axis Label:"),
                                                                                       value = "Eta"),
                                                                      # Ylab
                                                                      shiny::textInput(inputId = "ylab2",
                                                                                       h5("Y-axis Label:"),
                                                                                       value = "Beta"),
                                                                      # Significant digits
                                                                      shiny::numericInput(
                                                                        inputId = "signif2",
                                                                        h5("Significant Digits:"),
                                                                        value = 3
                                                                      ),
                                                                      circle = TRUE,
                                                                      status = "danger",
                                                                      icon = icon("gear")
                                                                    ),
                                                                    plotly::plotlyOutput('contPlot')
                                                      )
                                                    )
                                    )
                                  )
                                )
        ),

        # RGA data tab content
        shinydashboard::tabItem(tabName = "growthData",
                                shiny::fluidRow(
                                  shiny::column(
                                    width = 3,
                                    shinydashboard::box(
                                      title = "Data Input",
                                      width = NULL,
                                      collapsible = TRUE,
                                      shiny::helpText("Select a sample data set or upload your Reliability Growth data:"),

                                      shinyWidgets::radioGroupButtons(inputId = "growthDataInput",
                                                                      choices = c("Sample Data" = 1,
                                                                                  "Upload Data" = 2),
                                                                      selected = 1,
                                                                      justified = TRUE
                                      ),

                                      # Conditional panel for sample data selection
                                      shiny::conditionalPanel(
                                        condition = "input.growthDataInput == 1",

                                        # Demo data input select
                                        shiny::selectInput(
                                          inputId = "growthDataSelect",
                                          h5(" Data:"),
                                          c("Simple Data Set" = 1,
                                            "Large Data Set" = 2
                                          )
                                        )
                                      ),

                                      # Conditional panel for file input selection
                                      shiny::conditionalPanel(
                                        condition = "input.growthDataInput == 2",

                                        shiny::helpText("Your data must be a csv file containing 'cumulative time' and 'failure' columns."
                                        ),

                                        # File input
                                        shiny::fileInput(
                                          'growthFile',
                                          strong('File Input:'),
                                          accept = c('text/csv',
                                                     'text/comma-separated-values',
                                                     'text/tab-separated-values',
                                                     'text/plain',
                                                     '.csv',
                                                     '.tsv'
                                          )
                                        )
                                      )
                                    ),
                                    shinydashboard::box(
                                      title = "Data Selection",
                                      width = NULL,
                                      collapsible = TRUE,
                                      shiny::helpText("Arrange your data for analysis:"),

                                      # Times column
                                      shiny::selectizeInput(
                                        inputId = "times",
                                        h5("Cumulative Time Column:"),
                                        c(""),
                                        selected = 1
                                      ),
                                      shiny::helpText("Cumulative time column must contain postive numbers (1, 2, 3 ...)"),

                                      # Failures column
                                      shiny::selectizeInput(
                                        inputId = "failures",
                                        h5("Failure Column:"),
                                        c(""),
                                        selected = 2
                                      ),
                                      shiny::helpText("Failure column must must contain postive numbers (1, 2, 3 ...)")
                                    )
                                  ),
                                  shiny::column(
                                    width = 6,
                                    shinydashboard::box(
                                      title = "Data Table",
                                      width = NULL,
                                      collapsible = TRUE,
                                      shiny::tableOutput("growthTable")
                                    )
                                  ),
                                  shiny::column(
                                    width = 3,
                                    # Cumulative Failures value box
                                    shiny::uiOutput("failuresBox"),
                                    # Cumulative time value box
                                    shiny::uiOutput("timesBox")
                                  )
                                )
        ),

        # Growth model tab content
        shinydashboard::tabItem(tabName = "growthModel",
                                shiny::fluidRow(
                                  shinydashboard::box(
                                    title = "Model Selection",
                                    width = 3,
                                    collapsible = TRUE,
                                    shiny::helpText("Select the type of model to perform:"),
                                    # Distribution input
                                    shiny::selectInput(
                                      inputId = "growthModel",
                                      h5("Model:"),
                                      c(
                                        "Crow-AMSAA" = 1,
                                        "Piecewise Weibull NHPP" = 2,
                                        "Piecewise Weibull NHPP with Change Point Detection" = 3
                                      ),
                                      selected = "Crow-AMSAA"
                                    ),
                                    # Conditional panel for Piecewise Weibull NHPP
                                    shiny::conditionalPanel(
                                      condition = "input.growthModel == 2",

                                      # Breakpoint
                                      shiny::helpText("Enter the breakpoint for the Piecewise Weibull:"),
                                      # shiny::textInput('breakpoints', h5("Breakpoint(s)"))
                                      shiny::numericInput(
                                        inputId = "breakpoints",
                                        h5("Breakpoint:"),
                                        value = 1,
                                        min = 0.1,
                                        step = 0.1
                                      )
                                    ),

                                    shiny::sliderInput(inputId = "growthConf", h5("Confidence Level: "),
                                                       min = 0, max = 0.99, value = 0.9, step = 0.1)
                                  ),
                                  shinydashboard::tabBox(
                                    title = "Model Results",
                                    # The id lets us use input$tabset2 on the server to find the current tab
                                    id = "tabset2",
                                    width = 9,
                                    shiny::tabPanel(
                                      "Reliability Growth Plot",
                                      shiny::fluidRow(
                                        shiny::column(width = 12,
                                                      shinyWidgets::dropdownButton(
                                                        h3("Plot Options"),
                                                        # Points color
                                                        shiny::selectInput(
                                                          inputId = "pointCol",
                                                          h5("Points Color:"),
                                                          c("black",
                                                            "blue",
                                                            "red",
                                                            "yellow",
                                                            "green",
                                                            "orange",
                                                            "violet"
                                                          ),
                                                          selected = "black"
                                                        ),
                                                        # Fit color
                                                        shiny::selectInput(
                                                          inputId = "modelCol",
                                                          h5("Model Color:"),
                                                          c("blue",
                                                            "red",
                                                            "yellow",
                                                            "green",
                                                            "orange",
                                                            "violet"
                                                          ),
                                                          selected = "blue"
                                                        ),
                                                        # CB color
                                                        shiny::selectInput(
                                                          inputId = "growthConfCol",
                                                          h5("Confidence Bounds Color:"),
                                                          c("blue",
                                                            "red",
                                                            "yellow",
                                                            "green",
                                                            "orange",
                                                            "violet"
                                                          ),
                                                          selected = "blue"
                                                        ),
                                                        # Grid color
                                                        shiny::selectInput(
                                                          inputId = "growthGridCol",
                                                          h5("Grid Color:"),
                                                          c("lightgray",
                                                            "black",
                                                            "blue",
                                                            "red",
                                                            "yellow",
                                                            "green",
                                                            "orange",
                                                            "violet"
                                                          ),
                                                          selected = "lightgray"
                                                        ),
                                                        # Breakpoint color
                                                        shiny::selectInput(
                                                          inputId = "breakCol",
                                                          h5("Breakpoint Color:"),
                                                          c("blue",
                                                            "red",
                                                            "yellow",
                                                            "green",
                                                            "orange",
                                                            "violet"
                                                          ),
                                                          selected = "black"
                                                        ),
                                                        # Main title
                                                        shiny::textInput(inputId = "growthMain",
                                                                         h5("Title:"),
                                                                         value = "Reliability Growth Plot"),
                                                        # Xlab
                                                        shiny::textInput(inputId = "growthXlab",
                                                                         h5("X-axis Label:"),
                                                                         value = "Cumulative Time"),
                                                        # Ylab
                                                        shiny::textInput(inputId = "growthYlab",
                                                                         h5("Y-axis Label:"),
                                                                         value = "Cumulative Failures"),
                                                        circle = TRUE,
                                                        status = "danger",
                                                        icon = icon("gear")
                                                      ),
                                                      plotly::plotlyOutput('growthPlot'),
                                                      br(),
                                                      # Beta value(s)
                                                      shiny::fluidRow(
                                                        shinydashboard::box(
                                                          title = "Beta Value(s)",
                                                          solidHeader = TRUE,
                                                          shiny::verbatimTextOutput("betaValues")
                                                        )
                                                      ),
                                                      # # Lambda value(s)
                                                      # shiny::fluidRow(
                                                      #   shinydashboard::box(
                                                      #     title = "Lambda Value(s)",
                                                      #     solidHeader = TRUE,
                                                      #     shiny::verbatimTextOutput("lambdaValues")
                                                      #   )
                                                      # )
                                        )
                                      )
                                    ),
                                    shiny::tabPanel("Duane Plot",
                                                    shiny::fluidRow(
                                                      shiny::column(width = 12,
                                                                    shinyWidgets::dropdownButton(
                                                                      h3("Plot Options"),
                                                                      # Plot color
                                                                      shiny::selectInput(
                                                                        inputId = "pointCol2",
                                                                        h5("Points Color:"),
                                                                        c("black",
                                                                          "blue",
                                                                          "red",
                                                                          "yellow",
                                                                          "green",
                                                                          "orange",
                                                                          "violet"
                                                                        ),
                                                                        selected = "black"
                                                                      ),
                                                                      # Line color
                                                                      shiny::selectInput(
                                                                        inputId = "modelCol2",
                                                                        h5("Model Color:"),
                                                                        c("black",
                                                                          "blue",
                                                                          "red",
                                                                          "yellow",
                                                                          "green",
                                                                          "orange",
                                                                          "violet"
                                                                        ),
                                                                        selected = "blue"
                                                                      ),
                                                                      # Grid color
                                                                      shiny::selectInput(
                                                                        inputId = "growthGridCol2",
                                                                        h5("Grid Color:"),
                                                                        c("lightgray",
                                                                          "black",
                                                                          "blue",
                                                                          "red",
                                                                          "yellow",
                                                                          "green",
                                                                          "orange",
                                                                          "violet"
                                                                        ),
                                                                        selected = "lightgray"
                                                                      ),
                                                                      # Main title
                                                                      shiny::textInput(inputId = "duaneMain",
                                                                                       h5("Title:"),
                                                                                       value = "Duane Plot"),
                                                                      # Xlab
                                                                      shiny::textInput(inputId = "duaneXlab",
                                                                                       h5("X-axis Label:"),
                                                                                       value = "Cumulative Time"),
                                                                      # Ylab
                                                                      shiny::textInput(inputId = "duaneYlab",
                                                                                       h5("Y-axis Label:"),
                                                                                       value = "Cumulative MTBF"),
                                                                      circle = TRUE,
                                                                      status = "danger",
                                                                      icon = icon("gear")
                                                                    ), plotly::plotlyOutput('duanePlot')
                                                      )
                                                    )
                                    )
                                  )
                                )
        )
    )
    )
)

server <- function(input, output, session) {

    session$onSessionEnded(stopApp)

    # Example Time-to-Failure data
    acid_gas_compressor <- read.csv('data/acid_gas_compressor.csv')

    # Time-to-Failure data handler
    output$failure_data <- shiny::downloadHandler(
        filename = "acid_gas_compressor.csv",
        content = function(file) {
            write.csv(acid_gas_compressor, file, row.names = FALSE)
        }
    )

    # Example Right Censored data
    treat6mp <- read.csv('data/treat6mp.csv')

    # Right Censored data handler
    output$censored_data <- shiny::downloadHandler(
        filename = "treat6mp.csv",
        content = function(file) {
            write.csv(treat6mp, file, row.names = FALSE)
        }
    )

    # Get the data selection from user
    dat <- shiny::reactive({
      if (input$dataInput == 1) {

        if (input$dataSelect == 1) {
          dat <- data.frame(read.csv('data/acid_gas_compressor.csv'))
        } else if (input$dataSelect == 2) {
          dat <- data.frame(read.csv('data/treat6mp.csv'))
        }
      } else if (input$dataInput == 2) {

        # Get the uploaded file from user
        inFile <- input$file
        if (is.null(inFile))
          return(NULL)
        dat <- data.frame(read.csv(inFile$datapath))
      }
    })

    # Create the failures value box
    output$eventBox <- shiny::renderUI({

      # Get the number of failures in the data set
      if (is.null(dat())) {
        events <- NULL
      } else
        events <- nrow(dat())

      shinydashboard::valueBox(events,
                               "Events",
                               icon = shiny::icon("table"),
                               color = "blue",
                               width = 12)
    })

    # Create the failures value box
    output$failBox <- shiny::renderUI({

      # Get the number of failures in the data set
      if (is.null(dat())) {
        failures <- NULL
      } else if (is.null(input$event) || input$suspensions == 0) {
        failures <- nrow(dat())
      } else if (input$suspensions == 1) {
        req(input$event)
        event <- input$event
        datsub <- dat()[dat()[[event]] == 1, ]
        failures <- nrow(datsub)
      }

      shinydashboard::valueBox(failures,
                               "Failures",
                               icon = shiny::icon("arrow-down"),
                               color = "red",
                               width = 12)
    })

    # Create the suspensions value box
    output$suspBox <- shiny::renderUI({

      # Get the number of suspensions in the data set
      if (is.null(dat())) {
        suspensions <- NULL
      } else if (is.null(input$event) || input$suspensions == 0) {
          suspensions <- 0
      } else if (input$suspensions == 1) {
        req(input$event)
        event <- input$event
        datsub <- dat()[dat()[[event]] == 0, ]
        suspensions <- nrow(datsub)
      }

      shinydashboard::valueBox(suspensions,
                               "Suspensions",
                               icon = shiny::icon("arrow-up"),
                               color = "green",
                               width = 12)
    })

    # Get the column names
    coln <- shiny::reactive({
        coln <- names(dat())
    })

    # Send the column names to the user
    shiny::observe({
        shiny::updateSelectInput(session, "time", choices = coln())
    })
    shiny::observe({
        shiny::updateSelectInput(session, "event", choices = coln())
    })
    shiny::observe({
        shiny::updateSelectInput(session, "qty", choices = coln())
    })
    shiny::observe({
        shiny::updateSelectInput(session, "left", choices = coln())
    })
    shiny::observe({
        shiny::updateSelectInput(session, "right", choices = coln())
    })

    # Get the event names when the data is right-censored
    # eventn <- shiny::reactive({
    #   if (is.null(input$event) || input$suspensions == 0) {
    #     eventn <- NULL
    #   } else if (input$suspensions == 1) {
    #     eventn <- unique(dat()[, input$event])
    #   }
    # })

    # Send the event names to the user
    # shiny::observe({
    #   shiny::updateSelectInput(session, "fail_code", choices = eventn())
    # })

    # Check for suspensions
    event <- shiny::reactive({
        if (is.null(input$event) || input$suspensions == 0) {
            event <- rep(1, length(subset(dat(), select = 1)))
        } else if (input$suspensions == 1) {
            event <- subset(dat(), select = input$event)
        }
    })

    # Check for groups
    qty <- shiny::reactive({
        if (is.null(input$qty) || input$groups == 0) {
            qty <- rep(1, length(subset(dat(), select = 1)))
        } else if (input$groups == 1) {
            qty <- subset(dat(), select = input$qty)
        }
    })

    # Arrange data for the wblr function
    wblr_dat <- shiny::reactive({
        if (is.null(dat()))
            return(NULL)

        # Check for intervals
        if (is.null(input$left) || is.null(input$right) || input$intervals == 0) {
            time <- subset(dat(), select = input$time)
            wblr_dat <- data.frame(time, event(), qty())
        } else if (input$intervals == 1) {
            time <- subset(dat(), select = input$right)
            colnames(time) <- 'time'
            wblr_dat0 <- data.frame(time, event = event(), qty = qty())
                wblr_dat0 <- subset(event == 0)
        }
    })

    # Arrange data for interval censored models
    ints_dat <- shiny::reactive({
        if (is.null(dat()))
            return(NULL)

        # Check for intervals
        if (is.null(input$left) || is.null(input$right) || input$intervals == 0) {
            ints_dat <- NULL
        } else if (input$intervals == 1) {
            left <- subset(dat(), select = input$left)
            right <- subset(dat(), select = input$right)
            ints_dat <- data.frame(left, right, event = event(), qty = qty()) %>%
                subset(event == 1) %>%
                subset(select = c(left, right, qty))
        }
    })

    # Create a table of the user dataset
    output$table = shiny::renderTable({
        if (is.null(dat()))
            return(NULL)

        shiny::validate(
            shiny::need(!is.null(dat()), message = FALSE)
        )

        dat()
    }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')

    # Create a table of the user dataset
    # output$table <- DT::renderDT({
    #     if (is.null(dat()))
    #         return(NULL)
    #
    #     shiny::validate(
    #         shiny::need(!is.null(dat()), message = FALSE)
    #     )
    #
    #     DT::datatable(dat(), rownames = FALSE,
    #                   options = list(columnDefs = list(
    #                       list(className = 'dt-center', targets = "_all")
    #                   ))
    #                   # , editable = TRUE
    #     )
    # })

    # Get data edits from the user
    # observeEvent(input$table_cell_edit, {
    #   dat <<- editData(dat(), input$table_cell_edit, 'table', rownames = FALSE)
    # })

    # Create a wblr object
    wblr_obj <- shiny::reactive({
        if (is.null(wblr_dat()))
            return(NULL)

        # Error handling
        # Need error handling for intervals
        shiny::validate(
            shiny::need(
                try(is.numeric(wblr_dat()$time)),
                "Time column must be numeric"
            ) %then%
                shiny::need(
                    try(all(wblr_dat()$time>0)),
                    "Time column must contain positive numbers"
                ) %then%
                shiny::need(
                    try(all(wblr_dat()$event %in% 0:1)),
                    "Event column must be a binary variable"
                ) %then%
                shiny::need(
                    try(is.numeric(wblr_dat()$qty)),
                    "Group column must be numeric"
                ) %then%
                shiny::need(
                    try(all(wblr_dat()$qty>0)),
                    "Group column must contain positive numbers"
                )
        )

        # Get the confidence method
        if (input$meth == "mle") confMeth <- input$mleConf
        else confMeth = input$rrConf

        # Run the wblr object (Weibayes)
        if (input$dist == "weibayes") {

            wblr_obj <-
                WeibullR::wblr.fit(
                    WeibullR::wblr(
                        x = wblr_dat(),
                        interval = ints_dat(),
                        pp = input$pp
                    ),
                    method.fit = input$dist,
                    weibayes.beta = input$beta

                )
        }

        # Run the wblr object (non-Weibayes)
        else {

            wblr_obj <-
                WeibullR::wblr.conf(WeibullR::wblr.fit(
                    WeibullR::wblr(
                        x = wblr_dat(),
                        interval = ints_dat(),
                        pp = input$pp
                    ),
                    dist = input$dist,
                    method.fit = input$meth
                ),
                method.conf = confMeth,
                ci = input$cl)

        }
    })

    # Create a suspensions vector
    susp_vec <- shiny::reactive({
        if (is.null(input$event) || input$suspensions == 0) {
            susp_vec <- NULL
        } else if (input$suspensions == 1) {
            # susp_vec <- NULL
            susp_vec <- as.numeric(unlist(subset(wblr_dat(), event == 0, select = 'time')))
        }
    })

    # Build the probability plot
    output$probPlot <- plotly::renderPlotly({
        if (is.null(wblr_obj()))
            return(NULL)

        WeibullR.plotly::plotly_wblr(
            wblr_obj(),
            susp = susp_vec(),
            showSusp = input$suspPlot,
            showRes = input$resTab,
            main = input$main,
            xlab = input$xlab,
            ylab = input$ylab,
            probCol = input$probcol,
            fitCol = input$modelCol,
            confCol = input$confcol,
            gridCol = input$gridcol,
            showGrid = input$grid,
            signif = input$signif
        )

    })

    # Build the contour plot
    output$contPlot <- plotly::renderPlotly({
        if (is.null(wblr_obj()))
            return(NULL)
        shiny::validate(
            shiny::need(
                try(input$meth == "mle"),
                "Contour plots are only available for the ''MLE' estimation method..."
            ) %then%
                shiny::need(
                    try(input$mleConf == 'lrb'),
                    "Contour plots are only available for the 'LRB' confidence method..."
                )
        )
        WeibullR.plotly::plotly_contour(
            wblr_obj(),
            main = input$main2,
            xlab = input$xlab2,
            ylab = input$ylab2,
            col = input$col2,
            gridCol = input$gridcol2,
            showGrid = input$grid2,
            signif = input$signif2
        )
    })

    # Example reliability growth data
    simpleData <- read.csv('data/simpleData.csv')

    # Reliability growth data handler
    output$growthData <- shiny::downloadHandler(
      filename = "simpleData.csv",
      content = function(growthFile) {
        write.csv(simpleData, growthFile, row.names = FALSE)
      }
    )

    # Example large data set
    largeData <- read.csv('data/largeData.csv')

    # Large data handler
    output$largeData <- shiny::downloadHandler(
      filename = "largeData.csv",
      content = function(growthFile) {
        write.csv(largeData, growthFile, row.names = FALSE)
      }
    )

    # Get the data selection from user
    growthDat <- shiny::reactive({
      if (input$growthDataInput == 1) {

        if (input$growthDataSelect == 1) {
          growthDat <- data.frame(read.csv('data/simpleData.csv'))
        } else if (input$growthDataSelect == 2) {
          growthDat <- data.frame(read.csv('data/largeData.csv'))
        }
      } else if (input$growthDataInput == 2) {

        # Get the uploaded file from user
        growthFile <- input$growthFile
        if (is.null(growthFile))
          return(NULL)
        growthDat <- data.frame(read.csv(growthFile$datapath))
      }
    })

    # Create the cumulative failures value box
    output$failuresBox <- shiny::renderUI({

      # Get the number of failures in the data set
      if (is.null(growthDat())) {
        maxFailures <- NULL
      } else {
        req(input$failures)
        maxFailures <- sum(growthDat()[[input$failures]])
      }

      shinydashboard::valueBox(maxFailures,
                               "Cumulative Failures",
                               icon = shiny::icon("arrow-down"),
                               color = "red",
                               width = 12)
    })

    # Create the cumulative time value box
    output$timesBox <- shiny::renderUI({

      # Get the max time in the data set
      if (is.null(growthDat())) {
        maxTime <- NULL
      } else {
        req(input$times)
        maxTime <- tail(growthDat()[[input$times]], 1)
      }

      shinydashboard::valueBox(maxTime,
                               "Cumulative Time",
                               icon = shiny::icon("table"),
                               color = "blue",
                               width = 12)
    })

    # Get the column names
    growthColn <- shiny::reactive({
      growthColn <- names(growthDat())
    })

    # Send the column names to the user
    shiny::observe({
      shiny::updateSelectInput(session, "times", choices = growthColn())
    })
    shiny::observe({
      shiny::updateSelectInput(session, "failures", choices = growthColn())
    })

    # Create a table of the user dataset
    output$growthTable = shiny::renderTable({
      if (is.null(growthDat()))
        return(NULL)

      shiny::validate(
        shiny::need(!is.null(growthDat()), message = FALSE)
      )

      growthDat()
    }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')

    # Create a rga object
    rga_obj <- shiny::reactive({
      if (is.null(growthDat()))
        return(NULL)

      # Error handling
      shiny::validate(
        shiny::need(
          try(is.numeric(growthDat()[[input$times]])),
          "Time column must be numeric."
        ) %then%
          shiny::need(
            try(all(growthDat()[[input$times]])),
            "Time column must contain positive numbers."
          ) %then%
          shiny::need(
            try(is.numeric(growthDat()[[input$failures]])),
            "Failure column must be numeric."
          ) %then%
          shiny::need(
            try(all(growthDat()[[input$failures]]>0)),
            "Failure column must contain positive numbers."
          )
      )

      # Run the rga object
      if (input$growthModel == 1) {
        rga_obj <-
          ReliaGrowR::rga(
            times = growthDat()[[input$times]],
            failures = growthDat()[[input$failures]],
            conf_level = input$growthConf
          )
      } else if (input$growthModel == 2) {

        # Error handling
        shiny::validate(
            shiny::need(
              try(input$breakpoints > min(growthDat()[[input$times]])),
              "Breakpoint must be greater than the minimum failure time."
            ) %then%
            shiny::need(
              try(input$breakpoints < max(growthDat()[[input$times]])),
              "Breakpoint must be less than the maximum failure time."
            )
        )

        # Transform user-supplied breakpoints into a numeric vector
        breakpoints <- as.numeric(unlist(input$breakpoints,","))
        rga_obj <-
          ReliaGrowR::rga(
            times = growthDat()[[input$times]],
            failures = growthDat()[[input$failures]],
            model_type = "Piecewise NHPP",
            breaks = breakpoints,
            conf_level = input$growthConf
          )
      } else if (input$growthModel == 3) {
        rga_obj <-
          ReliaGrowR::rga(
            times = growthDat()[[input$times]],
            failures = growthDat()[[input$failures]],
            model_type = "Piecewise NHPP",
            conf_level = input$growthConf
          )
      }
    })

    # Get the selected node measure from the user and print the results
    output$betaValues <- shiny::renderPrint({
      if (is.null(rga_obj()))
        return(NULL)
      else {
        if (input$growthModel == 1) {
          print(rga_obj()$betas)
        } else
          print(rga_obj()$betas$log_times)
      }
    })

    # # Get the selected node measure from the user and print the results
    # output$lambdaValues <- shiny::renderPrint({
    #   if (is.null(rga_obj()))
    #     return(NULL)
    #   else {
    #     if (input$growthModel == 1) {
    #       print(rga_obj()$lambdas)
    #     } else
    #       print(rga_obj()$lambdas$log_times)
    #   }
    # })

    # Build the reliability growth plot
    output$growthPlot <- plotly::renderPlotly({
      if (is.null(rga_obj()))
        return(NULL)

      WeibullR.plotly::plotly_rga(
        rga_obj(),
        main = input$growthMain,
        xlab = input$growthXlab,
        ylab = input$growthYlab,
        pointCol = input$pointCol,
        fitCol = input$modelCol,
        confCol = input$growthConfCol,
        gridCol = input$growthGridCol,
        breakCol = input$breakCol
      )
    })

    # Create a duane object
    duane_obj <- shiny::reactive({
      if (is.null(growthDat()))
        return(NULL)

      # Error handling
      shiny::validate(
        shiny::need(
          try(is.numeric(growthDat()[[input$times]])),
          "Time column must be numeric"
        ) %then%
          shiny::need(
            try(all(growthDat()[[input$times]]>0)),
            "Time column must contain positive numbers"
          ) %then%
          shiny::need(
            try(is.numeric(growthDat()[[input$failures]])),
            "Failure column must be numeric"
          ) %then%
          shiny::need(
            try(all(growthDat()[[input$failures]]>0)),
            "Failure column must contain positive numbers"
          )
      )

      # Run the duane object
        duane_obj <-
          ReliaGrowR::duane_plot(
            times = growthDat()[[input$times]],
            failures = growthDat()[[input$failures]]
          )
    })

    # Build the duane plot
    output$duanePlot <- plotly::renderPlotly({
      if (is.null(duane_obj()))
        return(NULL)

      WeibullR.plotly::plotly_duane(
        duane_obj(),
        pointCol = input$pointCol2,
        fitCol = input$modelCol2,
        gridCol = input$growthGridCol2,
        main = input$duaneMain,
        xlab = input$duaneXlab,
        ylab = input$duaneYlab
      )
    })
}

shiny::shinyApp(ui, server, enableBookmarking = "url")
