require(shiny)
require(shinyWidgets)
require(shinycssloaders)

exp.stop <- c()

shinydashboard::dashboardPage( skin = "blue",
###header
  shinydashboard::dashboardHeader(title = "LDAShiny",tags$li(class = "dropdown", actionLink("stop_radiant", "Stop", icon = icon("power-off"),
                                                                                            onclick = "setTimeout(function(){window.close();}, 100); ")
                                                             )
                                  ),
# dashboardSidebar##########################
shinydashboard::dashboardSidebar(
  width = 400,
  shinydashboard::sidebarMenu(
    shinydashboard::menuItem("About LDAShiny",
                             tabName = "tab0"
    ),
    shinydashboard::menuItem("Preprocessing",
                             tabName = "tab1",
                             icon = icon("filter")
    ),
    shinydashboard::menuItem("Document Term Matrix Visualizations",
                             tabName = "tab2",
                             icon = icon("chart-bar")
    ),
    shinydashboard::menuItem("Number of topic (inference)",
                             tabName = "tab3",
                             icon = icon("calculator")
    ),
    shinydashboard::menuItem("LDA model",
                             tabName = "tab4",
                             icon = icon("desktop"),
                             shinydashboard::menuSubItem("Run model",
                                                         tabName = "tab41",
                                                         icon = icon("gear")
                             ),
                             shinydashboard::menuSubItem("Download tabular results",
                                                         tabName = "tab42",
                                                         icon = icon("download")
                             ),
                             shinydashboard::menuSubItem("Download graphics results",
                                                         tabName = "tab43",
                                                         icon = icon("download")
                             )
    ),

    shinydashboard::menuItem("Tutorial",
                             tabName = "tab5",
                             icon = icon("chalkboard-teacher")
    )
  )
),
## dashboardBody ##########################
shinydashboard::dashboardBody(
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "custom.css"
    )
  ),
  shinyWidgets::useSweetAlert(),
  shinydashboard::tabItems(
    shinydashboard::tabItem(
      tabName = "tab1",
      shinydashboard::box(
        width = 400,
        title = "Preprocessing",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        shiny::sidebarPanel(
          shinyjs::useShinyjs(),
          helpText(h3("Upload data file")),
          br(),
          helpText(h4("Example data")),
          br(),
          shinyWidgets::prettyCheckbox(
            inputId = "example",
            label = "Use example data set?",
            value = FALSE,
            status = "warning"
          ),
          br(),
          br(),
          fileInput("file", "Choose CSV File",
                    multiple = T
          ),
          helpText("Select the read.table parameters below"),
          shiny::checkboxInput(
            inputId = "header",
            label = "Header",
            value = TRUE
          ),
          shiny::checkboxInput(
            inputId = "stringAsFactors",
            "stringAsFactors", FALSE
          ),
          shiny::radioButtons(
            inputId = "sep",
            label = "Separator",
            choices = c(
              "Comma" = ",",
              "Semicolon" = ";",
              "Tab" = "\t", "Space" = " "
            ),
            selected = ","
          ),
          uiOutput("selectfile"),
          br(),
          br(),
          helpText(h3("Data cleanning")),
          br(),
          helpText(" Click the incorporate information button three times"),
          shinyWidgets::actionBttn("choice", "incorporate information",
                                   style = "float",
                                   block = TRUE,
                                   color = "primary"
          ),
          br(),
          br(),
          div(
            style = "display: inline-block;vertical-align:top; width: 170px;",
            selectInput("column1", "Select id document",
                        choices = NULL
            )
          ), # no choices before uploading
          shinyBS::bsPopover("column1",
                             "Select which column contains the id.",
                             options = list(container = "body")
          ),
          div(
            style = "display: inline-block;vertical-align:top; width: 170px;",
            selectInput("column2",
                        "Select document vector",
                        choices = NULL
            )
          ), # no choices before uploading

          shinyBS::bsPopover("column2",
                             "Select which column contains the column of text.",
                             options = list(container = "body")
          ),

          div(
            style = "display: inline-block;vertical-align:top; width: 170px;",
            selectInput(
              inputId = "column3",
              label = "Select publish year",
              choices = NULL
            )
          ), # no choices before uploading

          shinyBS::bsPopover("column3",
                             "Select which column contains years",
                             options = list(container = "body")
          ),
          shinyWidgets::awesomeRadio(
            inputId = "ngrams",
            label = "ngrams",
            choices = c("Unigrams" = 1L, "Bigrams" = 2L, "Trigrams" = 3L),
            selected = 1L,
            status = "primary"
          ),
          shinyWidgets::awesomeCheckbox(
            inputId = "removenumber",
            label = "Remove number",
            value = TRUE,
            status = "danger"
          ),
          shinyWidgets::pickerInput(
            inputId = "Language",
            label = "Select language for stopword",
            choices = c(
              "da", "nl", "en", "fi",
              "fr", "de", "hu", "it",
              "no", "pt", "ro",
              "ru", "es", "sv"
            ),
            choicesOpt = list(
              subtext = paste(Languages <- c(
                "danish", "dutch", "english", "finnish",
                "french", "german", "hungarian", "italian",
                "norwegian", "portuguese", "romanian",
                "russian", "spanish", "swedish"
              ))
            )
          ),
          textInput("stopwords",
                    label = "Stop Words",
                    value = paste(exp.stop, collapse = ", "),
                    placeholder = "also, such, really..."
          ),
          shinyBS::bsPopover("stopwords", "Include additional stop words to remove(words
                                        must be separated by commas)",
                             options = list(container = "body")
          ),
          br(),
          div(
            style = "display: inline-block;vertical-align:top; width: 170px;",
            shinyWidgets::awesomeCheckbox(
              inputId = "checkStemming",
              label = "Stemming",
              value = FALSE,
              status = "danger"
            )
          ),
          shinyBS::bsPopover("checkStemming", "Click if you want to stemming",
                             options = list(container = "body")
          ),
          div(
            style = "display: inline-block;vertical-align:top; width: 200px;",
            selectInput("Stemm", " Stemming langauge",
                        choices = c(
                          "porter", "danish", "dutch", "english", "finnish",
                          "french", "german", "hungarian", "italian",
                          "norwegian", "portuguese", "romanian",
                          "russian", "spanish", "swedish"
                        )
            )
          ),
          br(),
          sliderInput(
            inputId = "sparce",
            label = "Sparsity:",
            min = 0.00001,
            max = 0.99999,
            value = 0.995,
            step = 0.0001
          ),
          shinyBS::bsPopover("minDoc", "Remove sparse terms:",
                             options = list(container = "body")
          ),
          br(),
          # shinyjs::hidden(
          div(
            id = "dTm",
            style = "display:inline-block",
            shinyWidgets::actionBttn(
              inputId = "dtm.update",
              label = "Create DTM",
              style = "float",
              block = TRUE,
              color = "primary"
            )
          ),
          bsModalNoClose(
            id = "DTMdim",
            title = "dim DTM",
            trigger = "dTm",
            size = "large",
            shinycssloaders::withSpinner(DT::DTOutput("Table_dim"))
          ),

          DT::DTOutput("table_display1"),
          DT::DTOutput("table_display2")
        ),
        mainPanel(
          uiOutput("tb"),
          uiOutput("tb2")
        ) # tab panel display
      )
    ), shinydashboard::tabItem(
      tabName = "tab2",
      collapsible = TRUE,
      shinydashboard::box(
        width = 350,
        title = "Visualization",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        ## View data action button hidden initially until the dataset is loaded
        div(
          id = "data_b",
          style = "display:inline-block",
          shinyWidgets::actionBttn(
            inputId = "data",
            label = "View Data",
            icon = icon("table"),
            style = "float",
            block = TRUE,
            color = "primary"
          )
        ),
        ## View plot action button hidden initially until the dataset is loaded
        div(
          id = "plot_b",
          style = "display:inline-block",
          shinyWidgets::actionBttn(
            inputId = "plot",
            label = "View barplot",
            icon = icon("bar-chart"),
            style = "float",
            block = TRUE,
            color = "primary"
          )
        ),

        div(
          id = "plot_c",
          style = "display:inline-block",
          shinyWidgets::actionBttn(
            inputId = "plot2",
            label = "View wordcloud",
            icon = icon("bar-chart"),
            style = "float",
            block = TRUE,
            color = "primary"
          )
        ),

        bsModalNoClose(
          id = "dataset",
          title = "Basic corpus statistics",
          trigger = "data",
          size = "large",
          shinycssloaders::withSpinner(DT::DTOutput("data_b"))
        ),

        ## Shiny BS Modal to display the plot inside a modal
        ## A spinner is also added
        bsModalNoClose(
          id = "Plot", title = "Barplot", trigger = "plot", size = "large",
          div(
            style = "display: inline-block;vertical-align:top; width: 120px;",
            tags$h5("Select number of term")
          ),
          div(
            style = "display: inline-block;vertical-align:top; width: 120px;",
            shinyWidgets::dropdown(sliderInput(
              inputId = "b",
              label = "Select number of word",
              min = 10,
              max = 500,
              value = 10
            ),
            style = "unite",
            icon = icon("gear"),
            status = "danger",
            width = "500px",
            animate = shinyWidgets::animateOptions(
              enter = shinyWidgets::animations$fading_entrances$fadeInLeftBig,
              exit = shinyWidgets::animations$fading_exits$fadeOutRightBig
            )
            )
          ),
          shinycssloaders::withSpinner(highcharter::highchartOutput("plot_gg"))
        ),

        bsModalNoClose(
          id = "Plot2",
          title = "Wordcloud",
          trigger = "plot2",
          size = "large",
          div(
            style = "display: inline-block;vertical-align:top; width: 120px;",
            tags$h5("Select number of term")
          ),
          div(
            style = "display: inline-block;vertical-align:top; width: 120px;",
            shinyWidgets::dropdown(sliderInput(
              inputId = "c",
              label = "Select number of word",
              min = 10,
              max = 500,
              value = 10
            ),
            style = "unite",
            icon = icon("gear"),
            status = "danger",
            width = "500px",
            animate = shinyWidgets::animateOptions(
              enter = animations$fading_entrances$fadeInLeftBig,
              exit = animations$fading_exits$fadeOutRightBig
            )
            )
          ),
          shinycssloaders::withSpinner(highcharter::highchartOutput("plot_gf"))
        )
      )
    ),
    shinydashboard::tabItem(
      tabName = "tab3",
      width = 300,
      shinydashboard::box(
        title = "3. Number of topic",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        shinydashboard::tabBox(
          width = 300,
          title = "",
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset1",
          tabPanel(
            "Coherence",
            shiny::sidebarPanel(
              width = 300,
              helpText(h3("Candidate number of topics k")),
              div(
                style = "display: inline-block;vertical-align:top; width: 120px;",
                numericInput(
                  inputId = "num1",
                  label = "from",
                  value = 2,
                  min = 2
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 120px;",
                numericInput(
                  inputId = "num2",
                  label = "to",
                  value = 2,
                  min = 2
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 110px;",
                numericInput(
                  inputId = "num3",
                  label = "by",
                  value = 1,
                  min = 1
                )
              ),
              textInput(
                inputId = "OtherKCoherence",
                label = "Others K",
                value = paste(exp.stop, collapse = ", "),
                placeholder = "Enter values separated by a comma...50, 100, 150,..."
              ),
              textOutput("OthKcoh"),
              br(),
              helpText(h3("Parameters control Gibbs sampling")),
              div(
                style = "display: inline-block;vertical-align:top; width: 110px;",
                numericInput(
                  inputId = "num4",
                  label = "Iteractions",
                  value = 10,
                  min = 10
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 110px;",
                numericInput(
                  inputId = "num5",
                  label = "Burn-in",
                  value = 5,
                  min = 5
                )
              ),
              shinyBS::bsPopover(
                id = "num5",
                "Specifies how many iterations are discarded If is too low,the Gibbs sample will be polluted from the wrong distribution.If is too large, the only penalty is wasting computational effort",
                options = list(container = "body")
              ),
              br(),
              helpText(h3("Hyper-parameter")),
              withMathJax(),
              div(
                style = "display: inline-block;vertical-align:top; width: 110px;",
                numericInput(
                  inputId = "num6",
                  label = h4("\\( \\alpha \\)"),
                  value = 0.1,
                  min = 0.01,
                  max = 1,
                  step = 0.01
                )
              ),
              br(),
              shinyBS::bsPopover("num6",
                                 "Assuming symmetric Dirichlet distributions (for simplicity), a low alpha value places more weight on having each document composed of only a few dominant topics",
                                 options = list(container = "body")
              ),
              br(),
              shinyalert::useShinyalert(),
              div(
                id = "Run1",
                style = "display:inline-block",
                shinyWidgets::actionBttn(
                  inputId = "Run.model1",
                  label = "Run",
                  style = "float",
                  block = TRUE,
                  color = "primary"
                )
              ),
              bsModalNoClose(
                id = "Coherence",
                title = "Coherence",
                trigger = "Run1", size = "large",
                verbatimTextOutput("timeCoherence"),
                shinycssloaders::withSpinner(highcharter::highchartOutput("plot_gi"))
              )
            )
          ),
          tabPanel(
            "4-metrics",
            shiny::sidebarPanel(
              width = 300,
              helpText(h3("Candidate number of topics k")),
              div(
                style = "display: inline-block;vertical-align:top; width: 120px;",
                numericInput(
                  inputId = "num7",
                  label = "from",
                  value = 2,
                  min = 2
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 120px;",
                numericInput(
                  inputId = "num8",
                  label = "to",
                  value = 2,
                  min = 2
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 110px;",
                numericInput(
                  inputId = "num9",
                  label = "by",
                  value = 1,
                  min = 1
                )
              ),
              textInput(
                inputId = "OtherK4metric",
                label = "Others K",
                value = paste(exp.stop, collapse = ", "),
                placeholder = "Enter values separated by a comma...50, 100, 150,..."
              ),
              textOutput("OthK4metric"),
              br(),
              br(),
              awesomeRadio(
                inputId = "methods",
                label = helpText(h3("Estimation method")),
                choices = c("Gibbs", "VEM"),
                selected = "Gibbs"
              ),
              # checkboxGroupButtons(
              # inputId = "metric",
              # label = helpText(h3("Metrics")),
              # choices = c("Griffiths2004",
              # "CaoJuan2009",
              # "Arun2010",
              # "Deveaud2014"),
              # individual = TRUE,
              # checkIcon = list(
              # yes = tags$i(class = "fa fa-circle",
              #  style = "color: steelblue"),
              # no = tags$i(class = "fa fa-circle-o",
              # style = "color: steelblue"))
              # ),
              br(),
              shinyalert::useShinyalert(),
              div(
                id = "Run2",
                style = "display:inline-block",
                actionBttn(
                  inputId = "Run.model2",
                  label = "Run",
                  style = "float",
                  block = TRUE,
                  color = "primary"
                )
              ),
              bsModalNoClose(
                id = "fourmetric",
                title = "Four metrics",
                trigger = "Run2",
                size = "large",
                verbatimTextOutput("timefourmetric"),
                withSpinner(highcharter::highchartOutput("plot_gj"))
              )
            )
          ),
          tabPanel(
            "Perplexity",
            sidebarPanel(
              width = 300,
              helpText(h3("Candidate number of topics k")),
              div(
                style = "display: inline-block;vertical-align:top; width: 120px;",
                numericInput(
                  inputId = "num13",
                  label = "from",
                  value = 2,
                  min = 2
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 120px;",
                numericInput(
                  inputId = "num14",
                  label = "to",
                  value = 5,
                  min = 2
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 110px;",
                numericInput(
                  inputId = "num15",
                  label = "by",
                  value = 2,
                  min = 1
                )
              ),
              textInput(
                inputId = "OtherKLL",
                label = "Others K",
                value = paste(exp.stop,
                              collapse = ", "
                ),
                placeholder = "Enter values separated by a comma...50, 100, 150,..."
              ),
              textOutput("OthKLL"),
              helpText(h3("Parameters control Gibbs sampling")),
              div(
                style = "display: inline-block;vertical-align:top; width: 110px;",
                numericInput(
                  inputId = "num16",
                  label = "Iteractions",
                  value = 10,
                  min = 10
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 110px;",
                numericInput(
                  inputId = "num17",
                  label = "Burn-in",
                  value = 5,
                  min = 5
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 110px;",
                numericInput(
                  inputId = "num18",
                  label = "Thin",
                  value = 3,
                  min = 3
                )
              ),
              br(),
              br(),
              shinyalert::useShinyalert(),
              div(
                id = "Run3", style = "display:inline-block",
                actionBttn("Run.model3", "Run",
                           style = "float",
                           block = TRUE,
                           color = "primary"
                )
              ),
              bsModalNoClose(
                id = "perplex",
                title = "Perplexity",
                trigger = "Run3",
                size = "large",
                verbatimTextOutput("timeloglike"),
                withSpinner(highcharter::highchartOutput("plot_gk"))
              )
            )
          ),
          tabPanel(
            "Harmonic mean",
            sidebarPanel(
              width = 300,
              helpText(h3("Candidate number of topics k")),
              div(
                style = "display: inline-block;vertical-align:top; width: 120px;",
                numericInput("num19",
                             label = "from",
                             value = 2,
                             min = 2
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 120px;",
                numericInput("num20",
                             label = "to",
                             value = 5,
                             min = 5
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 110px;",
                numericInput("num21",
                             label = "by",
                             value = 1,
                             min = 1
                )
              ),
              textInput("OtherKHM",
                        label = "Others K",
                        value = paste(exp.stop, collapse = ", "),
                        placeholder = "Enter values separated by a comma...50, 100, 150,..."
              ),
              textOutput("Okhm"),
              helpText(h3("Parameters control Gibbs sampling")),
              div(
                style = "display: inline-block;vertical-align:top; width: 110px;",
                numericInput("num22",
                             label = "Iteractions",
                             value = 10,
                             min = 10
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 110px;",
                numericInput("num23",
                             label = "Burn-in",
                             value = 5,
                             min = 5
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 110px;",
                numericInput("num24",
                             label = "Keep",
                             value = 2,
                             min = 2
                )
              ),
              br(),
              br(),
              shinyalert::useShinyalert(),
              div(
                id = "Run4", style = "display:inline-block",
                actionBttn("Run.model4",
                           "Run",
                           style = "float",
                           block = TRUE,
                           color = "primary"
                )
              ),
              bsModalNoClose(
                id = "harmonic",
                title = "Harmonic mean",
                trigger = "Run4",
                size = "large", verbatimTextOutput("timeHmean"),
                withSpinner(highcharter::highchartOutput("plot_gl"))
              )
            )
          )
        )
      )
    ),

    shinydashboard::tabItem(
      tabName = "tab41",
      shinydashboard::box(
        width = 400,
        title = "4. LDA model",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        sidebarPanel(
          width = 400,
          helpText(h3("k parameter (topic) ")),
          div(
            style = "display: inline-block;vertical-align:top; width: 120px;",
            numericInput("num25",
                         label = "",
                         value = 2,
                         min = 2
            )
          ),
          br(),
          helpText(h3("Iterations")),
          div(
            style = "display: inline-block;vertical-align:top; width: 110px;",
            numericInput("num26",
                         label = "Iteractions",
                         value = 10,
                         min = 10
            )
          ),
          div(
            style = "display: inline-block;vertical-align:top; width: 110px;",
            numericInput("num27",
                         label = "Burn-in",
                         value = 5,
                         min = 5
            )
          ),
          shinyBS::bsPopover("num9",
                             "Specifies how many iterations are discarded. If is too low,the Gibbs sample will be polluted from the wrong distribution.If is too large, the only penalty is wasting computational effort",
                             options = list(container = "body")
          ),
          br(), helpText(h3("Hyper-parameter")),
          withMathJax(),
          div(
            style = "display: inline-block;vertical-align:top; width: 110px;",
            numericInput("num28",
                         label = h4("\\( \\alpha \\)"),
                         value = 0.1,
                         min = 0.01,
                         max = 1,
                         step = 0.01
            )
          ),
          br(),
          div(
            id = "Run5",
            style = "display:inline-block",
            actionBttn("Run.model5",
                       "Run LDA Model",
                       style = "float",
                       block = TRUE,
                       color = "primary"
            )
          ),
          bsModalNoClose(
            id = "ldasummary",
            title = "summary LDA model",
            trigger = "Run5",
            size = "large",
            # verbatimTextOutput("r2"),
            withSpinner(DT::DTOutput("sum"))
          )
        )
      )
    ),
    shinydashboard::tabItem(
      tabName = "tab42",
      shinydashboard::box(
        width = 400,
        title = "Download tabular results",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        div(
          id = "data_theta", style = "display:inline-block",
          shinyWidgets::actionBttn("datatheta",
                                   "theta",
                                   icon = icon("table"),
                                   style = "float", block = TRUE, color = "primary"
          )
        ),
        bsModalNoClose(
          id = "datathet",
          title = "theta",
          trigger = "data_theta",
          size = "large",
          shinycssloaders::withSpinner(DT::DTOutput("theta"))
        ),
        div(
          id = "data_phi",
          style = "display:inline-block",
          shinyWidgets::actionBttn("dataphi",
                                   "phi",
                                   icon = icon("table"), style = "float",
                                   block = TRUE, color = "primary"
          )
        ),
        bsModalNoClose(
          id = "datphi",
          title = "phi",
          trigger = "data_phi",
          size = "large",
          shinycssloaders::withSpinner(DT::DTOutput("phi"))
        ),
        div(
          id = "data_reg",
          style = "display:inline-block",
          shinyWidgets::actionBttn("reg",
                                   "trend",
                                   icon = icon("table"),
                                   style = "float",
                                   block = TRUE, color = "primary"
          )
        ),
        bsModalNoClose(
          id = "datareg",
          title = "Regression summary",
          trigger = "data_reg",
          size = "large",
          shinycssloaders::withSpinner(DT::DTOutput("reg"))
        ),
        div(
          id = "summ_LDA",
          style = "display:inline-block",
          shinyWidgets::actionBttn("sumLDA",
                                   "Summary LDA",
                                   icon = icon("table"),
                                   style = "float",
                                   block = TRUE,
                                   color = "primary"
          )
        ),
        bsModalNoClose(
          id = "SummLDA",
          title = "Summary",
          trigger = "summ_LDA",
          size = "large",
          sliderInput(
            inputId = "Topterm",
            label = "Select number of term",
            min = 1,
            max = 50,
            value = 10,
            step = 1
          ),
          sliderInput(
            inputId = "Labels",
            label = "Select number of label",
            min = 1,
            max = 5,
            value = 1,
            step = 1
          ),
          sliderInput(
            inputId = "assignments",
            label = "Select assignments",
            min = 0,
            max = 1,
            value = 0.05,
            step = 0.01
          ),
          shinycssloaders::withSpinner(DT::DTOutput("summLDA"))
        ),

        div(
          id = "Allocat", style = "display:inline-block",
          shinyWidgets::actionBttn("alloca", "Allocation", icon = icon("table"), style = "float", block = TRUE, color = "primary")
        ),

        bsModalNoClose(
          id = "allocat", title = "Allocation of document to topics", trigger = "Allocat", size = "large",
          sliderInput(inputId = "topnumber", label = "top number", min = 1, max = 50, value = 1),
          shinycssloaders::withSpinner(DT::DTOutput("Alloca"))
        )
      )
    ),
    shinydashboard::tabItem(
      tabName = "tab43",
      shinydashboard::box(
        width = 400,
        title = "Download graphics results",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        div(
          id = "Trend",
          style = "display:inline-block",
          shinyWidgets::actionBttn("t_trend",
                                   "trend",
                                   style = "float",
                                   block = TRUE,
                                   color = "primary",
                                   icon = icon("bar-chart")
          )
        ),
        bsModalNoClose(
          id = "Trend_bs",
          title = "Trend of topic",
          trigger = "Trend",
          size = "large",
          shinycssloaders::withSpinner(highcharter::highchartOutput("plot_trend"))
        ),
        div(
          id = "plot_cloud", style = "display:inline-block",
          actionBttn("plotcloud",
                     "View wordcloud by topic",
                     icon = icon("bar-chart"),
                     style = "float",
                     block = TRUE,
                     color = "primary"
          )
        ),
        bsModalNoClose(
          id = "plotworcloud",
          title = "Wordcloud",
          trigger = "plot_cloud",
          size = "large",
          div(style = "display: inline-block;vertical-align:top; width: 120px;", tags$h5("Select options")),
          div(
            style = "display: inline-block;vertical-align:top; width: 120px;",
            shinyWidgets::dropdown(numericInput("num29",
                                                label = "topic #",
                                                value = 1,
                                                min = 1
            ),
            sliderInput(
              inputId = "cloud",
              label = "Select number of word",
              min = 5,
              max = 100,
              value = 10
            ),
            style = "unite",
            icon = icon("gear"),
            status = "danger",
            width = "500px",
            animate = shinyWidgets::animateOptions(
              enter = animations$fading_entrances$fadeInLeftBig,
              exit = animations$fading_exits$fadeOutRightBig
            )
            )
          ),
          shinycssloaders::withSpinner(highcharter::highchartOutput("plot_worcloud"))
        ),
        div(
          id = "heatmap",
          style = "display:inline-block",
          shinyWidgets::actionBttn("Heat",
                                   "heat_map",
                                   style = "float",
                                   block = TRUE,
                                   color = "primary",
                                   icon = icon("bar-chart")
          )
        ),

        bsModalNoClose(
          id = "Heat_map",
          title = "Heatmap",
          trigger = "heatmap",
          size = "large",
          shinycssloaders::withSpinner(highcharter::highchartOutput("plot_heatmap"))
        )
      )
    ),

    shinydashboard::tabItem(
      tabName = "tab0",
      shiny::fluidPage(tags$iframe(
        src = "about.html",
        width = "100%",
        height = "1000px",
        frameborder = 0,
        scrolling = "auto"
      ))
    ),
    shinydashboard::tabItem(
      tabName = "tab5",
      shiny::fluidPage(shinydashboard::tabBox(
        title = "",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "1000px", width = "100%",
        tabPanel("English", tags$iframe(
          src = "A_brief_introduction_to_LDAShiny.html",
          width = "100%",
          height = "1000px",
          frameborder = 0,
          scrolling = "auto"
        )),
        tabPanel("Español", tags$iframe(
          src = "Una_breve_introducci-n_a_LDAShiny.html",
          width = "100%",
          height = "1000px",
          frameborder = 0,
          scrolling = "auto"
        ))
      ) )
    )
  )
)

)##final
