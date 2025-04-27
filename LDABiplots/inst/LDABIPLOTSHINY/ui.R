library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(shinybusy)
library(shinyalert)
library(rvest)
library(dplyr)
library(DT)
library(highcharter)
library(SnowballC)
library(ldatuning)
library(topicmodels)
library(textmineR)
library(chinese.misc)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(textplot)
library(glasso)
library(qgraph)
library(Matrix)
library(utils)
library(factoextra)
library(shinyBS)
library(quanteda)
library(readxl)


exp.stop <- c()
################################Header##########################################
header <- shinydashboard::dashboardHeader(
  title = "LDABiplots",
  titleWidth = 400,
  tags$li(
    class = "dropdown",
    actionLink("stop_radiant",
               "Stop",
               icon = icon("power-off"),
               onclick = "setTimeout(function(){window.close();}, 100); ")
  )
)
###############################Sidebar##########################################
sidebar <- shinydashboard::dashboardSidebar(
  width = 400,
  shinydashboard::sidebarMenu(
    shinydashboard::menuItem(
      "About ",
      tabName = "tab0"
      ),
    shinydashboard::menuItem(
      "Import or Load Data ",
      tabName = "tab1",
      icon = icon("file-import")
      ),
    shinydashboard::menuItem(
      "Document Term Matrix Visualizations",
      tabName = "tab2",
      icon = icon("chart-bar")
    ),
    shinydashboard::menuItem(
      "Inference (Number of topic)",
      tabName = "tab3",
      icon = icon("chart-bar")
    ),
    shinydashboard::menuItem("LDA and Biplot",
                             tabName = "tab4",
                             icon = icon("desktop"),
                             shinydashboard::menuSubItem("LDA",
                                                         tabName = "tab41"
                             ),
                             shinydashboard::menuSubItem("Biplot",
                                                         tabName = "tab42"
                             )

    )
  )
  )


###############################Body##########################################
body <- shinydashboard::dashboardBody(
  shinybusy::add_busy_spinner(spin = "breeding-rhombus",
                              height = "200px",
                              width = "200px",
                              color = "blue",
                              position ="top-right"
                             ),
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "custom.css"
    )
  ),
  shinydashboard::tabItems(
    shinydashboard::tabItem(
      tabName = "tab0",
      shiny::fluidPage(tags$iframe(
        src = "LDABIPLOT.html",
        width = "100%",
        height = "1000px",
        frameborder = 0,
        scrolling = "auto"
      ))),
    shinydashboard::tabItem (tabName = "tab1",
      shinydashboard::box(width = 12,
                          title = "Import or Load Data and Preprocessing",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          sidebarPanel(
                            selectInput(
                              "load",
                              label = "Please, choose what to do",
                              choices = c(
                                " " = "null",
                                "Import excel file" = "import",
                                "Load web data" = "load"
                                #"Use a example" = "demo"
                              ),
                              selected = "null"
                            ),
                            conditionalPanel(
                              condition = "input.load == 'import'",
                              fileInput("file1",
                                        "Upload the file",
                                        accept = c(".xlsx", ".xls"),
                                        multiple = FALSE),
                              selectInput(inputId = "worksheet",
                                          label="Worksheet Name",
                                          choices=NULL
                              ),
                              actionButton(inputId = "getData",
                                             label="Get data"),
                              conditionalPanel(condition = "input.getData != 0",
                               helpText(h3("Preprocessing")),
                              shinyWidgets::awesomeRadio(
                                inputId = "ngramsxl",
                                label = "ngrams",
                                choices = c("Unigrams" = 1L, "Bigrams" = 2L,
                                            "Trigrams" = 3L),
                                selected = 1L,
                                status = "primary"
                              ),
                              shinyWidgets::awesomeCheckbox(
                                inputId = "removenumberxl",
                                label = "Remove number",
                                value = TRUE,
                                status = "danger"
                              ),
                              shinyWidgets::pickerInput(
                                inputId = "Languagexl",
                                label = "Select language for stopword",
                                choices = c( "en","es"
                                ),
                                choicesOpt = list(
                                  subtext = paste(Languages <- c(
                                    "english",
                                    "spanish"))
                                )
                              ),
                        textInput("stopwordsxl",
                                      label = "Stop Words",
                                      value = paste(exp.stop, collapse = ", "),
                                      placeholder = "also, such, really..."
                              ),
                        shinyBS::bsPopover("stopwordsxl",
                                              "Include additional stop words
                                           to remove(words
                                        must be separated by commas)",
                                        placement = "right"),
                                        #options = list(container = "body")
br(),
div(style = "display: inline-block;vertical-align:top; width: 170px;",
                                shinyWidgets::awesomeCheckbox(
                                  inputId = "checkStemmingxl",
                                  label = "Stemming",
                                  value = FALSE,
                                  status = "danger"
                                )
                              ),
shinyBS::bsPopover("checkStemmingxl",
                   "Click if you want to stemming",
                   options = list(container = "body")
                              ),
conditionalPanel(condition = "input.checkStemmingxl != 0",
                 div(
                   style = "display: inline-block;vertical-align:top; width: 200px;",
                   selectInput("Stemmxl", " Stemming langauge",
                                                             choices = c(
                                                               "english","spanish")
                                                 )
                                               )),
                              br(),
                              sliderInput(
                                inputId = "sparcexl",
                                h4(
                                  span("Sparcity :"),
                                  span(icon("info-circle"), id = "sparcexl",
                                       style = "color: blue")),
                                min = 0.0,
                                max = 0.999,
                                value = 0.999,
                                step = 0.005
                              ),
                              bsPopover("sparce", "Sparcity","remove terms less
                                        than selected threshold",
                                        placement = "bottom", trigger = "click"),
                              div(
                                id = "dTmxl",
                                style = "display:inline-block",
                                shinyalert::useShinyalert(),
                                shinyWidgets::actionBttn(
                                  inputId = "dtmexcel.update",
                                  label = "Create DTM",
                                  style = "float",
                                  block = TRUE,
                                  color = "primary"
                                )
                              ),
                              bsModalNoClose(
                                id = "DTMdimxl",
                                title = "dim DTM",
                                trigger = "dTmxl",
                                size = "large",
                                shinycssloaders::withSpinner(
                                  DT::DTOutput("Table_dimxl"))
                              )
                              )),
                            conditionalPanel(
                              condition = "input.load == 'load'",
                            prettyRadioButtons(
                            inputId = "language",
                            label = "Choose language:",
                            choices = c("spanish","english")
                          ),
                            textInput("search",
                                      label = "Search",
                                      value = paste(exp.stop, collapse = "+"),
                                                 placeholder = "also, such,..."
                          ),useShinyalert(),
                          numericInput(inputId = "numberpag",
                                       "Number of pages",
                                       5,
                                       min = 1,
                                       max = 100,
                                       ),
                          shinyWidgets::actionBttn(
                            inputId = "runsearch",
                            label = "Run",
                            style = "float",
                            block = TRUE,
                            color = "primary"
                          ),
                          br(),
                  shinyjs::useShinyjs(),
                  conditionalPanel(condition = "input.runsearch != 0",
                                   selectizeInput(
                                     inputId = "selectnews",
                                     label = "Select/deselect all options",
                                     choices = NULL,
                                     multiple = TRUE
                                   ),
                          br(),
                          helpText(h3("Preprocessing")),
                          shinyWidgets::awesomeRadio(
                            inputId = "ngrams",
                            label = "ngrams",
                            choices = c("Unigrams" = 1L, "Bigrams" = 2L,
                                        "Trigrams" = 3L),
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
                            choices = c( "en","es"
                            ),
                            choicesOpt = list(
                              subtext = paste(Languages <- c(
                                "english",
                                "spanish"))
                            )
                          ),
                          textInput("stopwords",
                                    label = "Stop Words",
                                    value = paste(exp.stop, collapse = ", "),
                                    placeholder = "also, such, really..."
                          ),
                          shinyBS::bsPopover("stopwords",
                                             "Include additional stop words
                                             to remove(words
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
                          shinyBS::bsPopover("checkStemming",
                                             "Click if you want to stemming",
                                             options = list(container = "body")
                          ),
                          conditionalPanel(
                            condition = "input.checkStemming != 0",
                          div(
                            style = "display: inline-block;vertical-align:top; width: 200px;",
                            selectInput("Stemm", " Stemming langauge",
                                        choices = c(
                                          "english","spanish")
                            )
                          )),
                          br(),
                          sliderInput(
                            inputId = "sparce",
                            h4(
                              span("Sparcity :"),
                              span(icon("info-circle"),
                                   id = "sparce", style = "color: blue")),
                            min = 0.00,
                            max = 0.99,
                            value = 0.95,
                            step = 0.005
                          ),
                          bsPopover("sparce", "Sparcity","remove terms less
                                        than selected threshold",
                                    placement = "bottom", trigger = "click"),
                          br(),
                          div(
                            id = "dTm",
                            style = "display:inline-block",
                            shinyalert::useShinyalert(),
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
                            shinycssloaders::withSpinner(
                              DT::DTOutput("Table_dim"))
                          )

                          ))# fincoditionalpanel
                          ),

                          mainPanel(helpText(h3("Frecuency by Newspapers")),
                            DT::DTOutput("sum"),
                            br(),
                            helpText(h3("news by newspapers")),
                            DT::DTOutput("text")
                          ))),
    shinydashboard::tabItem(
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
            label = "Data",
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
            label = "Barplot",
            icon = icon("chart-bar"),
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
            label = "Wordcloud",
            icon = icon("chart-bar"),
            style = "float",
            block = TRUE,
            color = "primary"
          )
        ),
        div(
          id = "plot_d",
          style = "display:inline-block",
          shinyWidgets::actionBttn(
            inputId = "plot3",
            label = "Co-ocurrence",
            icon = icon("chart-bar"),
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
            tags$h5("Customize bar plot")
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
            textInput("colorbar",
                      label = "Bar color",
                      value = "gray"),
            shinyBS::bsPopover("colorbar",
                               "Write the color of your preference.
                               (for Example, red or #5faeb3 )",
                               options = list(container = "body")),
            style = "unite",
            #icon = icon("gear"),
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
            #icon = icon("gear"),
            status = "danger",
            width = "500px",
            animate = shinyWidgets::animateOptions(
              enter = animations$fading_entrances$fadeInLeftBig,
              exit = animations$fading_exits$fadeOutRightBig
            )
            )
          ),
          shinycssloaders::withSpinner(highcharter::highchartOutput("plot_gf"))
        ),
        bsModalNoClose(
          id = "Plot3",
          title = "Co-ocurrence",
          trigger = "plot3",
          size = "large",
          div(
            style = "display: inline-block;vertical-align:top; width: 120px;",
            tags$h5("Select number of term and download plot")
          ),
          div(
            style = "display: inline-block;vertical-align:top; width: 120px;",
            shinyWidgets::dropdown(sliderInput(
              inputId = "d",
              label = "Select number of word",
              min = 2,
              max = 1000,
              value = 25
            ),
            radioButtons("downplotco",
                         "Select the Option",
                         choices = list("png","pdf")),
            downloadButton("downplotco",
                           label = "Download the plot"),
            style = "unite",
            #icon = icon("gear"),
            status = "danger",
            width = "500px",
            animate = shinyWidgets::animateOptions(
              enter = animations$fading_entrances$fadeInLeftBig,
              exit = animations$fading_exits$fadeOutRightBig
            )
            )
          ),
          shinycssloaders::withSpinner(plotOutput("plotcoc"))
        )
      )
    ),
    shinydashboard::tabItem(
      tabName = "tab3",
      shinydashboard::box(
        width = 12,
        title = " Inference (Number of topic)",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        sidebarPanel(helpText(
          h3("Candidate number of topics k")),
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
                         h4(
                           span("Iteration :"),
                           span(icon("info-circle"), id = "iterationnt",
                                style = "color: blue")),
                         value = 10,
                         min = 10
                       ),
                       shinyBS::bsPopover(
                         id = "iterationnt","Iterations",
                         "Specifies number of iterations for Gibbs Sampling",
                         options = list(container = "body")
                       )
                     ),
                     div(
                       style = "display: inline-block;vertical-align:top; width: 150px;",
                       numericInput(
                         inputId = "num5",
                         h4(
                           span("Burn-in :"),
                           span(icon("info-circle"), id = "burninnt",
                                style = "color: blue")),
                         value = 5,
                         min = 5
                       ),
                       shinyBS::bsPopover(
                         id = "burninnt","Burn-in",
                         "Specifies how many iterations are discarded.",
                         options = list(container = "body")
                       )
                     ),

                     br(),
                     helpText(h3("Hyper-parameter")),
                     withMathJax(),
                     div(
                       style = "display: inline-block;vertical-align:top; width: 110px;",
                       numericInput(
                         inputId = "num6",
                         h4(
                           span("\\( \\alpha \\)"),
                           span(icon("info-circle"), id = "hyperalpha",
                                style = "color: blue")),
                         value = 0.1,
                         min = 0.01,
                         max = 1,
                         step = 0.01
                       ),
                       shinyBS::bsPopover(
                         id = "hyperalpha","alpha",
                         "α=(0.1,50/K);(0.1,0.1);(1/K,1/K)",
                         options = list(container = "body")
                       )
                     ),
                     br(),
                     shinyBS::bsPopover("num6",
                                        "Assuming symmetric Dirichlet
                                        distributions (for simplicity),
                                        a low alpha value places more weight
                                        on having each document composed
                                        of only a few dominant topics",
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
                     )),
        mainPanel(verbatimTextOutput("timeCoherence"),
          highcharter::highchartOutput("plot_gi")))
    ),
    shinydashboard::tabItem(
      tabName = "tab41",
      shinydashboard::box(
        width = 12,
        title = "LDA model",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        sidebarPanel(
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
          numericInput("num26",
                       h4(
                         span("Iteration :"),
                         span(icon("info-circle"), id = "iterationmodel",
                              style = "color: blue")),

                       value = 10,
                         min = 10
            ),
          shinyBS::bsPopover("iterationmodel","Iteration",
                             "Specifies number of iterations for Gibbs Sampling.",
                             options = list(container = "body")
          ),
          numericInput("num27",
                       h4(span("Burn-in :"),
                         span(icon("info-circle"), id = "burninmodel",
                              style = "color: blue")),
                         value = 5,
                         min = 5
            ),
          shinyBS::bsPopover("burninmodel","Burn-in",
                             "Specifies how many iterations are discarded.",
                             options = list(container = "body")
          ),
          br(), helpText(h3("Hyper-parameter")),
          withMathJax(),
          numericInput("num28",
                       h4(span("\\( \\alpha \\)"),
                         span(icon("info-circle"),
                              id = "hyper", style = "color: blue")),
                       value = 0.1,
                         min = 0.01,
                         max = 1,
                         step = 0.01
            ),
          shinyBS::bsPopover(
            id = "hyper","alpha ",
                         "α=(0.1,50/K);(0.1,0.1);(1/K,1/K)",
            options = list(container = "body")
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
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Tabular result",
                     helpText(h3("Summary Model LDA")),
                     br(),
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
                     verticalLayout(
                       DT::DTOutput("summLDA"),
                       helpText(h3("Theta Matrix")),
                       DT::DTOutput("theta"),
                       br(),
                       helpText(h3("Phi Matrix")),
                       DT::DTOutput("phi")
                       )),
              tabPanel("Worcloud by topic",
                     div(
              style = "display: inline-block;vertical-align:top; width: 120px;",
              shinyWidgets::dropdown(
                numericInput("num29",
                             label = "topic #",
                             value = 1,
                             min = 1),
                div(
                  style = "display: inline-block;vertical-align:top; width: 120px;",
                  sliderInput(
                  inputId = "cloud",
                  label = "Select number of word",
                  min = 5,
                  max = 100,
                  value = 10)),
                style = "unite",
                #icon = icon("gear"),
                status = "danger",
                width = "500px",
                tooltip = tooltipOptions(
                  title = "Click to see inputs!"),
                animate = animateOptions(
                  enter = animations$fading_entrances$fadeInLeftBig,
                  exit = animations$fading_exits$fadeOutRightBig
                )
              )
            ),
            highcharter::highchartOutput("plot_worcloud"),
                     ),
            tabPanel("Heatmap",
                     highcharter::highchartOutput("plot_heatmap")),
            tabPanel("Cluster",#plotOutput("num_cluster"),
                     br(),
                     pickerInput(
                       inputId = "methodaglo",
                       label = "Agglomeration method",
                       choices = c("complete", "single", "ward.D",
                                   "ward.D2", "average", "mcquitty","median",
                                   "centroid")
                     ), helpText(h4("Cophenetic correlation ")),
                     verbatimTextOutput("cophenetic"),
                     br(),
                     sliderInput(
                       inputId = "numberclu",
                       label = "Select k (number cluster)",
                       min = 2,
                       max = 50,
                       value = 5),
                     br(),
                     pickerInput(
                       inputId = "type",
                       label = "type of plot",
                       choices = c("rectangle",
                                   "circular", "phylogenic")
                     ),
                     sliderInput(
                       inputId = "numM",
                       label = "Select (number term)",
                       min = 2,
                       max = 20,
                       value = 3),
                     br(),
                     plotOutput("plot_cluster"),
                     br(),
                     br(),
                     radioButtons("butdowncluster",
                                  "Select the Option",
                                  choices = list("png","pdf")),
                     downloadButton("downcluster",
                                    label = "Download the plot")
                    )

        ))#fin mainpanel
      )

      ),
    tabItem(tabName = "tab42",shinydashboard::box(
      width = 12,
      title = " Biplot",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      sidebarPanel(
        radioGroupButtons(
          inputId = "selectypebiplot",
          label = "Biplot",
          choices = c("HJ_Biplot",
                     "JK_Biplot", "GH_Biplot"),
          individual = TRUE,
          checkIcon = list(
            yes = tags$i(class = "fa fa-circle",
                        style = "color: steelblue"),
           no = tags$i(class = "fa fa-circle-o",
                       style = "color: steelblue"))
        ),
        awesomeRadio(
        inputId = "biptransf",
        label = "Transform.Data",
        choices = c("scale", "center","center_scale","none" ),
        selected = "scale",
        inline = TRUE,
        checkbox = TRUE
      ),
      shinyWidgets::actionBttn(
        inputId = "runbiplot",
        label = "Run",
        style = "float",
        block = TRUE,
        color = "primary"),
      conditionalPanel(
        condition = "input.runbiplot != 0" ,
        helpText(h3("Options to Customize the biplot")),
      shinyWidgets::awesomeRadio(
        inputId = "biphide",
        label = "Hide",
        choices = c("none", "ind", "var"),
        selected = "none",
        inline = TRUE,
        checkbox = TRUE
      ),
      selectInput("themebi", " Theme",
                 choices = names(themesb)
     ),
      numericInput(
        inputId = "axisx",
        label = "Axis-X",
        value = 1,
        min = 1
      ),
      numericInput("axisy",
                   label = "Axis-Y",
                   value = 2,
                   min = 1),
      textInput("colorind",
                label = "Color ind",
                value = "#d92d5e"),
      textInput("colorvar",
                label = "Color var",
                value = "black"),
      div(
        style = "display: inline-block;vertical-align:top; width: 70px;",
      switchInput(
        inputId = "indlabel",
        label = "Label ind",
        onLabel = "Yes",
        offLabel = "No",
        size = "mini")),
      div(
        style = "display: inline-block;vertical-align:top; width: 70px;",
      switchInput(
        inputId = "varlabel",
        label = "Label var",
        onLabel = "Yes",
        offLabel = "No",
        size = "mini")),
      div(
        style = "display: inline-block;vertical-align:top; width: 70px;",
      switchInput(
        inputId = "angle",
        label = "Angle label var",
        onLabel = "Yes",
        offLabel = "No",
        size = "mini")),
      sliderInput(
        inputId = "pch",
        label = " Point shape" ,
        min = 0  ,
        max =20 ,
        value = 19),
      sliderInput(inputId = "sizeind",
                  label = "Size ind" ,
                  min = 0.01  ,
                  max = 20 ,
                  step = 0.1,
                  value = 5),
      sliderInput(inputId = "labelsize",
                  label = "Label size ind" ,
                  min = 0.1  ,
                  max = 20,
                  step = 0.1,
                  value = 5),
     sliderInput(inputId = "sizeaxis",
                 label = "Size axis title " ,
                 min = 0.1  ,
                 max = 30,
                 step = 0.1,
                 value = 16),
     sliderInput(inputId = "axistest",
                 label = "Size axis text " ,
                 min = 0.1  ,
                 max = 30,
                 step = 0.1,
                 value = 16),
      sliderInput(
        inputId= "varsize",
        label = "Var size",
        min = 0.01,
        max = 20,
        value = 0.5,
        step = 0.1),
      sliderInput(inputId = "varlabelsize",
                  label = "Label size var" ,
                  min = 0.01  ,
                  max = 20 ,
                  value = 5,
                  step = 0.01))
 ),
      mainPanel(
        tabsetPanel(
          tabPanel("Tabular result",
                   helpText(h3("Eigenvalues")),
                   DT::DTOutput("eigen"),
                   br(),
                   helpText(h3("Variance explained")),
                   DT::DTOutput("variance"),
                   br(),
                   helpText(h3("Loading")),
                   DT::DTOutput("loading"),
                   br(),
                   helpText(h3("Coordinates of individuals")),
                   DT::DTOutput("indcoor"),
                   br(),
                   helpText(h3("Coordinates of variables")),
                   DT::DTOutput("varcoor")
                   ),
          tabPanel("Biplot",
                   plotOutput("biplot"),
                   br(),
                   radioButtons("butdown",
                                "Select the Option",
                                choices = list("png","pdf")),
                   downloadButton("down",
                                  label = "Download the plot")



      )
      )

    )
            )

  )
  )
  )

dashboardPage(header, sidebar, body)
