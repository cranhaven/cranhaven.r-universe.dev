library(shiny)
library(ReturnCurves)
library(evd)
library(ismev)
library(DT)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(shinydashboard)

source("plots_eda.R")
source("plots_margtransf.R")
source("plot_rc.R")
source("plot_adf.R")

ui <- dashboardPage(
  dashboardHeader(title = "Return Curves"),
  dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    tags$style(HTML('#margqmarg1-label ~ span span.irs-line { background: linear-gradient(to right, #ff0e0e 0%, #37b61e 100%);}
                     #margqmarg2-label ~ span span.irs-line { background: linear-gradient(to right, #ff0e0e 0%, #37b61e 100%);}
                     #margalpha-label ~ span span.irs-line { background: linear-gradient(to right, #37b61e 0%, #ff0e0e 100%);}
                     #rclengthw-label ~ span span.irs-line { background: #428bca;}
                     #rcqmarg1-label ~ span span.irs-line { background: linear-gradient(to right, #ff0e0e 0%, #37b61e 100%);}
                     #rcqmarg2-label ~ span span.irs-line { background: linear-gradient(to right, #ff0e0e 0%, #37b61e 100%);}
                     #rcq-label ~ span span.irs-line { background: linear-gradient(to right, #ff0e0e 0%, #37b61e 100%);}
                     #rcqalphas1-label ~ span span.irs-line { background: linear-gradient(to right, #ff0e0e 0%, #37b61e 100%);}
                     #rcqalphas2-label ~ span span.irs-line { background: linear-gradient(to right, #ff0e0e 0%, #37b61e 100%);}
                     #rcalpha-label ~ span span.irs-line { background: linear-gradient(to right, #37b61e 0%, #ff0e0e 100%);}
                     #rcgofalpha-label ~ span span.irs-line { background: linear-gradient(to right, #37b61e 0%, #ff0e0e 100%);}
                     #lengthw-label ~ span span.irs-line { background: #428bca;}
                     #qmarg1-label ~ span span.irs-line { background: linear-gradient(to right, #ff0e0e 0%, #37b61e 100%);}
                     #qmarg2-label ~ span span.irs-line { background: linear-gradient(to right, #ff0e0e 0%, #37b61e 100%);}
                     #q-label ~ span span.irs-line { background: linear-gradient(to right, #ff0e0e 0%, #37b61e 100%);}
                     #qalphas1-label ~ span span.irs-line { background: linear-gradient(to right, #ff0e0e 0%, #37b61e 100%);}
                     #qalphas2-label ~ span span.irs-line { background: linear-gradient(to right, #ff0e0e 0%, #37b61e 100%);}
                     #alpha-label ~ span span.irs-line { background: linear-gradient(to right, #37b61e 0%, #ff0e0e 100%);}
                     .irs--shiny .irs-bar { background: none;}
                     .shiny-output-error-validation { color: #ff0e0e;}
                    ')),
    sidebarMenu(
      fileInput("data", label = "File input", accept = c(".csv", ".rds", ".txt"), buttonLabel = "Browse...", placeholder = "No file selected"),
      uiOutput("select_column_x"),
      uiOutput("select_column_y"),
      menuItem("Exploratory Data Analysis",
               tabName = "eda", icon = icon("eye")),
      menuItem("Marginal Transformation",
               tabName = "margtransf", icon = icon("chart-column")),
      menuItem("Return Curve Estimation",
               tabName = "retcurve", icon = icon("chart-simple")),
      menuItem("Angular Dependence Function",
               tabName = "adf", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "eda",
              fluidRow(box(title = "Data", solidHeader = T, status = "info", collapsible = T, collapsed = T,
                           DTOutput("edatable"), width = 12)),
              fluidRow(box(title = "Histogram", solidHeader = T, status = "primary",
                           plotOutput("hist")),
                       box(title = "Autocorrelation", solidHeader = T, status = "primary",
                           plotOutput("acf"))),
              fluidRow(box(title = "Time series", solidHeader = T, status = "primary",
                           plotOutput("timeseries")),
                       box(title = "Joint distribution", solidHeader = T, status = "primary",
                           plotOutput("joint")))
      ),
      tabItem(tabName = "margtransf",
              fluidRow(
                box(title = "Marginal transformation inputs", solidHeader = T, status = "info", width = 12,
                    column(4,
                           sliderInput("margqmarg1", "Marginal quantile for the first variable",
                                       min = 0.01, max = 0.99, step = 0.01, value = 0.95)
                           ),
                    column(4,
                           sliderInput("margqmarg2", "Marginal quantile for the second variable",
                                       min = 0.01, max = 0.99, step = 0.01, value = 0.95)
                           ),
                    column(4,
                           radioButtons("margconstrainedshape", "Constrained the shape parameter of the GPD fit",
                                        choices = c(TRUE, FALSE))
                           )
                    ),
                box(title = "Histogram of transformed data", solidHeader = T, 
                          status = "primary",plotOutput("histexp")),
                box(title = "Joint distribution of transformed data", solidHeader = T, 
                    status = "primary", plotOutput("jointexp"))),
              fluidRow(box(title = "Marginal tail fits", solidHeader = T, 
                           status = "primary", width = 12, height = 570,
                           tagList(
                             withMathJax(),
                             column(4,
                                    numericInput("margblocksize", "Size of blocks for block bootstrap",
                                                 value = 1, min = 1, step = 1)
                                    ),
                             column(4,
                                    numericInput("margnboot", "Number of bootstrap samples",
                                                 value = 250, min = 1, step = 1)
                                    ),
                             column(4,
                                    sliderInput("margalpha", "Significance level for the \\((1-\\alpha)\\)% CI",
                                                min = 0.01, max = 0.99, step = 0.05, value = 0.05)
                                    )
                             ),
                           plotOutput("qqplots")))
              ),
      tabItem(tabName = "retcurve",
              withMathJax(),
              fluidRow(
                column(width = 12,
                       box(title = "Marginal transformation inputs", solidHeader = T, status = "info",
                           column(6,
                                  sliderInput("rcqmarg1", "Marginal quantile for the first variable",
                                              min = 0.01, max = 0.99, step = 0.01, value = 0.95)
                           ),
                           column(6,
                                  sliderInput("rcqmarg2", "Marginal quantile for the second variable",
                                              min = 0.01, max = 0.99, step = 0.01, value = 0.95)
                                  ),
                           hr(),
                           column(12,
                                  radioButtons("rcconstrainedshape", "Constrained the shape parameter of the GPD fit",
                                               choices = c(TRUE, FALSE), inline = T)
                                  )
                           
                           ),
                       box(title = "Inputs for \\(\\lambda(\\omega)\\)", solidHeader = T, status = "info",
                           column(6,
                                  sliderInput("rclengthw", "Number of rays \\(\\omega\\)",
                                              min = 51, max = 1001, value = 101, step = 50),
                                  
                                  sliderInput("rcq", "Quantile for \\(T_\\omega\\) and/or Hill estimator",
                                              min = 0.01, max = 0.99, step = 0.01, value = 0.95)
                                  ),
                           column(6,
                                  radioButtons("rcmethod", "Method to estimate \\(\\lambda(\\omega)\\)",
                                               choiceValues = list("hill", "cl"), choiceNames = list("Hill", "Composite Likelihood")),
                                  numericInput("rck", "Bernstein-Bezier polynomial degree", value = 7, min = 1)
                                  )
                           
                           )
                ),
                column(6,
                       box(title = "Return Curve Estimation", solidHeader = T, status = "primary",
                           plotOutput("rc"), width = 12)
                ),
                column(width = 6,
                       box(title = "Conditional extremes input", solidHeader = T, status = "info", 
                           radioButtons("rcconstrained", "Knowledge of the conditional extremes",
                                        choices = c(FALSE, TRUE), inline = T),
                           sliderInput("rcqalphas1", "Quantile used for the first variable",
                                       min = 0.01, max = 0.99, step = 0.01, value = 0.95),
                           sliderInput("rcqalphas2", "Quantile used for the second variable",
                                       min = 0.01, max = 0.99, step = 0.01, value = 0.95)),
                       box(title = "Return curve inputs", solidHeader = T, status = "info",
                           numericInput("probability", "Curve survival probability \\(p\\)", value = 0.001,
                                        min = 0, max = 1, step = 0.001)),
                       box(title = "Informed use only", status = "warning", solidHeader = T,
                           numericInput("rctol", "Convergence tolerance for the composite maximum likelihood procedure",
                                        value = 0.0001, min = 0),
                           numericInput("rcparinit", "Initial values for the parameters \\(\\beta\\)",value = 0))
                       
                )),
                fluidRow(
                  column(12,
                         box(title = "Uncertainty of Return Curve", solidHeader = T, status = "primary", 
                             collapsible = T, height = 750, collapsed = T,
                           actionButton("unc", "Uncertainty"),
                             uiOutput("uncertainty_inputs"),
                             plotOutput("rcunc")),
                         box(title = "Goodness-of-fit of Return Curve", solidHeader = T, status = "primary", 
                             collapsible = T, height = 750, collapsed = T,
                             actionButton("rcgof", "Goodness-of-fit"),
                             uiOutput("rcgof_inputs"),
                             plotOutput("rcgof")))
                )
      ),
      tabItem(tabName = "adf",
              withMathJax(),
              fluidRow(
                column(width = 12,
                       box(title = "Marginal transformation inputs", solidHeader = T, status = "info",
                           column(6,
                                  sliderInput("qmarg1", "Marginal quantile for the first variable",
                                              min = 0.01, max = 0.99, step = 0.01, value = 0.95)
                           ),
                           column(6,
                                  sliderInput("qmarg2", "Marginal quantile for the second variable",
                                              min = 0.01, max = 0.99, step = 0.01, value = 0.95)
                           ),
                           hr(),
                           column(12,
                                  radioButtons("constrainedshape", "Constrained the shape parameter of the GPD fit",
                                               choices = c(TRUE, FALSE), inline = T)
                           )
                           
                       ),
                       box(title = "Inputs for \\(\\lambda(\\omega)\\)", solidHeader = T, status = "info",
                           column(6,
                                  sliderInput("lengthw", "Number of rays \\(\\omega\\)",
                                              min = 51, max = 1001, value = 101, step = 50),
                                  
                                  sliderInput("q", "Quantile for \\(T_\\omega\\) and/or Hill estimator",
                                              min = 0.01, max = 0.99, step = 0.01, value = 0.95)
                           ),
                           column(6,
                                  radioButtons("method", "Method to estimate \\(\\lambda(\\omega)\\)",
                                               choiceValues = list("hill", "cl"), choiceNames = list("Hill", "Composite Likelihood")),
                                  numericInput("k", "Bernstein-Bezier polynomial degree", value = 7, min = 1)
                           )
                           
                       )
                ),
                column(6,
                       box(title = "Angular Dependence Function Estimation", solidHeader = T, status = "primary",
                           plotOutput("adfplot"), width = 12)
                ),
                column(width = 6,
                       box(title = "Conditional extremes input", solidHeader = T, status = "info", 
                           radioButtons("constrained", "Knowledge of the conditional extremes",
                                        choices = c(FALSE, TRUE), inline = T),
                           sliderInput("qalphas1", "Quantile used for the first variable",
                                       min = 0.01, max = 0.99, step = 0.01, value = 0.95),
                           sliderInput("qalphas2", "Quantile used for the second variable",
                                       min = 0.01, max = 0.99, step = 0.01, value = 0.95)),
                       box(title = "Informed use only", status = "warning", solidHeader = T,
                           numericInput("tol", "Convergence tolerance for the composite maximum likelihood procedure",
                                        value = 0.0001, min = 0),
                           numericInput("parinit", "Initial values for the parameters \\(\\beta\\)",value = 0))
                       
                ),
                column(12,
                  box(title = "Goodness-of-fit of the Angular Dependence Function", solidHeader = T, 
                      status = "primary", collapsible = T, width = 12, height = 750,
                      collapsed = T,
                      actionButton("adfgof", "Goodness-of-fit"),
                      uiOutput("adfgof_inputs"),
                      plotOutput("adfgof"))
                  )
                )
      )
    )
  )
)



server <- function(input, output, session) {
  data <- reactive({
    req(input$data)
    ext <- tools::file_ext(input$data$name)
    switch(ext,
           csv = read.csv(input$data$datapath),
           rds = readRDS(input$data$datapath),
           txt = read.table(input$data$datapath, header = TRUE, sep = ";", dec = "."),
           stop("Unsupported file type")
    )
  })
  
  observe({
    req(data())
    numeric_cols <- names(data())[sapply(data(), is.numeric)]
    updateSelectInput(session, "colX", choices = numeric_cols)
    updateSelectInput(session, "colY", choices = numeric_cols)
  })
  
  output$select_column_x <- renderUI({
    req(data())
    selectInput("colX", "Select first variable", choices = NULL)
  })
  
  output$select_column_y <- renderUI({
    req(data())
    selectInput("colY", "Select second variable", choices = NULL)
  })
  
  output$edatable <- renderDT({
    req(data())
  })
  
  output$hist <- renderPlot({
    req(data(), input$colX, input$colY)
    validate(
      need(input$colX %in% names(data()), "Select a valid first variable"),
      need(input$colY %in% names(data()), "Select a valid second variable"),
      need(is.numeric(data()[[input$colX]]), "First variable must be numeric"),
      need(is.numeric(data()[[input$colY]]), "Second variable must be numeric")
    )
    histplot(data(), input$colX, input$colY)
  })
  
  output$timeseries <- renderPlot({
    req(data(), input$colX, input$colY)
    validate(
      need(input$colX %in% names(data()), "Select a valid first variable"),
      need(input$colY %in% names(data()), "Select a valid second variable"),
      need(is.numeric(data()[[input$colX]]), "First variable must be numeric"),
      need(is.numeric(data()[[input$colY]]), "Second variable must be numeric")
    )
    timeseriesplot(data(), input$colX, input$colY)
  })
  
  output$acf <- renderPlot({
    req(data(), input$colX, input$colY)
    validate(
      need(input$colX %in% names(data()), "Select a valid first variable"),
      need(input$colY %in% names(data()), "Select a valid second variable"),
      need(is.numeric(data()[[input$colX]]), "First variable must be numeric"),
      need(is.numeric(data()[[input$colY]]), "Second variable must be numeric")
    )
    acfplot(data(), input$colX, input$colY)
  })
  
  output$joint <- renderPlot({
    req(data(), input$colX, input$colY)
    validate(
      need(input$colX %in% names(data()), "Select a valid first variable"),
      need(input$colY %in% names(data()), "Select a valid second variable"),
      need(is.numeric(data()[[input$colX]]), "First variable must be numeric"),
      need(is.numeric(data()[[input$colY]]), "Second variable must be numeric")
    )
    jointplot(data(), input$colX, input$colY)
  })
  
  output$histexp <- renderPlot({
    req(data(), input$colX, input$colY)
    validate(
      need(input$colX %in% names(data()), "Select a valid first variable"),
      need(input$colY %in% names(data()), "Select a valid second variable"),
      need(is.numeric(data()[[input$colX]]), "First variable must be numeric"),
      need(is.numeric(data()[[input$colY]]), "Second variable must be numeric")
    )
    histexpplot(data(), input$colX, input$colY, input$margqmarg1, input$margqmarg2, 
                input$margconstrainedshape)
  })
  
  output$qqplots <- renderPlot({
    req(data(), input$colX, input$colY)
    validate(
      need(input$colX %in% names(data()), "Select a valid first variable"),
      need(input$colY %in% names(data()), "Select a valid second variable"),
      need(is.numeric(data()[[input$colX]]), "First variable must be numeric"),
      need(is.numeric(data()[[input$colY]]), "Second variable must be numeric")
    )
    qqplot(data(), input$colX, input$colY, input$margqmarg1, input$margqmarg2, 
           input$margconstrainedshape, input$margblocksize, input$margnboot, input$margalpha)
  })
  
  output$jointexp <- renderPlot({
    req(data(), input$colX, input$colY)
    validate(
      need(input$colX %in% names(data()), "Select a valid first variable"),
      need(input$colY %in% names(data()), "Select a valid second variable"),
      need(is.numeric(data()[[input$colX]]), "First variable must be numeric"),
      need(is.numeric(data()[[input$colY]]), "Second variable must be numeric")
    )
    jointexpplot(data(), input$colX, input$colY, input$margqmarg1, input$margqmarg2, 
                 input$margconstrainedshape)
  })
  
  rcunctoggleState <- reactiveVal(FALSE)
  rcplotOutput <- reactiveVal(NULL)
  
  observeEvent(input$unc, {
    rcunctoggleState(!rcunctoggleState())
    output$uncertainty_inputs <- renderUI({
      if (rcunctoggleState()) {
        tagList(
          withMathJax(),
          hr(),
          column(6,
                 numericInput("rcblocksize", "Size of blocks for block bootstrap",
                              value = 1, min = 1, step = 1),
                 numericInput("rcnboot", "Number of bootstrap samples",
                              value = 50, min = 1, step = 1)
                 ),
          column(6,
                 numericInput("rcnangles", "Number of rays in \\((0, \\pi/2)\\)",
                              value = 150, min = 1, step = 1),
                 sliderInput("rcalpha", "Significance level for the \\((1-\\alpha)\\)% CI",
                             min = 0.01, max = 0.99, step = 0.05, value = 0.05)
          )
        )
      }
    })
  })
  
  rcgoftoggleState <- reactiveVal(FALSE)
  
  observeEvent(input$rcgof, {
    rcgoftoggleState(!rcgoftoggleState())
    output$rcgof_inputs <- renderUI({
      if (rcgoftoggleState()) {
        tagList(
          withMathJax(),
          hr(),
          column(6,
                 numericInput("rcgofblocksize", "Size of blocks for block bootstrap",
                              value = 1, min = 1, step = 1),
                 numericInput("rcgofnboot", "Number of bootstrap samples",
                              value = 250, min = 1, step = 1)
          ),
          column(6,
                 numericInput("rcgofnangles", "Number of rays in \\((0, \\pi/2)\\)",
                              value = 150, min = 1, step = 1),
                 sliderInput("rcgofalpha", "Significance level for the \\((1-\\alpha)\\)% CI",
                             min = 0.01, max = 0.99, step = 0.05, value = 0.05)
          )
        )
      }
    })
  })
  
  output$rcunc <- renderPlot({
    req(rcplotOutput(), input$rcblocksize, input$rcnboot, input$rcnangles, input$rcalpha)
    if (rcunctoggleState()) {
      uncrcplot(rcplotOutput(), input$rcblocksize, input$rcnboot, input$rcnangles, input$rcalpha)
    }
  })
  
  output$rcgof <- renderPlot({
    req(rcplotOutput(), input$rcgofblocksize, input$rcgofnboot, input$rcgofnangles, input$rcgofalpha)
    if (rcgoftoggleState()) {
      gofrcplot(rcplotOutput(), input$rcgofblocksize, input$rcgofnboot, input$rcgofnangles, input$rcgofalpha)
    }
  })
  
  output$rc <- renderPlot({
    req(data(), input$colX, input$colY)
    validate(
      need(input$colX %in% names(data()), "Select a valid first variable"),
      need(input$colY %in% names(data()), "Select a valid second variable"),
      need(is.numeric(data()[[input$colX]]), "First variable must be numeric"),
      need(is.numeric(data()[[input$colY]]), "Second variable must be numeric"),
      need(
            input$probability <= 1 - input$rcqmarg1 &&
            input$probability <= 1 - input$rcqmarg2 && 
            input$probability <= 1 - input$rcq &&
            1 - input$rcqalphas1 && 
            1 - input$rcqalphas2, 
            "Warning: The curve survival probability p should not be too extreme and within the range of the data, i.e. smaller than the marginal quantiles"
          )
    )
    rc_data <- rcplot(data(), input$colX, input$colY, input$rcqmarg1, input$rcqmarg2, input$rcconstrainedshape,
                      input$rclengthw, input$probability, input$rcmethod, input$rcq, input$rcqalphas1, 
                      input$rcqalphas2, input$rck, input$rcconstrained, input$rctol, input$rcparinit)
    rcplotOutput(rc_data)
    plot(rc_data)
  })
  
  adfgoftoggleState <- reactiveVal(FALSE)
  adfplotOutput <- reactiveVal(NULL)
  
  observeEvent(input$adfgof, {
    adfgoftoggleState(!adfgoftoggleState())
    output$adfgof_inputs <- renderUI({
      if (adfgoftoggleState()) {
        tagList(
          withMathJax(),
          hr(),
          column(6,
                 numericInput("ray", "Ray \\(\\omega\\)",
                              value = 0.5, min = 0, max = 1, step = 0.05),
                 numericInput("blocksize", "Size of blocks for block bootstrap",
                              value = 1, min = 1, step = 1)
          ),
          column(6,
                 numericInput("nboot", "Number of bootstrap samples",
                              value = 250, min = 1, step = 1),
                 sliderInput("alpha", "Significance level for the \\((1-\\alpha)\\)% tolerance intervals",
                             min = 0.01, max = 0.99, step = 0.05, value = 0.05)
          )
        )
      }
    })
  })
      
  output$adfplot <- renderPlot({
    req(data(), input$colX, input$colY)
    validate(
      need(input$colX %in% names(data()), "Select a valid first variable"),
      need(input$colY %in% names(data()), "Select a valid second variable"),
      need(is.numeric(data()[[input$colX]]), "First variable must be numeric"),
      need(is.numeric(data()[[input$colY]]), "Second variable must be numeric")
    )
    
    adf_result <- adfplot(data(), input$colX, input$colY, input$qmarg1, input$qmarg2, input$constrainedshape,
                          input$lengthw, input$method, input$q, input$qalphas1, 
                          input$qalphas2, input$k, input$constrained, input$tol, input$parinit)
    
    adfplotOutput(adf_result)
    plot(adf_result)
  })
  
  output$adfgof <- renderPlot({
    req(adfplotOutput(), input$ray, input$blocksize, input$nboot, input$alpha)
    if (adfgoftoggleState()) {
      gofadfplot(adfplotOutput(), input$ray, input$blocksize, input$nboot, input$alpha)
    }
  })
}

shinyApp(ui = ui, server = server)

