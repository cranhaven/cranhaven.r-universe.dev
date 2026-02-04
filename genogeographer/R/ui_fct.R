
ui_fct <- function(){
  fluidPage(
    useShinyjs(),  # Set up shinyjs
    tags$style(appCSS),
    titlePanel("GenoGeographer - A tool for genogeographic inference"),
    sidebarLayout(
      sidebarPanel = 
        sidebarPanel(width = 2,
                     h4("Input file"),
                     fileInput('profile_file', 'Choose CSV File', width = "100%",
                               accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv', '.xlsx', '.xls')),
                     uiOutput("column_panel"),
                     ##
                     h4("Settings"),
                     radioButtons(inputId = "meta", label = "Grouping type:", choices = c("Meta-populations" = "meta", "Populations" = "pop")),
                     sliderInput(inputId = "min_n", label = "Minimum sample size:", min = 5, max = 200, step = 5, value = 75),
                     checkboxGroupInput(inputId = "admix", label = "Analyse 1st order admixture:", 
                                        choiceNames = list("Admixture (may take some time)"), choiceValues = list("admix")),
                     shinyWidgets::sliderTextInput(inputId = "CI", label = "Confidence level:", choices = c(95, 97.5, 99, 99.9, 99.99), selected = 95, post = "%"),
                     # sliderInput(inputId = "CI", label = "Confidence level:", min = 0.95, max = 0.9999, sep = "", step = 0.001, value = 0.95),
                     uiOutput("dbs"),
                     checkboxGroupInput(inputId = "tilt", label = "Adjust p-values by exponential tilting:", 
                                        choiceNames = list("Adjust (may take some time)"), choiceValues = list("adjust")),
                     uiOutput("side_pvalue"),
                     uiOutput("LR_select"),
                     tags$style(type='text/css', "#analyse, #reset, #report_download { width:100%; margin-top: 25px;}"),
                     withBusyIndicatorUI(
                       actionButton(inputId = "analyse", label = "Analyse!", icon = icon("refresh"), class = "btn-primary")
                     ), 
                     ##actionButton(inputId = "analyse", label = "Analyse!", icon("refresh")),
                     actionButton(inputId = "reset", label = "Reset", icon = icon("trash")),
                     tags$hr(),
                     uiOutput("report_panel"),
                     div(helpText(paste0("Version: genogeographer (", packageVersion("genogeographer"),")"))),
                     div(helpText(paste0("Developer: ", packageDescription("genogeographer", fields = "Maintainer"))))
        ),
      mainPanel = mainPanel(width = 10, uiOutput("analysis"))
    )
  )
}