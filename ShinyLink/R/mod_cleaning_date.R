#' cleaning_date UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cleaning_date_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      width = 12,
      title = "Format Dates",
      status = "success",
      solidHeader = FALSE,
      collapsible = TRUE,
      helpText(
        "Tell ShinyLink what date format is used in the sample and matching data"
      )
    ),
    fluidRow(column(
      width = 6,
      box(
        width = 12,
        title = "Sample data set",
        status = "orange",
        solidHeader = FALSE,
        collapsible = TRUE,
        fluidRow(column(
          3,
          HTML("<p><b>Current date format</b></p>")
        ),
        column(9, verbatimTextOutput(ns("date_example_a")))),
        awesomeRadio(
          inputId = ns("date_type_dfA"),
          label = "Select the format that best matches the date shown above",
          choices = c(
            "M/D/Y",
            "Y/M/D",
            "D/M/Y",
            "MMDDYY",
            "YYMMDD",
            "YYYYMMDD",
            "DDMMYY"
          ),
          selected = NULL,
          inline = TRUE,
          status = "success"
        ),
        column(12, DT::dataTableOutput(ns('date_dfA'), width = "100%"))
      )
    ),
    column(
      width = 6,
      box(
        width = 12,
        title = "Matching data set",
        status = "maroon",
        solidHeader = FALSE,
        collapsible = TRUE,
        fluidRow(column(
          3,
          HTML("<p><b>Current date format</b></p>")
        ),
        column(9, verbatimTextOutput(ns("date_example_b")))),
        awesomeRadio(
          inputId = ns("date_type_dfB"),
          label = "Select the format that best matches the date shown above",
          choices = c(
            "M/D/Y",
            "Y/M/D",
            "D/M/Y",
            "MMDDYY",
            "YYMMDD",
            "YYYYMMDD",
            "DDMMYY"
          ),
          selected = NULL,
          inline = TRUE,
          status = "success"
        ),
        column(12, DT::dataTableOutput(ns('date_dfB'), width = "100%"))
      )
    )),
    fluidRow(
      column(
        width = 6,
        actionBttn(
          inputId = ns("previous_gender_race"),
          label = "Previous: Recode Race & Gender",
          style = "simple",
          color = "primary",
          icon = icon("arrow-left"),
          size = "sm"
        ),
        align = "left",
        style = "margin-bottom: 10px;",
        style = "margin-top: -10px;"
      ),
      column(
        width = 6,
        actionBttn(
          inputId = ns("next_simple_settings"),
          label = "Next: Simple Match Settings",
          style = "simple",
          color = "primary",
          icon = icon("arrow-right"),
          size = "sm"
        ),
        align = "right",
        style = "margin-bottom: 10px;",
        style = "margin-top: -10px;"
      ),
      style = "margin-left: 0px;",
      style = "margin-right: 0px;"
    )
  )
}

#' cleaning_date Server Functions
#' @importFrom utils head
#' @noRd
mod_cleaning_date_server <- function(id, state, parent){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Show the date format before cleaning
    output$date_example_a <- renderPrint({

      req(state$dfA_cleaned_gender$birthday)

      if ("birthday" %in% colnames(state$dfA_cleaned_gender)) {
        head(state$dfA_cleaned_gender$birthday, n = 1L)
      } else {
        NULL
      }
    })

    output$date_example_b <- renderPrint({

      req(state$dfB_cleaned_gender$birthday)

      if ("birthday" %in% colnames(state$dfB_cleaned_gender)) {
        head(state$dfB_cleaned_gender$birthday, n = 1L)
      } else {
        NULL
      }
    })

    date_dataset_a <- reactive({

      req(state$dfA_cleaned_gender)

      if (!requireNamespace("lubridate", quietly = TRUE)) {
        stop("Package 'lubridate' is required for date manipulation. Please install it.")
      }

      data <- state$dfA_cleaned_gender
      # TODO Under development
      # TODO Adding imputation function here
      c("M/D/Y",
        "Y/M/D",
        "D/M/Y",
        "MMDDYY",
        "YYMMDD",
        "YYYYMMDD",
        "DDMMYY")

      if (input$date_type_dfA %in% c("M/D/Y", "MMDDYY")) {
        data$birthday <- lubridate::mdy(data$birthday)
      }

      if (input$date_type_dfA %in% c("Y/M/D", "YYMMDD", "YYYYMMDD")) {
        data$birthday <- lubridate::ymd(data$birthday)
      }

      if (input$date_type_dfA %in% c("D/M/Y", "DDMMYY")) {
        data$birthday <- lubridate::dmy(data$birthday)
      }
      # lubridate::ymd()
      # lubridate::ydm()

      # lubridate::mdy()
      # lubridate::myd()

      # lubridate::dmy()
      # lubridate::dym()
      state$dfA_cleaned_date <- data # TODO Under development update state
      state$state_dfA <- data


      return(data)
    })

    date_dataset_b <- reactive({

      req(state$dfB_cleaned_gender)

      if (!requireNamespace("lubridate", quietly = TRUE)) {
        stop("Package 'lubridate' is required for date manipulation. Please install it.")
      }

      data <- state$dfB_cleaned_gender
      # TODO Under development
      # TODO Adding imputation function here
      if (input$date_type_dfB %in% c("M/D/Y", "MMDDYY")) {
        data$birthday <- lubridate::mdy(data$birthday)
      }

      if (input$date_type_dfB %in% c("Y/M/D", "YYMMDD", "YYYYMMDD")) {
        data$birthday <- lubridate::ymd(data$birthday)
      }

      if (input$date_type_dfB %in% c("D/M/Y", "DDMMYY")) {
        data$birthday <- lubridate::dmy(data$birthday)
      }

      state$dfB_cleaned_date <- data # TODO Under development update state
      state$state_dfB <- data

      return(data)
    })


    output$date_dfA <- DT::renderDataTable(
      date_dataset_a(),
      # caption = 'Data in the Sample data set',
      extensions = 'Buttons',
      selection = "single",
      rownames = FALSE,
      server = TRUE,
      options = list(
        autoWidth = FALSE,
        scrollX = TRUE,
        lengthMenu = list(c(15, 20, 50,-1), c('default', '20', '50', 'All')),
        pageLength = 15,
        dom = 'Blfrtip',
        buttons = list('copy', list(
          extend = 'collection',
          buttons = list(
            list(extend = 'csv', filename = "Date formated Sample Data"),
            list(extend = 'excel', filename = "Date formated Sample Data")),
          text = 'Download'
        ))
      ),
      class = 'compact hover row-border nowrap stripe'
    )

    output$date_dfB <- DT::renderDataTable(
      date_dataset_b(),
      # caption = 'Data in the Matching data set',
      extensions = 'Buttons',
      selection = "single",
      rownames = FALSE,
      server = TRUE,
      options = list(
        autoWidth = FALSE,
        scrollX = TRUE,
        lengthMenu = list(c(15, 20, 50,-1), c('default', '20', '50', 'All')),
        pageLength = 15,
        dom = 'Blfrtip',
        buttons = list('copy', list(
          extend = 'collection',
          buttons = list(
            list(extend = 'csv', filename = "Date formated Matching Data"),
            list(extend = 'excel', filename = "Date formated Matching Data")),
          text = 'Download'
        ))
      ),
      class = 'compact hover row-border nowrap stripe'
    )


    # Previous page button redirection
    observeEvent(input$previous_gender_race, {
      updateTabItems(session = parent, "tabs", "gender_race")
    })

    # Next page button redirection
    observeEvent(input$next_simple_settings, {
      updateTabItems(session = parent, "tabs", "simple_settings")
    })
  })
}

## To be copied in the UI
# mod_cleaning_date_ui("cleaning_date_1")

## To be copied in the server
# mod_cleaning_date_server("cleaning_date_1")
