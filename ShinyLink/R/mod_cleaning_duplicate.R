#' cleaning_duplicate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cleaning_duplicate_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # hr(),
    box(
      width = 12,
      title = "Duplicate Record Remove",
      status = "success",
      solidHeader = FALSE,
      collapsible = TRUE,
      helpText(
        "Remove duplicated entries based on all columns in sample data set or matching data set."
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
          width = 10,
          helpText("Remove duplicated entries in sample data set.")
        ),
        column(
          width = 2,
          switchInput(
            inputId = ns("duplicate_switchA"),
            onStatus = "success",
            offStatus = "danger",
            size = "small"
          )
        )),
        column(12, DT::dataTableOutput(ns('duplicate_dfA'), width = "100%"))
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
          width = 10,
          helpText("Remove duplicated entries in matching data set.")
        ),
        column(
          width = 2,
          switchInput(
            inputId = ns("duplicate_switchB"),
            onStatus = "success",
            offStatus = "danger",
            size = "small"
          )
        )),
        column(12, DT::dataTableOutput(ns('duplicate_dfB'), width = "100%"))
      )
    )),
    fluidRow(
      column(
        width = 6,
        actionBttn(
          inputId = ns("previous_upload"),
          label = "Previous: Data Uploading",
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
          inputId = ns("next_assignment"),
          label = "Next: Assign Variables",
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

#' cleaning_duplicate Server Functions
#' @noRd
mod_cleaning_duplicate_server <- function(id, state, parent) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # library(magrittr)

    duplicate_dataset_a <- reactive({
      # Table data validation
      req(state$dfA_uploaded)

      if (input$duplicate_switchA) {
        data <- state$dfA_uploaded %>% dplyr::distinct()
      }
      else {
        data <- state$dfA_uploaded
      }

      state$dfA_cleaned_duplicate <- data # update the state

      return(data)
    })

    output$duplicate_dfA <- DT::renderDataTable(
      duplicate_dataset_a(),
      caption = 'Data in the Sample data set',
      extensions = 'Buttons',
      selection = "single",
      rownames = FALSE,
      server = TRUE,
      options = list(
        autoWidth = FALSE,
        scrollX = TRUE,
        lengthMenu = list(c(15, 20, 50, -1), c('default', '20', '50', 'All')),
        pageLength = 15,
        dom = 'Blfrtip',
        buttons = list()
      ),
      class = 'compact hover row-border nowrap stripe'
    )

    duplicate_dataset_b <- reactive({
      # Table data validation
      req(state$dfB_uploaded)

      if (input$duplicate_switchB) {
        data <- state$dfB_uploaded %>% dplyr::distinct()
      }
      else {
        data <- state$dfB_uploaded
      }

      state$dfB_cleaned_duplicate <- data # update the state

      return(data)
    })

    output$duplicate_dfB <- DT::renderDataTable(
      duplicate_dataset_b(),
      caption = 'Data in the Matching data set',
      extensions = 'Buttons',
      selection = "single",
      rownames = FALSE,
      server = TRUE,
      options = list(
        autoWidth = FALSE,
        scrollX = TRUE,
        lengthMenu = list(c(15, 20, 50, -1), c('default', '20', '50', 'All')),
        pageLength = 15,
        dom = 'Blfrtip',
        buttons = list()
      ),
      class = 'compact hover row-border nowrap stripe'
    )

    # Notification
    observeEvent(input$duplicate_switchA, {
      req(state$dfA_uploaded)
      if (input$duplicate_switchA == TRUE) {
        showNotification(paste(
          sum(duplicated(state$dfA_uploaded) == TRUE),
          "duplicated entries were removed in the Matching Data Set"
        ),
        type = "message")
      }
    }, ignoreInit = TRUE)
    observeEvent(input$duplicate_switchB, {
      req(state$dfB_uploaded)
      if (input$duplicate_switchB == TRUE) {
        showNotification(paste(
          sum(duplicated(state$dfB_uploaded) == TRUE),
          "duplicated entries were removed in the Matching Data Set"
        ),
        type = "message")
      }
    }, ignoreInit = TRUE)


    # Previous page button redirection
    observeEvent(input$previous_upload, {
      updateTabItems(session = parent, "tabs", "upload")
    })

    # Next page button redirection
    observeEvent(input$next_assignment, {
      updateTabItems(session = parent, "tabs", "assignment")
    })
  })
}

## To be copied in the UI
# mod_cleaning_duplicate_ui("cleaning_duplicate_1")

## To be copied in the server
# mod_cleaning_duplicate_server("cleaning_duplicate_1")
