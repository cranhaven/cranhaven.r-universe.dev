#' simple_settings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_simple_settings_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      width = 12,
      title = "Matching Variables",
      status = "orange",
      solidHeader = FALSE,
      collapsible = TRUE,
      helpText(
        "Check all variables to use for matching. Must be present in both data sets."
      ),
      checkboxGroupButtons(
        inputId = ns("matching_variables"),
        label = NULL,
        choiceNames = c(
          "First Name",
          "Middle Name",
          "Last Name",
          "Date of Birth",
          "Race",
          "Sex",
          "House Number",
          "Street Name",
          "City",
          "SSN"
        ),
        choiceValues = c(
          "firstname",
          "middlename",
          "lastname",
          "birthday",
          "race",
          "sex",
          "housenum",
          "streetname",
          "city",
          "SSN"
        ),
        selected = c(
          "firstname",
          "middlename",
          "lastname",
          "birthday",
          "race",
          "sex"
        ),
        status = "primary",
        size = "sm",
        justified = TRUE,
        width = "100%",
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
      )
    ),
    box(
      width = 12,
      title = "String Matching",
      status = "orange",
      solidHeader = FALSE,
      collapsible = TRUE,
      helpText(
        "Check all variables from among 'Matching Variables' for string distance matching. Must not be selected in 'Numeric Matching'."
      ),
      checkboxGroupButtons(
        inputId = ns("string_matching"),
        label = NULL,
        choiceNames = c(
          "First Name",
          "Middle Name",
          "Last Name",
          "Date of Birth",
          "Race",
          "Sex",
          "House Number",
          "Street Name",
          "City",
          "SSN"
        ),
        choiceValues = c(
          "firstname",
          "middlename",
          "lastname",
          "birthday",
          "race",
          "sex",
          "housenum",
          "streetname",
          "city",
          "SSN"
        ),
        selected = c(
          "firstname",
          "middlename",
          "lastname",
          "birthday",
          "race",
          "sex"
        ),
        status = "primary",
        size = "sm",
        justified = TRUE,
        width = "100%",
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
      )
    ),
    box(
      width = 12,
      title = "Numeric Matching",
      status = "orange",
      solidHeader = FALSE,
      collapsible = TRUE,
      helpText(
        "Check all variables from among 'Matching Variables' for numeric matching. Must not be selected in 'String Matching'."
      ),
      checkboxGroupButtons(
        inputId = ns("numeric_matching"),
        label = NULL,
        choiceNames = c(
          "First Name",
          "Middle Name",
          "Last Name",
          "Date of Birth",
          "Race",
          "Sex",
          "House Number",
          "Street Name",
          "City",
          "SSN"
        ),
        choiceValues = c(
          "firstname",
          "middlename",
          "lastname",
          "birthday",
          "race",
          "sex",
          "housenum",
          "streetname",
          "city",
          "SSN"
        ),
        status = "primary",
        size = "sm",
        justified = TRUE,
        width = "100%",
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
      )
    ),
    box(
      width = 12,
      title = "Partial Matching",
      status = "orange",
      solidHeader = FALSE,
      collapsible = TRUE,
      helpText(
        "Check all variables from among 'String Variables' for indicating a partial matching category for the string distances."
      ),
      checkboxGroupButtons(
        inputId = ns("partial_matching"),
        label = NULL,
        choiceNames = c(
          "First Name",
          "Middle Name",
          "Last Name",
          "Date of Birth",
          "Race",
          "Sex",
          "House Number",
          "Street Name",
          "City",
          "SSN"
        ),
        choiceValues = c(
          "firstname",
          "middlename",
          "lastname",
          "birthday",
          "race",
          "sex",
          "housenum",
          "streetname",
          "city",
          "SSN"
        ),
        selected = c("firstname",
                     "middlename",
                     "lastname",
                     "birthday"),
        status = "primary",
        size = "sm",
        justified = TRUE,
        width = "100%",
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
      )
    ),
    fluidRow(
      column(
        width = 6,
        actionBttn(
          inputId = ns("previous_date_format"),
          label = "Previous: Format Dates",
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
          inputId = ns("next_simple_results"),
          label = "Next: Matching Results",
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

#' simple_settings Server Functions
#'
#' @noRd
mod_simple_settings_server <- function(id, state, parent){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      state$matching_variables <- input$matching_variables
      state$string_matching <- input$string_matching
      state$numeric_matching <- input$numeric_matching
      state$partial_matching <- input$partial_matching
    })

    # Previous page button redirection
    observeEvent(input$previous_date_format, {
      updateTabItems(session = parent, "tabs", "date_format")
    })

    # Next page button redirection
    observeEvent(input$next_simple_results, {
      updateTabItems(session = parent, "tabs", "simple_results")
    })
  })
}

## To be copied in the UI
# mod_simple_settings_ui("simple_settings_1")

## To be copied in the server
# mod_simple_settings_server("simple_settings_1")
