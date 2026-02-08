#' cleaning_imputation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cleaning_imputation_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      width = 12,
      title = "Impute Missing Gender",
      status = "success",
      solidHeader = FALSE,
      collapsible = TRUE,
      helpText(
        "Method",
        "This value determines the data set that is used to predict the gender of the name.",
        "The 'ssa' method looks up names based from the U.S. Social Security Administration baby name data.",
        "(This method is based on an implementation by Cameron Blevins.)",
        "The 'ipums' method looks up names from the U.S. Census data in the Integrated Public Use Microdata Series.",
        "(This method was contributed by Ben Schmidt.)"
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
        materialSwitch(
          inputId = ns("enable_imputation_a"),
          label = "Enable missing gender imputation",
          right = TRUE,
          value = FALSE,
          status = "success"
        ),
        prettyRadioButtons(
          inputId = ns("method_imputation_a"),
          label = "Select Method",
          choiceNames = c(
            "Social Security Administration baby name data",
            "Census data in the Integrated Public Use Microdata Series"
          ),
          choiceValues = c("ssa", "ipums"),
          inline = TRUE,
          status = "danger",
          fill = TRUE,
          bigger = TRUE
        ),
        sliderTextInput(
          inputId = ns("range_imputation_a"),
          label = "Choose a range for year of birth:",
          choices = 1900:2022,
          selected = c(1960, 2000),
          grid = TRUE
        ),
        column(12, DT::dataTableOutput(ns('imputation_dfA'), width = "100%"))
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
        materialSwitch(
          inputId = ns("enable_imputation_b"),
          label = "Enable missing gender imputation",
          right = TRUE,
          value = FALSE,
          status = "success"
        ),
        prettyRadioButtons(
          inputId = ns("method_imputation_b"),
          label = "Select Method",
          choiceNames = c(
            "Social Security Administration baby name data",
            "Census data in the Integrated Public Use Microdata Series"
          ),
          choiceValues = c("ssa", "ipums"),
          inline = TRUE,
          status = "danger",
          fill = TRUE,
          bigger = TRUE
        ),
        sliderTextInput(
          inputId = ns("range_imputation_b"),
          label = "Choose a range for year of birth:",
          choices = 1900:2022,
          selected = c(1940, 1980),
          grid = TRUE
        ),
        column(12, DT::dataTableOutput(ns('imputation_dfB'), width = "100%"))
      )
    )),
    # fluidRow(
      # column(
      #   width = 6,
      #   actionBttn(
      #     inputId = ns("previous_date_format"),
      #     label = "Previous: Format Dates",
      #     style = "simple",
      #     color = "primary",
      #     icon = icon("arrow-left"),
      #     size = "sm"
      #   ),
      #   align = "left",
      #   style = "margin-bottom: 10px;",
      #   style = "margin-top: -10px;"
      # ),
      # column(
      #   width = 6,
      #   actionBttn(
      #     inputId = ns("next_simple_settings"),
      #     label = "Next: Simple Match Settings",
      #     style = "simple",
      #     color = "primary",
      #     icon = icon("arrow-right"),
      #     size = "sm"
      #   ),
      #   align = "right",
      #   style = "margin-bottom: 10px;",
      #   style = "margin-top: -10px;"
      # ),
    #   style = "margin-left: 0px;",
    #   style = "margin-right: 0px;"
    # )
  )
}

#' cleaning_imputation Server Functions
#' @noRd
mod_cleaning_imputation_server <- function(id, state, parent){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # library(magrittr)
    # library(gender)

    # TODO Adding features
    # https://github.com/lmullen/gender


    imputation_dataset_a <- reactive({

      req(state$dfA_cleaned_date)

      if (!requireNamespace("gender", quietly = TRUE)) {
        stop("Package 'gender' is required for gender imputation. Please install it.")
      }
      if (!requireNamespace("lubridate", quietly = TRUE)) {
        stop("Package 'lubridate' is required for date manipulation. Please install it.")
      }

      data <- state$dfA_cleaned_date
      # TODO Under development
      # TODO Adding imputation function here
      method0 <- "ssa"
      if (input$enable_imputation_a) {
        for (i in 1:nrow(data)) {
          if (is.na(data$sex[i])) {
            data$sex[i] <-
              gender::gender(
                data$firstname[i],
                method = method0,
                years = lubridate::year(data$birthday[i])
              )$gender
            message("Doing missing gender imputation")
            message(data$firstname[i])
            message(data$sex[i])
          }
        }
      }

      state$dfA_cleaned_imputation <- data
      state$state_dfA <- data

      return(data)
    })

    imputation_dataset_b <- reactive({

      req(state$dfB_cleaned_date)

      if (!requireNamespace("gender", quietly = TRUE)) {
        stop("Package 'gender' is required for gender imputation. Please install it.")
      }
      if (!requireNamespace("lubridate", quietly = TRUE)) {
        stop("Package 'lubridate' is required for date manipulation. Please install it.")
      }

      data <- state$dfB_cleaned_date
      # TODO Under development
      # TODO Adding imputation function here

      # TODO refactor to utils for code re-use
      method0 <- "ssa"
      if (input$enable_imputation_b) {
        for (i in 1:nrow(data)) {
          if (is.na(data$sex[i])) {
            data$sex[i] <-
              gender::gender(
                data$firstname[i],
                method = method0,
                years = lubridate::year(data$birthday[i])
              )$gender
            message("Doing missing gender imputation")
            message(data$firstname[i])
            message(data$sex[i])
          }
        }
      }

      state$dfB_cleaned_imputation <- data
      state$state_dfB <- data

      return(data)
    })

    output$imputation_dfA <- DT::renderDataTable(
      imputation_dataset_a(),
      caption = 'Data in the Sample data set',
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
        buttons =
          list(
            "copy",
            list(
              extend = "collection"
              ,
              buttons = c("csv", "excel", "pdf")
              ,
              text = "Download"
            )
          )
      ),
      class = 'compact hover row-border nowrap stripe'
    )

    output$imputation_dfB <- DT::renderDataTable(
      imputation_dataset_b(),
      caption = 'Data in the Matching data set',
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
        buttons =
          list(
            "copy",
            list(
              extend = "collection"
              ,
              buttons = c("csv", "excel", "pdf")
              ,
              text = "Download"
            )
          )
      ),
      class = 'compact hover row-border nowrap stripe'
    )

    # Previous page button redirection
    # observeEvent(input$previous_date_format, {
    #   updateTabItems(session = parent, "tabs", "date_format")
    # })

    # Next page button redirection
    # observeEvent(input$next_simple_settings, {
    #   updateTabItems(session = parent, "tabs", "simple_settings")
    # })
  })
}

## To be copied in the UI
# mod_cleaning_imputation_ui("cleaning_imputation_1")

## To be copied in the server
# mod_cleaning_imputation_server("cleaning_imputation_1")
