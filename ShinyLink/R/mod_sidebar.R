#' Sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sidebar_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarMenu(
      ns("tabs"),
      menuItem(
        "Data Uploading",
        tabName = "upload",
        icon = icon("upload")
      ),
      menuItem(
        "Data Cleaning",
        icon = icon("brush"),
        startExpanded = TRUE,
        menuSubItem(
          "Remove Duplicate Rows",
          tabName = "duplicate",
          icon = icon("copy")
        ),
        menuSubItem(
          "Assign Variables",
          tabName = "assignment",
          icon = icon("pen")
        ),
        menuSubItem(
          "Recode Race & Gender",
          tabName = "gender_race",
          icon = icon("venus-mars")
        ),
        menuSubItem(
          "Format Dates",
          tabName = "date_format",
          icon = icon("calendar")
        ),
        menuSubItem(
          "Impute Missing Gender ",
          tabName = "imputation",
          icon = icon("circle-question")
        )
      ),

      menuItem(
        "Simple Match",
        icon = icon("link"),
        startExpanded = TRUE,
        menuSubItem(
          "Matching Settings",
          tabName = "simple_settings",
          icon = icon("gear")
        ),
        menuSubItem(
          "Matching Results",
          tabName = "simple_results",
          icon = icon("square-poll-vertical")
        ),
        menuSubItem(
          "Matching Details",
          tabName = "simple_details",
          icon = icon("download")
        )
      ),
      menuItem(
        "Advanced Match",
        icon = icon("microchip"),
        startExpanded = TRUE,
        menuSubItem(
          "Advanced Settings",
          tabName = "advanced_parameters",
          icon = icon("sliders")
        ),
        menuSubItem(
          "Matching Results",
          tabName = "advanced_results",
          icon = icon("hard-drive")
        )
      )
    )
  )
}

#' name_of_module2 Server Functions
#'
#' @noRd
mod_sidebar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_sidebar_ui("mod_sidebar_1")

## To be copied in the server
# mod_sidebar_server("mod_sidebar_1")
