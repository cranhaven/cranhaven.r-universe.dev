#' advanced_details UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_advanced_details_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    fluidRow(column(
      12,
      box(
        width = 12,
        title = "Linked Data",
        status = "success",
        solidHeader = FALSE,
        collapsible = FALSE,
        column(12, DT::dataTableOutput(ns('matched_dfA')))
      )
    )),
    fluidRow(column(
      12,
      box(
        width = 12,
        title = "Confirmed Matches",
        status = "success",
        solidHeader = FALSE,
        collapsible = FALSE,
        column(12, DT::dataTableOutput(ns('matched_dfB')))
      )
    )),
    br(),
    fluidRow(column(
      6,
      box(
        width = 12,
        title = "Unique in Sample Data Set",
        status = "success",
        solidHeader = FALSE,
        collapsible = FALSE,
        column(12, DT::dataTableOutput(ns('unmatched_dfA')))
      )
    ),
    column(
      6,
      box(
        width = 12,
        title = "Unique in Matching Data Set",
        status = "success",
        solidHeader = FALSE,
        collapsible = FALSE,
        column(12, DT::dataTableOutput(ns('unmatched_dfB')))
      )
    ))
)}

#' advanced_details Server Functions
#'
#' @noRd
mod_advanced_details_server <- function(id, state, session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output[["matched_dfA"]] <- DT::renderDataTable(
      state$advanced_results[['matched_union']],
      caption = 'Linked Data',
      extensions = 'Buttons',
      selection = "multiple",
      rownames = FALSE,
      server = TRUE,
      options = list(
        autoWidth = FALSE,
        scrollX = TRUE,
        lengthMenu = list(c(5, 20, 50, -1), c('default', '20', '50', 'All')),
        pageLength = 5,
        dom = 'Blfrtip',
        buttons = list(
          'copy',
          list(
            extend = 'collection',
            buttons = list(
              list(extend = 'csv', filename = "Linked Data"),
              list(extend = 'excel', filename = "Linked Data")
            ),
            text = 'Download'
          )
        )
      ),
      class = 'compact hover row-border nowrap stripe'
    )


    output[["matched_dfB"]] <- DT::renderDataTable(
      state$advanced_results[['matched_intersect']],
      caption = 'Confirmed Matches',
      extensions = 'Buttons',
      selection = "multiple",
      rownames = FALSE,
      server = TRUE,
      options = list(
        autoWidth = FALSE,
        scrollX = TRUE,
        lengthMenu = list(c(5, 20, 50, -1), c('default', '20', '50', 'All')),
        pageLength = 5,
        dom = 'Blfrtip',
        buttons = list(
          'copy',
          list(
            extend = 'collection',
            buttons = list(
              list(extend = 'csv', filename = "Confirmed Matches"),
              list(extend = 'excel', filename = "Confirmed Matches")
            ),
            text = 'Download'
          )
        )
      ),
      class = 'compact hover row-border nowrap stripe'
    )
    output[["unmatched_dfA"]] <- DT::renderDataTable(
      state$advanced_results[['dfA.unmatch']],
      caption = 'Unique in Sample Data Set',
      extensions = 'Buttons',
      selection = "multiple",
      rownames = FALSE,
      server = TRUE,
      options = list(
        autoWidth = FALSE,
        scrollX = TRUE,
        lengthMenu = list(c(5, 20, 50, -1), c('default', '20', '50', 'All')),
        pageLength = 5,
        dom = 'Blfrtip',
        buttons = list(
          'copy',
          list(
            extend = 'collection',
            buttons = list(
              list(extend = 'csv', filename = "Unique in Sample Data Set"),
              list(extend = 'excel', filename = "Unique in Sample Data Set")
            ),
            text = 'Download'
          )
        )
      ),
      class = 'compact hover row-border nowrap stripe'
    )

    output[["unmatched_dfB"]] <- DT::renderDataTable(
      state$advanced_results[['dfB.unmatch']],
      caption = 'Unique in Matching Data Set',
      extensions = 'Buttons',
      selection = "multiple",
      rownames = FALSE,
      server = TRUE,
      options = list(
        autoWidth = FALSE,
        scrollX = TRUE,
        lengthMenu = list(c(5, 20, 50, -1), c('default', '20', '50', 'All')),
        pageLength = 5,
        dom = 'Blfrtip',
        buttons = list(
          'copy',
          list(
            extend = 'collection',
            buttons = list(
              list(extend = 'csv', filename = "Unique in Matching Data Set"),
              list(extend = 'excel', filename = "Unique in Matching Data Set")
            ),
            text = 'Download'
          )
        )
      ),
      class = 'compact hover row-border nowrap stripe'
    )
  })
}

## To be copied in the UI
# mod_advanced_details_ui("advanced_details_1")

## To be copied in the server
# mod_advanced_details_server("advanced_details_1")
