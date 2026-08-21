#' Assumptions UI Module
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @noRd
assumptions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Descriptive Statistics
    card(
      card_header("Descriptive Statistics", class = "bg-info text-white", bs_icon("table")),
      card_body(
        actionButton(ns("run_descriptives_button"), "Calculate Descriptives", icon = icon("calculator"), class = "btn-info mb-3"),
        withSpinner(tableOutput(ns("descriptives_table_output")), type = 8, color = "#2C3E50"),
        div(
          class = "d-flex flex-wrap gap-2",
          downloadButton(ns("download_descriptives_button"), "Download CSV"),
          downloadButton(ns("download_assumptions_apa7"), "Download APA 7 Word", class = "btn-primary")
        )
      )
    ),

    # Collinearity & Normality Side-by-Side
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Collinearity", bs_icon("link-45deg")),
        card_body(
          p("Check for multicollinearity using VIF, Tolerance (TOL), and Condition Index (CI)."),
          actionButton(ns("run_collinearity_button"), "Run Collinearity Check", class = "btn-secondary w-100 mb-2"),
          tableOutput(ns("collinearity_table_output")),
          div(
            class = "mt-2 p-2 rounded",
            style = "background:#f1f5f9; font-size:0.78rem; color:#475569; line-height:1.6;",
            tags$b("Interpretation guidelines:"), tags$br(),
            tags$b("VIF:"), " < 5 acceptable, < 10 maximum threshold.", tags$br(),
            tags$b("TOL:"), " > 0.20 acceptable, > 0.10 minimum threshold.", tags$br(),
            tags$b("CI:"), " < 10 no collinearity, 10-30 moderate, > 30 severe."
          )
        )
      ),
      card(
        card_header("Multivariate Normality", bs_icon("graph-up-arrow")),
        card_body(
          p("Mardia's Skewness & Kurtosis, Energy Test."),
          actionButton(ns("run_normality_tests_button"), "Run Normality Tests", class = "btn-secondary w-100 mb-2"),
          tableOutput(ns("multivariate_normality_table_output"))
        )
      )
    )
  )
}
