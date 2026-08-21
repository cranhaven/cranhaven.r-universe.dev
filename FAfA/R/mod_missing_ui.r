#' Missing Value Handling UI Module
#'
#' @param id Module namespace ID.
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @export
mod_missing_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 8),

      # Configuration Card
      card(
        card_header(
          class = "bg-primary text-white",
          "Imputation Settings",
          bs_icon("gear")
        ),
        card_body(
          p("Analyze missing data patterns and apply advanced imputation strategies."),
          selectInput(
            ns("imputation_method"),
            "Choose Imputation Method:",
            choices = c(
              "Visualize Only (No Action)" = "none",
              "Listwise Deletion (Remove Rows)" = "listwise",
              "Mean Imputation (Simple)" = "mean",
              "Median Imputation (Simple)" = "median",
              "Amelia II (Expectation-Maximization)" = "amelia",
              "MICE (Predictive Mean Matching)" = "mice",
              "Random Forest (Categorical / Integer)" = "missForest_cat",
              "Random Forest (Continuous / Decimal)" = "missForest_cont"
            ),
            selected = "none"
          ),
          helpText(
            icon("info-circle"),
            "Note: Use 'RF (Categorical)' for Likert scales (1,2,3...) to preserve integer structure. Use 'RF (Continuous)' for decimal values."
          ),
          actionButton(
            ns("apply_imputation"),
            "Apply Imputation",
            class = "btn-success w-100",
            icon = icon("check")
          ),
          hr(),
          downloadButton(
            ns("download_data"),
            "Download Processed Data",
            class = "btn-secondary w-100"
          )
        )
      ),

      # Visualization Card
      card(
        card_header("Missingness Map"),
        card_body(
          plotOutput(ns("missing_plot"), height = "400px")
        )
      )
    ),

    # Summary Table & MCAR Test Card
    card(
      card_header("Missing Data Summary & Diagnostics"),
      card_body(
        layout_columns(
          col_widths = c(6, 6),

          # Sol Kolon: MCAR Testi
          div(
            h5("Little's MCAR Test"),
            p("Test whether missing data is Missing Completely at Random (MCAR)."),
            actionButton(ns("run_mcar_button"), "Run MCAR Test", icon = icon("play-circle"), class = "btn-warning mb-2"),
            verbatimTextOutput(ns("mcar_output"))
          ),

          # Sag Kolon: Ozet Tablo
          div(
            h5("Missingness by Variable"),
            tableOutput(ns("missing_summary_table"))
          )
        )
      )
    )
  )
}
