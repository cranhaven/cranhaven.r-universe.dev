#' ItemRest Analysis UI Module
#'
#' @param id Module namespace ID.
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @export
mod_itemrest_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 8),
      
      # Analysis settings
      card(
        card_header(
          class = "bg-info text-white",
          "Analysis Settings", 
          bs_icon("sliders")
        ),
        card_body(
          p("Automated item removal strategies for EFA."),
          
          numericInput(
            ns("n_factors"),
            "Number of Factors (Optional):",
            value = NA, 
            min = 1,
            step = 1
          ),
          helpText("Leave empty to determine automatically via Parallel Analysis."),
          
          selectInput(
            ns("cor_method"),
            "Correlation Method:",
            choices = c("Pearson" = "pearson", "Polychoric" = "polychoric"),
            selected = "pearson"
          ),
          
          selectInput(
            ns("extraction_method"),
            "Extraction Method:",
            choices = c(
              "Unweighted Least Squares" = "uls",
              "Minimum Residual" = "minres",
              "Maximum Likelihood" = "ml",
              "Principal Axis" = "pa"
            ),
            selected = "uls"
          ),
          
          selectInput(
            ns("rotation_method"),
            "Rotation Method:",
            choices = c(
              "Oblimin" = "oblimin",
              "Varimax" = "varimax",
              "Promax" = "promax",
              "GeominQ" = "geominQ"
            ),
            selected = "oblimin"
          ),
          
          actionButton(
            ns("run_itemrest"),
            "Run ItemRest Analysis",
            class = "btn-info w-100",
            icon = icon("play")
          )
        )
      ),
      
      # Optimal strategy
      card(
        card_header("Optimal Strategy Result"),
        card_body(
          verbatimTextOutput(ns("optimal_strategy_text")),
          p("To apply this strategy, go to Exclude Variables, remove the listed items, and continue the analysis with the updated data.")
        )
      )
    ),
    
    # Strategy comparison
    card(
      card_header("Comparative Removal Strategies"),
      card_body(
        tableOutput(ns("removal_summary_table")),
        p(em("This table compares model fit and structure across different item removal thresholds."))
      )
    )
  )
}
