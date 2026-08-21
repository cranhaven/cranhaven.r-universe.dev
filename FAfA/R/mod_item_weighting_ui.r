#' Item Weighting UI
#' @noRd
item_weighting_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("Item Weighting (K\u0131l\u0131\u00e7, 2026)", class = "bg-primary text-white"),
      card_body(
        p("Applies psychometric weighting based on item difficulty and discrimination."),
        p(
          strong("Reference: "),
          tags$a(
            href = "https://doi.org/10.3758/s13428-026-03095-w",
            target = "_blank",
            rel = "noopener noreferrer",
            "K\u0131l\u0131\u00e7, A. F. (2026)"
          ),
          ". Mitigating the slipping effect in polytomous scales: The Generalized Conditional Reliability Weighting (G-CRW) Algorithm and the WeightMyItems R package."
        ),
        actionButton(ns("calculate_weighted_scores_button"), "Calculate Scores", icon = icon("balance-scale"), class = "btn-primary"),
        hr(),
        h5("Preview"),
        tableOutput(ns("weighted_scores_table_output")),
        downloadButton(ns("download_weighted_data_button"), "Download Weighted Data")
      )
    )
  )
}
