#' About UI Module
#' @param id Module namespace ID.
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @noRd
about_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Application overview
    card(
      card_header(
        class = "bg-dark text-white",
        "About FAfA",
        bs_icon("info-circle")
      ),
      card_body(
        htmlOutput(ns("application_description_html"))
      )
    ),

    card(
      class = "mt-3",
      card_header(
        class = "bg-warning text-dark",
        "What's New in FAfA",
        span(class = "badge text-bg-dark ms-2", "1.2"),
        bs_icon("star")
      ),
      card_body(
        htmlOutput(ns("whats_new_html"))
      )
    ),

    # Developer, contributors and citation
    layout_columns(
      col_widths = c(4, 4, 4),

      card(
        card_header(
          class = "bg-primary text-white",
          "Developer",
          bs_icon("person-workspace")
        ),
        card_body(
          htmlOutput(ns("developer_info_html"))
        )
      ),

      card(
        card_header(
          class = "bg-success text-white",
          "Acknowledgements & Version",
          bs_icon("people")
        ),
        card_body(
          htmlOutput(ns("contributors_version_html"))
        )
      ),

      card(
        card_header(
          class = "bg-info text-white",
          "How to Cite",
          bs_icon("quote")
        ),
        card_body(
          htmlOutput(ns("citation_info_html"))
        )
      )
    )
  )
}
