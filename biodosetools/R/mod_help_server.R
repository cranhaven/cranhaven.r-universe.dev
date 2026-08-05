#' Help chromosome hottable server module
#'
#' @param id Internal parameter for {shiny}.
#'
#' @import shiny rhandsontable
#' @importFrom rlang .data
#' @noRd
mod_help_chromosome_hot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Tables ----
    data <- data.frame(
      Chromosome = c(1, 4, 12),
      Red = c(FALSE, TRUE, FALSE),
      Green = c(TRUE, FALSE, FALSE),
      Yellow = c(FALSE, FALSE, TRUE)
    ) %>%
      dplyr::mutate(
        Chromosome = as.integer(.data$Chromosome)
      )

    output$help_chromosome_hot <- renderRHandsontable({
      num_cols <- as.numeric(ncol(data))

      # Convert to hot and format table
      hot <- data %>%
        rhandsontable(
          width = (80 + num_cols * 85),
          height = "100%"
        ) %>%
        hot_col(1, colWidths = 115, readOnly = TRUE) %>%
        hot_col(2:ncol(data), colWidths = 85) %>%
        hot_cols(halign = "htCenter")

      hot$x$contextMenu <- list(items = c("remove_row", "---------", "undo", "redo"))
      return(hot)
    })
  })
}
