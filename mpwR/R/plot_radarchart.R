#' Radar chart
#'
#' Plot radar chart of summary statistics.
#'
#' Summary results are displayed via radar chart. Each analysis has its own trace.
#'
#' @param input_df Data frame with summary information. Analysis column and at least one category column is required.
#'
#' @author Oliver Kardell
#'
#' @import plotly
#'
#' @return This function returns a radar chart as htmlwidget.
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(plotly)
#' library(tibble)
#'
#' # Example data
#' data <- tibble::tibble(
#'  Analysis = c("A", "B"),
#'  "Median ProteinGroup.IDs [abs.]" = c(5, 10),
#'  "Median Protein.IDs [abs.]" = c(5, 10),
#'  "Median Peptide.IDs [abs.]" = c(5, 10),
#'  "Median Precursor.IDs [abs.]" = c(5, 10),
#'  "Full profile - Precursor.IDs [abs.]" = c(5, 10),
#'  "Full profile - Peptide.IDs [abs.]" = c(5, 10),
#'  "Full profile - Protein.IDs [abs.]" = c(5, 10),
#'  "Full profile - ProteinGroup.IDs [abs.]" = c(5, 10),
#'  "Full profile - Precursor.IDs [%]" = c(5, 10),
#'  "Full profile - Peptide.IDs [%]" = c(5, 10),
#'  "Full profile - Protein.IDs [%]" = c(5, 10),
#'  "Full profile - ProteinGroup.IDs [%]" = c(5, 10),
#'  "Precursor.IDs [abs.] with a CV Retention time < 5 [%]" = c(5, 10),
#'  "Proteingroup.IDs [abs.] with a CV LFQ < 20 [%]" = c(NA, 10),
#'  "Peptide.IDs [abs.] with a CV LFQ < 20 [%]" = c(NA, 10),
#'  "Peptide IDs with zero missed cleavages [abs.]" = c(5, 10),
#'  "Peptide IDs with zero missed cleavages [%]" = c(5, 10)
#' )
#'
#' # Plot
#' plot_radarchart(
#'   input_df = data
#' )


#Radar chart
plot_radarchart <- function(input_df) {

  #dependency ==
  #Require Analysis column
  cols <- c("Analysis")

    if (sum(colnames(input_df) %in% cols) != length(cols)) {
      stop("No Analysis column detected.")
    }

  #Require at least one additional input column
  if (length(colnames(input_df)) <= 1) {
    stop("Please add at least one category column.")
  }
  #====

  input_df <- get_summary_percentage(input_df = input_df)

  analysis <- input_df$`Analysis`
  names <- categories(input_df)

  fig <- plotly::plot_ly(
    mode = "markers",
    type = "scatterpolar",
    fill = "toself"
  ) %>%
    layout(
      polar = list(
        radialaxis = list(visible = TRUE, range = c(0, 100))
      )
    )

  for (i in seq_len(nrow(input_df)))
    fig <- fig %>% add_trace(
      r = values(input_df, i),
      theta = names,
      name = analysis[i]
    )

  return(fig)
}
