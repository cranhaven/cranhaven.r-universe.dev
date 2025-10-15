#' Plot metrics generated from the "calculate" family of quicR functions.
#'
#' Generates a faceted figure of boxplots.
#'
#' @param data A dataframe containing the calculated metrics from the "calculate" family of quicR functions.
#' @param sample_col The name of the column containing the sample IDs.
#' @param fill The column containing the fill aesthetic. Usually the dilutions column.
#' @param dilution_bool Logical; should dilution factors be included in the plot?
#' @param nrow Integer; number of rows to output in the plot.
#' @param ncol Integer; number of columns to output in the plot.
#'
#' @importFrom dplyr %>%
#' @importFrom tidyr gather
#' @import ggplot2
#'
#' @return A ggplot object
#'
#' @examples
#' # This test takes >5 sec
#' \donttest{
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test4.xlsx",
#'   package = "quicR"
#' )
#'
#' data <- quicR::get_real(file)[[1]] |>
#'   quicR::normalize_RFU()
#'
#' meta <- quicR::organize_tables(file) |>
#'   quicR::convert_tables()
#'
#' calculate_metrics(data, meta) |>
#'   plot_metrics()
#' }
#'
#' @export
plot_metrics <- function(data, sample_col = "Sample IDs", fill = "Dilutions", dilution_bool = TRUE, nrow = 2, ncol = 2) {

  variables <- function() {
    (data %>% gather("variable", "value", -c(sample_col, fill)))$variable %>%
      unique() %>%
      length()
  }

  data %>%
    gather("variable", "value", -c(sample_col, fill)) %>%
    ggplot(
      aes(!!sym(sample_col),
        .data$value,
        fill = if (dilution_bool) as.factor(!!sym(fill))
      )
    ) +
    geom_boxplot(
      position = position_dodge2(preserve = "single")
    ) +
    {if (variables() > 1) {
      facet_wrap(~variable, scales = "free_y", nrow = nrow, ncol = ncol)
    }} +
    labs(
      fill = if (dilution_bool) "Dilutions"
    ) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      axis.title = if(variables() > 1) element_blank() else element_text()
    )
}
