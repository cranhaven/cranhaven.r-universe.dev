#' Plot CRISPR scores after normalization
#' @description This plots normalization after CRISPR scores have been calculated
#'
#' @param .data Data can be piped in with tidyverse pipes from function to
#' function. But the data must still be a gimap_dataset
#' @param gimap_dataset A special dataset structure that is setup using the
#' `setup_data()` function.
#' @param output_file A file for the output
#' @export
#' @return A ggplot2 boxplot of the CRISPR scores separated by the type of
#' target. Can be used to determine the normalization has proceeded properly.
#'
#' @import ggplot2
#'
#' @examples \donttest{
#'
#' gimap_dataset <- get_example_data("gimap") %>%
#'   gimap_filter() %>%
#'   gimap_annotate(cell_line = "HELA") %>%
#'   gimap_normalize(
#'     timepoints = "day",
#'     missing_ids_file = tempfile()
#'   )
#'
#' # Plot:
#' plot_crispr(gimap_dataset)
#' }
plot_crispr <- function(.data = NULL, gimap_dataset, output_file = "crispr_norm_plot.png") {
  if (!is.null(.data)) gimap_dataset <- .data

  if (!("gimap_dataset" %in% class(gimap_dataset))) stop("This function only works with gimap_dataset objects which can be made with the setup_data() function.")

  if (is.null(gimap_dataset$normalized_log_fc)) {
    stop("No normalized data found in this gimap_dataset. Make sure you have run the gimap_normalize() function")
  }

  if (!is.null(gimap_dataset$normalized_log_fc)) {
    source_data <- gimap_dataset$normalized_log_fc
  }

  output_plot <- gimap_dataset$normalized_log_fc %>%
    ggplot2::ggplot(ggplot2::aes(x = norm_ctrl_flag, y = crispr_score, fill = norm_ctrl_flag)) +
    ggplot2::geom_violin() +
    ggplot2::geom_boxplot(outliers = FALSE) +
    ggplot2::theme(axis.text.x = element_text(angle = 90)) +
    ggplot2::facet_wrap(~rep)

  return(output_plot)
}
