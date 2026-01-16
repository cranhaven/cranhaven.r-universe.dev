#' Plot diversity dynamics over time
#'
#' This function plots the diversity of barcoded populations across generations.
#' A multi-panel EPS figure is saved, showing one panel per diversity order.
#'
#' @param dataframe A data frame containing barcode diversities with columns for generations and diversity metrics.
#' @param output_directory A string specifying the directory where plots will be saved.
#' @param input_name A string used as the base name for output files (e.g., "replicate1").
#'
#' @return A faceted ggplot object (invisible). The function also saves the figure to an EPS file.
#' @import ggplot2
#' @export
#' @name plotDiversity
#' 
#' @examples
#' # Load demo barcode count data (installed with the package)
#' demo_file <- system.file("extdata", "demo_input.csv", package = "doblin")
#' input_dataframe <- readr::read_csv(demo_file, show_col_types = FALSE)
#'
#' # Calculate diversity indices over time
#' diversity_df <- calculate_diversity(input_dataframe)
#' 
#' # Plot and save diversity figure
#' plotDiversity(
#'   dataframe = diversity_df,
#'   output_directory = tempdir(),
#'   input_name = "demo"
#' )

plotDiversity <- function(dataframe,
                          output_directory,
                          input_name) {

  df <- reshape2::melt(
    dataframe,
    id.vars = "Generations",
    variable.name = "q_type",
    value.name = "q_value"
  )
  
  x_breaks <- sort(unique(df$Generations))
  y_breaks <- sort(unique(log10(df$q_value)))
  
  df_diversities <- ggplot(df) +
    geom_line(aes(Generations, log10(q_value), color = q_type), size = 1.5) +
    geom_hline(yintercept = 3, linetype = "dashed") +
    theme_Publication() +
    xlab("Time") +
    ylab("Diversity") +
    scale_x_continuous(limits = c(min(x_breaks), max(x_breaks))) +
    scale_y_continuous(limits = c(0, max(y_breaks) + 1)) +
    coord_cartesian(expand = FALSE) +
    guides(color = guide_legend(override.aes = list(size = 8, shape = 15)))
  
  p <- df_diversities +
    guides(color = "none") +
    theme_Publication_noYaxis() +
    facet_wrap(~q_type) +
    coord_cartesian(expand = TRUE)
  
  ggsave(
    filename = paste0(output_directory, "/", input_name, "_diversity.eps"),
    plot = p,
    width = 8.25,
    height = 6
  )
}
