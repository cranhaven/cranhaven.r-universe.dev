#' Plot heatmap for result confusion matrix.
#'
#' @param table A table.
#' @param filepath File path the plot to save.Default NULL.
#' @param name Model names.
#'
#' @importFrom reshape2 melt
#' @importFrom dplyr mutate group_by ungroup across na_if
#' @importFrom ggplot2 coord_equal ggplot aes geom_text geom_tile theme_bw coord_equal scale_fill_gradient2 labs theme element_text ggsave scale_fill_gradientn
#' @importFrom stringr str_c
#' @importFrom RColorBrewer brewer.pal
#' @return A `ggplot` object.
#' @export
mi_plot_heatmap <- function(table,name = NULL, filepath = NULL) {
  melted <- table %>%
    as.table() %>%
    melt()
  heat <- ggplot(melted, aes(x = .data[['Reference']], y =.data[['Prediction']], fill = .data[['value']])) +
    geom_tile() +
    geom_text(aes(label = na_if(round(.data[['value']], 2), 0)), color = "#00468B99", size = 2) +
    theme_bw() +
    coord_equal() +
    scale_fill_gradientn(colors = c("#ffffff",brewer.pal(5, "YlOrRd")),values = c(0,4e-04,4e-03,4e-02,0.2,1))+
    theme(
    axis.text.x = element_text(
      angle = 45,
      vjust = 1, size = 20, hjust = 1,
      lineheight = 10
    ),
    axis.text.y = element_text(size = 20),
    strip.text.y = element_text(
      angle = 0,
      vjust = 0.5,
      hjust = 0.5,
      size = 10
    ),
    axis.title = element_text(size = 40)
  )
  if (!is.null(filepath)) {
    ggsave(str_c(filepath, "heatmap_", name, ".png"), width = 22.37, height = 20, plot = heat, device = "png", dpi = 600)
  }
  return(heat)
}
