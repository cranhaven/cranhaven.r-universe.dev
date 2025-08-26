#' Make a heatmap of cell MSEs
#'
#' @param x An object of class \code{biclustermd}.
#' @param ... Arguments to pass to \code{geom_tile()}
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot aes geom_tile labs

#' @return A ggplot object.
#' @examples
#' data("synthetic")
#' bc <- biclustermd(synthetic, col_clusters = 3, row_clusters = 2,
#'                 miss_val = mean(synthetic, na.rm = TRUE),
#'                 miss_val_sd = sd(synthetic, na.rm = TRUE),
#'                 col_min_num = 2, row_min_num = 2,
#'                 col_num_to_move = 1, row_num_to_move = 1,
#'                 max.iter = 10)
#'
#' mse_heatmap(bc)
#'
#' mse_heatmap(bc) + ggplot2::scale_fill_viridis_c()

mse_heatmap <- function(x, ...) {

  mse_obj <- cell_mse(x)

  mse_obj %>%
    ggplot(aes(x = col_cluster, y = row_cluster, fill = CellMSE)) +
    geom_tile(...) +
    labs(x = "Column Cluster Index",
         y = "Row Cluster Index",
         fill = "Cell MSE")

}
