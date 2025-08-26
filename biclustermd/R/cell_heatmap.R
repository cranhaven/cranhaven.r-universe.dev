#' Make a heat map of bicluster cell sizes.
#'
#' @param x An object of class \code{biclustermd}.
#' @param ... Arguments to pass to \code{geom_tile()}
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate row_number
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot aes geom_tile theme labs element_blank
#' @examples
#' data("synthetic")
#'
#' bc <- biclustermd(synthetic, col_clusters = 3, row_clusters = 2,
#'                 miss_val = mean(synthetic, na.rm = TRUE),
#'                 miss_val_sd = sd(synthetic, na.rm = TRUE),
#'                 col_min_num = 2, row_min_num = 2,
#'                 col_num_to_move = 1, row_num_to_move = 1,
#'                 max.iter = 10)
#'
#' cell_heatmap(bc)
#'
#' cell_heatmap(bc) + ggplot2::scale_fill_viridis_c()

cell_heatmap <- function(x, ...) {

  bc <- x

  cell_sizes <- colSums(bc$Q) %*% t(colSums(bc$P))
  colnames(cell_sizes) <- seq(1, ncol(cell_sizes))
  cell_sizes <- as.data.frame(cell_sizes)

  cell_sizes <- cell_sizes %>%
    mutate(row_proto = row_number()) %>%
    gather(col_proto, cell_size, -row_proto)

  cell_sizes %>%
    ggplot(aes(x = col_proto, y = row_proto, fill = cell_size)) +
    geom_tile(...) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(x = "Column Cluster Index", y = "Row Cluster Index",
         fill = "Cell Size")

}
