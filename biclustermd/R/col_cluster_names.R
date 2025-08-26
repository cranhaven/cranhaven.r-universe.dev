#' Get column names in each column cluster
#'
#' @param x Biclustering object to extract column cluster designation from
#' @param data Data that contains the column names
#'
#' @return A data frame with two columns: \code{cluster} corresponds to the column
#'   cluster and \code{name} gives the column names in each cluster.
#'
#' @examples
#' data("synthetic")
#' rownames(synthetic) <- letters[1:nrow(synthetic)]
#' colnames(synthetic) <- letters[1:ncol(synthetic)]
#' bc <- biclustermd(synthetic, col_clusters = 3, row_clusters = 2,
#'                 miss_val = mean(synthetic, na.rm = TRUE),
#'                 miss_val_sd = sd(synthetic, na.rm = TRUE),
#'                 col_min_num = 2, row_min_num = 2,
#'                 col_num_to_move = 1, row_num_to_move = 1,
#'                 max.iter = 10)
#' bc
col_cluster_names <- function(x, data) {
  col_clust <- data.frame(col_cluster = part_matrix_to_vector(x$P))

  col_clust$name <- colnames(data)

  col_clust %>% arrange(col_cluster) %>% return()
}

