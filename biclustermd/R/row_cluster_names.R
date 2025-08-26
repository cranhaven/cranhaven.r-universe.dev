#' Get row names in each row cluster
#'
#' @param x Biclustering object to extract row cluster designation from
#' @param data Data that contains the row names
#'
#' @return A data frame with two columns: \code{cluster} corresponds to the row
#'   cluster and \code{name} gives the row names in each cluster.
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
row_cluster_names <- function(x, data) {
  row_clust <- data.frame(row_cluster = part_matrix_to_vector(x$Q))

  row_clust$name <- rownames(data)

  row_clust %>% arrange(row_cluster) %>% return()
}

