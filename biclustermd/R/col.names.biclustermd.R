#' Get data matrix column names and their corresponding column cluster membership
#'
#' @param x and object of class \code{biclustermd}
#'
#' @return a data frame with column names of the shuffled matrix and corresponding column cluster names.
#'
#' @export
#'
#' @examples
#' data("synthetic")
#' # default parameters
#' bc <- biclustermd(synthetic)
#' bc
#' col.names(bc)
#' # this is a simplified version of the output for gather(bc):
#' library(dplyr)
#' gather(bc) %>% distinct(col_cluster, col_name)

col.names.biclustermd <- function(x) {
  col_clust <- data.frame(col_cluster = part_matrix_to_vector(x$P))
  col_clust$col_name <- colnames(x$data)
  col_clust %>% arrange(col_cluster)
}
