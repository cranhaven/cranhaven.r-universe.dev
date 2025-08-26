#' Get data matrix row names and their corresponding row cluster membership
#'
#' @param x and object of class \code{biclustermd}
#'
#' @return a data frame with row names of the shuffled matrix and corresponding row cluster names.
#'
#' @export
#'
#' @examples
#' data("synthetic")
#' # default parameters
#' bc <- biclustermd(synthetic)
#' bc
#' row.names(bc)
#' # this is a simplified version of the output for gather(bc):
#' library(dplyr)
#' gather(bc) %>% distinct(row_cluster, row_name)

row.names.biclustermd <- function (x) {
  row_clust <- data.frame(row_cluster = part_matrix_to_vector(x$Q))
  row_clust$row_name <- rownames(x$data)
  row_clust %>% arrange(row_cluster)
}
