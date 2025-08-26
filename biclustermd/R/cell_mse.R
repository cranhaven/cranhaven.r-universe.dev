#' Make a data frame containing the MSE for each bicluster cell
#'
#' @param x An object of class \code{biclustermd}.
#'
#' @importFrom stats na.omit
#'
#' @export
#'
#' @return A data frame giving the row cluster, column cluster, the number of
#'   data points in each row and column cluster, the number of data points missing
#'   in the cell, and the cell MSE.
#' @examples
#' data("synthetic")
#' bc <- biclustermd(synthetic, col_clusters = 3, row_clusters = 2,
#'                 miss_val = mean(synthetic, na.rm = TRUE),
#'                 miss_val_sd = sd(synthetic, na.rm = TRUE),
#'                 col_min_num = 2, row_min_num = 2,
#'                 col_num_to_move = 1, row_num_to_move = 1,
#'                 max.iter = 10)
#' cell_mse(bc)

cell_mse <- function(x) {
  data <- as.matrix(x$data)

  mse_df <- expand.grid(row_cluster = 1:ncol(x$Q),
                        col_cluster = 1:ncol(x$P))

  nr <- nrow(mse_df)

  row_cluster_count <- colSums(x$Q, na.rm = TRUE)
  col_cluster_count <- colSums(x$P, na.rm = TRUE)
  mse_df$row_cluster_count <- unlist(lapply(1:nr, function(z) {
    row_cluster_count[mse_df$row_cluster[z]]
  }))
  mse_df$col_cluster_count <- unlist(lapply(1:nr, function(z) {
    col_cluster_count[mse_df$col_cluster[z]]
  }))

  mse_df$CellMean <- unlist(lapply(1:nr, function(z) {

    x$A[mse_df$row_cluster[z], mse_df$col_cluster[z]]

  }), use.names = FALSE)

  cell_mses <- function(row) {
    cell_mean <- mse_df$CellMean[row]

    rows <- which(x$Q[, mse_df$row_cluster[row]] == 1)
    cols <- which(x$P[, mse_df$col_cluster[row]] == 1)

    cell_values <- na.omit(as.vector(data[rows, cols]))

    mean((cell_mean - cell_values) ^ 2)
  }

  mse_df$CellMSE <- unlist(lapply(1:nr, cell_mses), use.names = FALSE)

  return(mse_df)
}
