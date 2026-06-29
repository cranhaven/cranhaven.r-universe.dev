#'@title Network Reconstruction via epsilon-NN
#'
#'@description Reconstruction of gene-gene network via low-dimentional projections (via epsilon-NN).
#'@name eNN
#'@importFrom Matrix sparseMatrix
#'@importFrom stats dist
#'@importFrom tictoc tic toc
#'@param E_g Embedding representations of genes.
#'@return The epsilon-NN network.
#'
#'@export

eNN <- function(E_g) {
  message("Construct gene-gene network using low-dimensional representations via epsilon-NN...\n")
  tic()

  q_FDR <- 0.05
  n_gene <- nrow(E_g)

  num <- ceiling(q_FDR * n_gene * (n_gene - 1) / 2)

  if (inherits(E_g, "dgCMatrix") || inherits(E_g, "dgTMatrix")) {
    E_g_dense <- as.matrix(E_g)
  } else {
    E_g_dense <- E_g
  }

  temp <- as.matrix(dist(E_g_dense, method = "euclidean", diag = TRUE, upper = TRUE))

  upper_idx <- upper.tri(temp, diag = FALSE)
  temp_values <- temp[upper_idx]

  rows_idx <- row(temp)[upper_idx]
  cols_idx <- col(temp)[upper_idx]

  result <- cbind(temp_values, rows_idx, cols_idx)
  result <- result[order(result[, 1]), , drop = FALSE]
  result <- result[1:num, , drop = FALSE]

  rows <- result[, 2]
  cols <- result[, 3]
  vals <- exp(-result[, 1])

  sparse_matrix <- sparseMatrix(i = rows, j = cols, x = vals, dims = c(n_gene, n_gene))
  W_eNN <- sparse_matrix + t(sparse_matrix)
  toc()
  return(W_eNN)
}
