#' Improved SVD Imputation
#'
#' This function performs imputation using Singular Value Decomposition (SVD) with iterative refinement.
#' It begins by filling missing values with the mean of their respective columns.
#' Then, it computes a low-rank (k) approximation of the data matrix. Using this approximation,
#' it refills the missing values. This process of recomputing the rank-k approximation with
#' the newly imputed values and refilling the missing data is repeated for a specified number
#' of iterations, `num.iters`.
#'
#' @param x A data frame or matrix where each row represents a different record.
#' @param k The rank-k approximation to use for the data matrix.
#' @param num.iters The number of times to compute the rank-k approximation and impute the missing data.
#' @param verbose If TRUE, print status updates during the process.
#' @return A list containing:
#' \item{data.matrix}{The imputed matrix with missing values filled.}
#' @examples
#' # Create a sample matrix with random values and introduce missing values
#' x = matrix(rnorm(100), 10, 10)
#' x[x > 1] = NA
#'
#' # Perform SVD imputation
#' imputed_x = SVDImpute(x, 3)
#'
#' # Print the imputed matrix
#' print(imputed_x)
#' @export
#'
SVDImpute <- function(x, k, num.iters = 10, verbose = TRUE) {
  # Convert the data to a matrix if it's not already
  data.matrix <- as.matrix(x)

  # Get the dimensions of the data
  n <- nrow(data.matrix)
  p <- ncol(data.matrix)

  # Initial imputation using column means
  for (j in 1:p) {
    data.matrix[is.na(data.matrix[, j]), j] <- base::mean(data.matrix[!is.na(data.matrix[, j]), j], na.rm = TRUE)
  }

  # Iterative process
  for (iter in 1:num.iters) {
    if (verbose) {
      cat("Iteration", iter, ": Performing SVD and imputation\n")
    }

    # Perform SVD
    svd.res <- svd(data.matrix, nu = k, nv = k)

    # Construct the rank-k approximation
    Uk <- svd.res$u
    Dk <- diag(svd.res$d[1:k])
    Vk <- svd.res$v
    data.approx <- Uk %*% Dk %*% t(Vk)

    # Impute missing values using the rank-k approximation
    data.matrix[is.na(data.matrix)] <- data.approx[is.na(data.matrix)]
  }

  # Return the imputed data
  return(data.matrix)
}
