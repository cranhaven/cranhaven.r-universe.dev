
matrix_power <- function(x, k, ...) {
  Reduce(`%*%`, rep(list(x), k), ...)
}

matrix_summation <- function(mat_list) {
  Reduce(`+`, mat_list, accumulate = FALSE)
}

#' Compute transition matrix
#' 
#' @param x sparseMatrix or denseMatrix
#' 
#' @return transition matrix
compute_transition_matrix <- function(x) {
      x / Matrix::rowSums(x)
}

#' Construct sparse matrix from weighted edgelist
#' 
#' Takes the weights from compute_kernel and creates weighted adjacency matrix
#' 
#' @param edgelist a dataframe with two columns
#' @param weights a vector of weights
#' @param ... other parameters to be passed to Matrix::sparseMatrix()
#' 
#' @return sparseMatrix
create_weight_matrix <- function(edgelist, weights, ...) {
  Matrix::sparseMatrix(
    i = edgelist[,1],
    j = edgelist[,2],
    x = weights,
    ...
  )
}

#' Update edge weights
#' 
#' @param M matrix
#' @param edgelist dataframe representing weighted edgelist
#' @param similarity a similarity function
#' @param k integer, length of longest walk
#' 
#' @return list
update_weights <- function(M, edgelist, similarity, k) {
  
  M <- compute_transition_matrix(M)
  Mk <- matrix_summation(
    matrix_power(M, k, accumulate = TRUE)
  )
  
  weights <- compute_similarities(edgelist, Mk, similarity = similarity, k = k)
  adj <- create_weight_matrix(edgelist, weights, symmetric = TRUE, check = TRUE)

  return(list(weights = weights, adj = adj))

}

#' Execute main algorithm loop
#' 
#' @param M transition matrix
#' @param edgelist dataframe edgelist
#' @param similarity a similarity function
#' @param k integer, length of longest walk
#' @param iter number of iterations
#' 
#' @return list
run_main_loop <- function(M, edgelist, similarity, k, iter) {

  if (!is.numeric(iter) || iter < 1) {
    stop("Invalid value for iter")
  }

  for (i in 1:iter) {
    results <- update_weights(M = M, edgelist = edgelist, similarity = similarity, k = k)
    M <- results[["adj"]]
  }

  return(new_rwclust(results))
}
