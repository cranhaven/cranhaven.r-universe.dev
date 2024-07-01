

auxiliary_mcc_function_1 <- function(X, max_lag = 20) {


  series_length <- nrow(X)
  d <- ncol(X)


  # Constructing the matrix of maximal cross-correlations

  matrix_mcc <- matrix(0, d, d)

  for (i in 1 : d) {

    for (j in 1 : d) {

      matrix_mcc[i, j] <- max(abs(stats::ccf(X[,i], X[,j], lag.max = max_lag, plot = F)$acf))

    }

  }

  return(matrix_mcc)

}



auxiliary_mcc_function_2 <- function(X, max_lag = 20, delta = 0.5) {


  series_length <- nrow(X)
  d <- ncol(X)


  # Computing the matrix of maximal cross-correlation and the corrected matrix

  matrix_mcc <- auxiliary_mcc_function_1(X, max_lag = max_lag)
  indexes_corrected_matrix <- which(matrix_mcc <= delta)
  corrected_matrix_mcc <- matrix_mcc
  corrected_matrix_mcc[indexes_corrected_matrix] <- 0


  # Computing the connected components of the corresponding graph

  graph_mcc <- igraph::graph_from_adjacency_matrix(corrected_matrix_mcc, mode = 'undirected', weighted = T)
  components_mcc <- igraph::components(graph_mcc)


  # Computing the center of each connected component

  number_components <- components_mcc$no
  indexes_components <- components_mcc$membership
  indexes_variables <- 1 : d
  centers <- numeric()

  for (i in  1 : number_components) {

    indexes_component <- which(indexes_components == i)
    vector_sums <- base::colSums(corrected_matrix_mcc)[indexes_component]
    index_center <- which.max(vector_sums)
    centers[i] <- indexes_component[index_center]

  }


  # Obtaining the reduced MTS

  X_reduced <- as.matrix(X[,centers])
  return(X_reduced)

}
