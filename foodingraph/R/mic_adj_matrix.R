#' Computes a MIC adjacency matrix
#'
#' For a given dataset, computes the adjacency matrix
#' of maximal information coefficient (MIC) of each
#' pairwise association.
#' NOTE : another approach could have been to give the whole
#'  data frame to the \code{minerva} package func \code{cstats()},
#'  but it seemed slower in my tests.
#'
#' @param obs_data (data.frame or matrix) : a dataset which rows are
#'    observations and columns the variables.
#'
#' @return the adjacency matrix of MIC values for each pairwise association.
#' @examples
#' mic_adj_matrix(iris[,-5])
#' @references
#' Reshef et al. (2011) <doi:10.1126/science.1205438>
#' @importFrom minerva cstats
#' @export
mic_adj_matrix <- function(obs_data) {

  n_var <- ncol(obs_data)

  # Creating the adjacency matrix
  adj_matrix <- matrix(0, nrow = n_var, ncol = n_var)
  colnames(adj_matrix) <- rownames(adj_matrix) <- colnames(obs_data)

  # For each pair of variables
  for (i in 1:n_var) {
    for (j in 1:n_var) {
      if (adj_matrix[i,j] == 0 && !is.na(adj_matrix[i,j])) {
        mic_value <- cstats(as.matrix(obs_data[,i]),
                            as.matrix(obs_data[,j]))

        adj_matrix[i,j] <- mic_value[,3]
        adj_matrix[j,i] <- mic_value[,3]
      }
    }
  }

  adj_matrix
}
