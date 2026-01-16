


#' Constructs a pairwise distance matrix based on the Eros distance measure
#'
#' \code{dis_eros} returns a pairwise distance matrix based on the Eros distance
#' proposed by \insertCite{yang2004pca;textual}{mlmts}.
#'
#' @param X A list of MTS (numerical matrices).
#' @param method The aggregated function to compute the weights.
#' @param normalization Logical indicating whether the raw eigenvalues or the
#' normalized eigenvalues should be used to compute the weights. Default is
#' \code{FALSE}, i.e., the raw eigenvalues are used.
#' @param cor Logical indicating whether the Singular Value Decomposition is
#' applied over the covariance matrix or over the correlation matrix. Default
#' is \code{TRUE}, i.e., the correlation matrix is employed to avoid issues of
#' scale.

#' @return The computed pairwise distance matrix.
#' @examples
#' toy_dataset <- BasicMotions$data[1 : 10] # Selecting the first 10 MTS from the
#' # dataset BasicMotions
#' distance_matrix <- dis_eros(toy_dataset) # Computing the pairwise
#' # distance matrix based on the distance dis_eros
#' distance_matrix <- dis_eros(toy_dataset, method = 'max', normalization = TRUE)
#' # Considering the function max as aggregation function and the normalized
#' # eigenvalues for the computation of the weights
#' @details
#' Given a collection of MTS, the function returns the pairwise distance matrix,
#' where the distance between two MTS \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T} is defined
#' as  \eqn{d_{Eros}(\boldsymbol X_T, \boldsymbol Y_T)=\sqrt{2-2Eros(\boldsymbol X_T, \boldsymbol Y_T)}},
#' where \deqn{Eros(\boldsymbol X_T, \boldsymbol Y_T)=\sum_{i=1}^{d}w_i|<\boldsymbol x_i,\boldsymbol y_i>|=
#' \sum_{i=1}^{d}w_i|\cos \theta_i|,}
#' where \eqn{\{\boldsymbol x_1, \ldots, \boldsymbol x_d\}}, \eqn{\{\boldsymbol y_1, \ldots, \boldsymbol y_d\}}
#' are sets of eigenvectors concerning the covariance or correlation matrix of series \eqn{\boldsymbol X_T} and
#' \eqn{\boldsymbol Y_T}, respectively, \eqn{<\boldsymbol x_i,\boldsymbol y_i>} is the inner product of
#' \eqn{\boldsymbol x_i} and \eqn{\boldsymbol y_i}, \eqn{\boldsymbol w=(w_1, \ldots, w_d)}
#' is a vector of weights which is based on the eigenvalues of the MTS dataset with \eqn{\sum_{i=1}^{d}w_i=1}
#' and \eqn{\theta_i} is the angle between \eqn{\boldsymbol x_i} and \eqn{\boldsymbol y_i}.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{yang2004pca}{mlmts}
#'
#' }
#' @export

dis_eros <- function(X, method = 'mean', normalization = FALSE, cor = TRUE) {

  # Usual checkings

  check_mts(X)
  l <- length(X)


  # Computing the weights and the eigenvector matrices

  auxiliary_results <- auxiliary_eros_function_1(X, method = method, normalization = normalization, cor = cor)
  auxiliary_weights <- auxiliary_results$weights
  auxiliary_eigenvector_matrices <- auxiliary_results$eigenvectors_matrices

  # Computing EROS distance matrix

  eros_distance_matrix <- matrix(0, nrow = l, ncol = l)


  for (i in 1 : l) {

    if(i > 1) {for (j in 1 : (i - 1)) {

      eros_distance_matrix[i, j] <- auxiliary_eros_function_2(auxiliary_eigenvector_matrices[[i]],
                                                              auxiliary_eigenvector_matrices[[j]],
                                                              auxiliary_weights)

    }

    }

  }


  return(stats::as.dist(eros_distance_matrix))



}
