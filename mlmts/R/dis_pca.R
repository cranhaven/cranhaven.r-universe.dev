

#' Constructs a pairwise distance matrix based on Principal Component Analysis (PCA)
#'
#' \code{dis_eros} returns a pairwise distance matrix based on the
#' PCA similarity factor proposed by \insertCite{singhal2005clustering;textual}{mlmts}.
#'
#' @param X A list of MTS (numerical matrices).
#' @param retained_components Number of retained principal components.
#' @return The computed pairwise distance matrix.
#' @examples
#' toy_dataset <- BasicMotions$data[1 : 10] # Selecting the first 10 MTS from the
#' # dataset BasicMotions
#' distance_matrix <- dis_pca(toy_dataset) # Computing the pairwise
#' # distance matrix based on the distance dis_pca
#' @details
#' Given a collection of MTS, the function returns the pairwise distance matrix,
#' where the distance between two MTS \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T} is defined
#' as \eqn{d_{PCA}(\boldsymbol X_{T}, \boldsymbol Y_{T})=1-S_{PCA}
#' (\boldsymbol X_{T}, \boldsymbol Y_{T})}, with
#' \deqn{S_{PCA}(\boldsymbol X_{T}, \boldsymbol Y_{T})=\frac{\sum_{i=1}^{k}\sum_{j=1}^{k}
#' (\lambda^i_{\boldsymbol X_T}
#' \lambda^j_{\boldsymbol Y_T})\cos^2 \theta_{ij}}{\sum_{i=1}^{k}
#' \lambda^i_{\boldsymbol X_T} \lambda^i_{\boldsymbol Y_T}},}
#' where \eqn{\theta_{ij}} is the angle between the \eqn{i}th eigenvector of
#' \eqn{\boldsymbol X_{T}} and the \eqn{j}th eigenvector of series \eqn{\boldsymbol Y_{T}},
#' respectively, and \eqn{\lambda^i_{\boldsymbol Y_T}} and \eqn{\lambda^i_{\boldsymbol Y_T}}
#' are the \eqn{i}th eigenvalues of \eqn{\boldsymbol X_{T}} and the
#' \eqn{j}th eigenvalues of series \eqn{\boldsymbol Y_{T}} respectively.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{singhal2005clustering}{mlmts}
#'
#' }
#' @export

dis_pca <- function(X, retained_components = 3) {

  # Usual checkings

  check_mts(X)
  l <- length(X)
  c <- ncol(X[[1]])

  if (retained_components > c) {

    stop('The number of selected principal components is larger than the dimensions of the MTS')

  }

  # Computation of the distance matrix

  list_cov_matrices <- lapply(X, stats::cov)

  dis_matrix_pca <- matrix(0, l, l)

  for (i in 1 : l) {

    for(j in 1 : l) {

      dis_matrix_pca[i, j] <- 1 - PCAsimilarity(list_cov_matrices[[i]],
                                                list_cov_matrices[[j]], ret.dim = retained_components)

    }

  }

  dis_matrix_pca[col(dis_matrix_pca) >= row(dis_matrix_pca)] <- 0
  stats::as.dist(dis_matrix_pca)

}


