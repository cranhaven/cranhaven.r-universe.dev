

#' Constructs a pairwise distance matrix based on two-dimensional singular
#' value decomposition (2dSVD)
#'
#' \code{dis_2dsvd} returns a pairwise distance matrix based on the 2dSVD
#' distance measure proposed by \insertCite{weng2008classification;textual}{mlmts}.
#'
#' @param X A list of MTS (numerical matrices).
#' @param var_u Rate of retained variability concerning the row-row covariance
#' matrix.
#' @param var_v Rate of retained variability concerning the column-column
#' covariance matrix.
#' @param features Logical. If \code{features = FALSE} (default), a distance matrix is returned. Otherwise, the function
#' returns a dataset of feature vectors.
#' @return If \code{features = FALSE} (default), returns a distance matrix based on the distance \eqn{d_{2dSVD}}. Otherwise, the function
#' returns a dataset of feature vectors, i.e., each row in the dataset contains the features employed to compute the
#' distance \eqn{d_{2dSVD}}.
#' @examples
#' toy_dataset <- BasicMotions$data[1 : 10] # Selecting the first 10 MTS from the
#' # dataset BasicMotions
#' distance_matrix <- dis_2dsvd(toy_dataset) # Computing the pairwise
#' # distance matrix based on the distance dis_2dsvd
#' feature_dataset <- dis_2dsvd(toy_dataset, features = TRUE) # Computing
#' # the corresponding dataset of features
#' @details
#' Given a collection of MTS, the function returns the pairwise distance matrix,
#' where the distance between two MTS \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T} is defined
#' as
#' \deqn{d_{2dSVD}(\boldsymbol X_T, \boldsymbol Y_T)=\sum_{b=1}^s||{\boldsymbol M}^{\boldsymbol X_T}_{\bullet, b}-
#' {\boldsymbol M}^{\boldsymbol Y_T}_{\bullet, b}||,}
#' where \eqn{{\boldsymbol M}^{\boldsymbol X_T}_{\bullet, b}} and \eqn{{\boldsymbol M}^{\boldsymbol Y_T}_{\bullet, b}} are the
#' \eqn{b}th columns of matrices \eqn{{\boldsymbol M}^{\boldsymbol X_T}}
#' and \eqn{{\boldsymbol M}^{\boldsymbol Y_T}}, which are obtained by
#' decomposing the time series \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T}, respectively,
#' by means of the 2dSVD procedure (average row-row and column-column covariance matrices
#' are taken into account), and \eqn{s} is the number of first retained eigenvectors
#' concerning the average column-column covariance matrices.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{weng2008classification}{mlmts}
#'
#' }
#' @export

dis_2dsvd <- function(X, var_u = 0.90, var_v = 0.90, features = FALSE) {

  check_mts(X)
  l <- length(X)
  distance_matrix <- matrix(0, nrow = l, ncol = l)
  feature_matrices <- auxiliary_2dsvd_function_1(X, var_u, var_v)


  if (features == TRUE) {

    feature_vectors <- listTomatrix(lapply(feature_matrices, c))
    return(feature_vectors)

  } else {

    distance_matrix <- matrix(0, nrow = l, ncol = l)

    for (i in 1 : l) {

      if(i > 1) {for (j in 1 : (i - 1)) {

        distance_matrix[i, j] <- auxiliary_2dsvd_function_2(M1 = feature_matrices[[i]],
                                                            M2 = feature_matrices[[j]])

      }

      }

    }

    return(stats::as.dist(distance_matrix))

  }





}
