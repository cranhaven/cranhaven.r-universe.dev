
#' Constructs a pairwise distance matrix relying on a piecewise representation
#' based on PCA
#'
#' \code{dis_ppca} returns a pairwise distance matrix based on an extension of
#' the procedure proposed by \insertCite{wan2022dimensionality;textual}{mlmts}. The
#' function can also be used for dimensionality reduction purposes.
#'
#' @param X A list of MTS (numerical matrices).
#' @param w The number of segments (in the time dimension) in which we want to
#' divide the MTS (default is 2).
#' @param var_rate Rate of retained variability concerning the
#' dimensionality-reduced MTS samples (default is 0.90).
#' @param features Logical. If \code{features = FALSE} (default), a distance matrix is returned. Otherwise, the function
#' returns a dataset of feature vectors.
#' @return The computed pairwise distance matrix.
#' @examples
#' reduced_dataset <- dis_ppca(RacketSports$data[1], features = TRUE) # Reducing
#' # the dimensionality of the first MTS in dataset RacketSports
#' reduced_dataset
#' distance_matrix <- dis_ppca(RacketSports$data) # Computing the
#' # corresponding distance matrix for all MTS in dataset RacketSports
#' # (by default, features = F)
#' @details
#' Given a collection of MTS, the function returns the pairwise distance matrix,
#' where the distance between two MTS \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T} is defined
#' as \deqn{d_{PPCA}(\boldsymbol X_{T}, \boldsymbol Y_{T})=\Big|\Big|vec\big(\widehat{\boldsymbol \Sigma}_a ^{\boldsymbol X_T}\big)
#' -vec\big(\widehat{\boldsymbol \Sigma}_a^{\boldsymbol Y_T}\big)\Big|\Big|,}
#' where \eqn{\widehat{\boldsymbol \Sigma}_a ^{\boldsymbol X_T}} and \eqn{\widehat{\boldsymbol \Sigma}_a ^{\boldsymbol Y_T}}
#' are estimates of the covariance matrices based on a piecewise representation for which the
#' original MTS \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T}, respectively,
#' are divided into a number of \code{w} local segments (in the time dimension).
#' If we use the function to perform dimensionality reduction (\code{features = TRUE}),
#' then for a given series \eqn{\boldsymbol X_T}, matrix \eqn{\widehat{\boldsymbol \Sigma}_a ^{\boldsymbol X_T}}
#' is decomposed by executing the standard PCA and a certain number of
#' principal components are retained (according to the parameter \code{var_rate}).
#' Function \code{dis_ppca} returns the reduced counterpart of \eqn{\boldsymbol X_T},
#' which is constructed from \eqn{\boldsymbol X_T} by considering the
#' matrix of scores with respect to the retained principal components.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{wan2022dimensionality}{mlmts}
#'
#' }
#' @export

dis_ppca <- function(X, w = 2, var_rate = 0.90, features = F) {

  # Usual checkings

  check_mts(X)
  l <- length(X)
  d <- ncol(X[[1]])
  matrix_ppca <- matrix(0, l, d ^ 2)


  if (features == TRUE) {

    list_reduced_series <- lapply(X, auxiliary_ppca_function_2, w = w, var_rate = var_rate)
    return(list_reduced_series)

  } else {

    list_matrices_ppca <- lapply(X, auxiliary_ppca_function_1, w = w)
    auxiliary_list <- lapply(list_matrices_ppca, c)

    for (i in 1 : l) {

      matrix_ppca[i,] <- auxiliary_list[[i]]

    }

    return(stats::dist(matrix_ppca))

  }




}
