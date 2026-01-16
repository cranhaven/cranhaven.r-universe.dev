
#' Constructs a pairwise distance matrix based on maximal cross-correlations
#'
#' \code{dis_mcc} returns a pairwise distance matrix based on an extension of
#' the procedure proposed by \insertCite{egri2017cross;textual}{mlmts}. The
#' function can also be used for dimensionality reduction purposes.
#'
#' @param X A list of MTS (numerical matrices).
#' @param max_lag The maximum number of lags for the computation of the
#' cross-correlations (default is 20).
#' @param delta The threshold value concerning the maximal cross-correlations
#' (default is 0.7).
#' @param features Logical. If \code{features = FALSE} (default), a distance matrix is returned. Otherwise, the function
#' returns a dataset of feature vectors.
#' @return The computed pairwise distance matrix.
#' @examples
#' reduced_dataset <- dis_mcc(RacketSports$data[1], features = TRUE) # Reducing
#' # the dimensionality of the first MTS in dataset RacketSports
#' reduced_dataset
#' distance_matrix <- dis_mcc(Libras$data) # Computing the
#' # corresponding distance matrix for all MTS in dataset Libras
#' # (by default, features = F)
#' @details
#' Given a collection of MTS, the function returns the pairwise distance matrix,
#' where the distance between two MTS \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T} is defined
#' as \deqn{d_{MCC}(\boldsymbol X_{T}, \boldsymbol Y_{T})=\Big|\Big|vec\big(\widehat{\boldsymbol \Theta}^{\boldsymbol X_T}\big)
#' -vec\big(\widehat{\boldsymbol \Theta}^{\boldsymbol Y_T}\big)\Big|\Big|,}
#' where \eqn{\widehat{\boldsymbol \Theta}^{\boldsymbol X_T}} and \eqn{\widehat{\boldsymbol \Theta}^{\boldsymbol Y_T}}
#' are matrices containing pairwise estimated maximal cross-correlations
#' (in absolute value) for series \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T}, respectively,
#' and the operator \eqn{vec(\cdot)} creates a vector by concatenating the columns
#' of the matrix received as input. If we use the function to perform dimensionality
#' reduction (\code{features = TRUE}), then for a given series \eqn{\boldsymbol X_T},
#' a new matrix \eqn{\widehat{\boldsymbol \Theta}^{\boldsymbol X_T}_\delta} is
#' constructed by keeping the entries of matrix \eqn{\widehat{\boldsymbol \Theta}^{\boldsymbol X_T}}
#' which are above \eqn{\delta} (and setting all the remaining entries to zero).
#' The connected components of the graph defined by matrix
#' \eqn{\widehat{\boldsymbol \Theta}^{\boldsymbol X_T}_\delta} are computed
#' along with their corresponding centers (variables). Function \code{dis_mcc}
#' returns the reduced counterpart of \eqn{\boldsymbol X_T}, which is constructed
#' from \eqn{\boldsymbol X_T} by removing all the variables which were not
#' selected as centers of the corresponding components.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{egri2017cross}{mlmts}
#'
#' }
#' @export

dis_mcc <- function(X, max_lag = 20, delta = 0.7, features = F) {

  # Usual checkings

  check_mts(X)
  l <- length(X)
  d <- ncol(X[[1]])
  matrix_mcc <- matrix(0, l, d ^ 2)

  if (features == TRUE) {

    list_reduced_series <- lapply(X, auxiliary_mcc_function_2, max_lag = 20, delta = delta)
    return(list_reduced_series)

  } else {

    list_matrices_mcc <- lapply(X, auxiliary_mcc_function_1, max_lag = max_lag)
    auxiliary_list <- lapply(list_matrices_mcc, c)

    for (i in 1 : l) {

      matrix_mcc[i,] <- auxiliary_list[[i]]

    }

    return(stats::dist(matrix_mcc))

  }




}
