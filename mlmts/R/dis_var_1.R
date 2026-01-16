


#' Constructs a pairwise distance matrix based on the estimated VAR coefficients
#' of the series
#'
#' \code{dis_cor} returns a pairwise distance matrix based on a generalization of the
#' dissimilarity introduced by \insertCite{piccolo1990distance;textual}{mlmts}.
#'
#' @param X A list of MTS (numerical matrices).
#' @param max_p The maximum order considered with respect to the fitting of VAR models.
#' @param criterion The criterion used to determine the VAR order.
#' @param features Logical. If \code{features = FALSE} (default), a distance matrix is returned. Otherwise, the function
#' returns a dataset of feature vectors.
#' @return If \code{features = FALSE} (default), returns a distance matrix based on the distance \eqn{d_{COR}}. Otherwise, the function
#' returns a dataset of feature vectors, i.e., each row in the dataset contains the features employed to compute the
#' distance \eqn{d_{VAR}}.
#' @examples
#' toy_dataset <- Libras$data[1 : 2] # Selecting the first 2 MTS from the
#' # dataset Libras
#' distance_matrix <- dis_var_1(toy_dataset) # Computing the pairwise
#' # distance matrix based on the distance dis_var_1
#' feature_dataset <- dis_var_1(toy_dataset, features = TRUE) # Computing
#' # the corresponding dataset of features
#' @details
#' Given a collection of MTS, the function returns the pairwise distance matrix,
#' where the distance between two MTS \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T} is defined
#' as
#' \deqn{d_{VAR}(\boldsymbol X_T, \boldsymbol Y_T)=||\widehat{\boldsymbol \theta}^{\boldsymbol X_T}_{VAR}-
#' \widehat{\boldsymbol \theta}^{\boldsymbol Y_T}_{VAR}||,}
#' where \eqn{\widehat{\boldsymbol \theta}^{\boldsymbol X_T}_{VAR}} and \eqn{\widehat{\boldsymbol \theta}^{\boldsymbol Y_T}_{VAR}} are vectors
#' containing the estimated VAR parameters for \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T}, respectively. If VAR models of
#' different orders are fitted to \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T}, then the shortest
#' vector is padded with zeros until it reaches the length of the longest vector.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{piccolo1990distance}{mlmts}
#'
#' }
#' @seealso
#' \code{\link{dis_var_2}}, \code{\link[TSclust]{diss.AR.PIC}}
#' @export

dis_var_1 <- function(X, max_p = 1, criterion = 'AIC', features = FALSE) {

  # Usual checkings

  check_mts(X)
  l <- length(X)
  c <- ncol(X[[1]])


  # Computing the dataset of features

  var_matrix <- auxiliary_var_1_function(X, max_p, criterion)


  if (features == TRUE) {

    var_matrix

  } else {

    stats::dist(var_matrix) # Computation of distance matrix

  }



}
