

#' Constructs a pairwise distance matrix based on locality preserving
#' projections (LPP)
#'
#' \code{dis_lpp} returns a pairwise distance matrix based on the
#' dissimilarity introduced by \insertCite{weng2008classificationlpp;textual}{mlmts}.
#'
#' @param X A list of MTS (numerical matrices).
#' @param approach Parameter indicating whether the feature vector representing
#' each MTS is constructed by means of Li's first (\code{approach=1}, default) or Li's
#' second (\code{approach=2}) approach.
#' @param k Number of neighbors determining the construction of the local
#' structure matrix \eqn{\boldsymbol S}.
#' @param t Parameter determining the construction of the local
#' structure matrix \eqn{\boldsymbol S} (denominator in the exponential transformation).
#' @param features Logical. If \code{features = FALSE} (default), a distance matrix is returned. Otherwise, the function
#' returns a dataset of feature vectors.
#' @return If \code{features = FALSE} (default), returns a distance matrix based on the distance \eqn{d_{QCD}}. Otherwise, the function
#' returns a dataset of feature vectors, i.e., each row in the dataset contains the features
#' resulting from applying Li's first (\code{approach=1}) or Li's second (\code{approach=2}).
#' @examples
#' toy_dataset <- AtrialFibrillation$data[1 : 10] # Selecting the first 10 MTS from the
#' # dataset AtrialFibrillation
#' distance_matrix <- dis_lpp(toy_dataset) # Computing the pairwise
#' # distance matrix based on the distance dis_lpp
#' feature_dataset <- dis_lpp(toy_dataset, features = TRUE) # Computing
#' # the corresponding dataset of features
#' @details
#' Given a collection of MTS, the function returns the pairwise distance matrix,
#' where the distance between two MTS \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T}
#' is defined as
#' \deqn{d_{LPP}(\boldsymbol X_T, \boldsymbol Y_T)=
#' \big| \big| {\boldsymbol \varphi^{\boldsymbol X_T}
#' \boldsymbol A_{LPP}  - \boldsymbol \varphi^{\boldsymbol Y_T} \boldsymbol A_{LPP}} \big| \big|,}
#' where \eqn{\boldsymbol \varphi^{\boldsymbol X_T}} and \eqn{\boldsymbol \varphi^{\boldsymbol Y_T}} are the feature
#' vectors constructed from Li's first (\code{approach=1}) or Li's second (\code{approach=2})
#' approach with respect to series \eqn{\boldsymbol X_T}
#' and \eqn{\boldsymbol Y_T}, respectively
#' and \eqn{\boldsymbol A_{LPP}} is the matrix of locality preserving projections
#' whose columns are eigenvectors solving the generalized eigenvalue problem defined
#' by matrix \eqn{\boldsymbol S}.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{weng2008classificationlpp}{mlmts}
#'
#' }
#' @export

dis_lpp <- function(X, approach = 1, k = 2, t = 1, features = FALSE) {

  check_mts(X)
  l <- length(X)
  distance_matrix <- matrix(0, nrow = l, ncol = l)

  list_auxiliary_lpp_function_1 <- auxiliary_lpp_function_1(X, approach, k, t)
  matrix_lpp <- auxiliary_lpp_function_2(list_auxiliary_lpp_function_1$matrix_li,
                                                       list_auxiliary_lpp_function_1$D_matrix,
                                                       list_auxiliary_lpp_function_1$L_matrix)


  if (features == TRUE) {

    return(matrix_lpp)

  } else {

    return(stats::dist(matrix_lpp)) # Computation of distance matrix

  }


}

