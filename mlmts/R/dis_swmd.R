

#' Constructs a pairwise distance matrix based on VPCA and SWMD
#'
#' \code{dis_swmd} returns a pairwise distance matrix based on variable-based
#' principal component analysis (VPCA) and a spatial weighted matrix distance
#' (SWMD) \insertCite{he2018unsupervised}{mlmts}.
#'
#' @param X A list of MTS (numerical matrices).
#' @param var_rate Rate of retained variability concerning the
#' dimensionality-reduced MTS samples (default is 0.90).
#' @param features Logical. If \code{features = FALSE} (default), a distance matrix is returned. Otherwise, the function
#' returns a dataset of feature vectors.
#' @return If \code{features = FALSE} (default), returns a distance matrix based on the distance \eqn{d_{SWMD}}. Otherwise, the function
#' returns a dataset of feature vectors, i.e., each row in the dataset contains the features employed to compute the
#' distance \eqn{d_{SWMD}}.
#' @examples
#' toy_dataset <- AtrialFibrillation$data[1 : 10] # Selecting the first 10 MTS from the
#' # dataset AtrialFibrillation
#' distance_matrix <- dis_swmd(toy_dataset) # Computing the pairwise
#' # distance matrix based on the distance dis_swmd
#' feature_dataset <- dis_swmd(toy_dataset, features = TRUE) # Computing
#' # the corresponding dataset of features
#' @details
#' Given a collection of MTS, the function returns the pairwise distance matrix,
#' where the distance between two MTS \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T} is defined
#' as  \deqn{d_{SWMD}(\boldsymbol X_T, \boldsymbol Y_T)=\Big[\big(vec
#' (\boldsymbol Z^{\boldsymbol X_T})-vec(\boldsymbol Z^{\boldsymbol Y_T})\big)\boldsymbol
#' S\big(vec(\boldsymbol Z^{\boldsymbol X_T})-vec(\boldsymbol Z^{\boldsymbol Y_T})\big)^\top\Big]^{1/2},}
#' where \eqn{\boldsymbol Z^{\boldsymbol X_T}} and \eqn{\boldsymbol Z^{\boldsymbol Y_T}} are the dimensionality-
#' reduced MTS samples associated with \eqn{\boldsymbol X_T} and
#' \eqn{\boldsymbol Y_T}, respectively, the operator \eqn{vec(\cdot)}
#' creates a vector by concatenating the columns of the matrix received as input
#' and \eqn{\boldsymbol S} is a matrix integrating the spatial dimensionality
#' difference between the corresponding elements.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{he2018unsupervised}{mlmts}
#'
#' }
#' @seealso
#' \code{\link{vpca_clustering}}
#' @export


dis_swmd <- function(X, var_rate = 0.90, features = FALSE) {

  check_mts(X)
  l <- length(X)
  c <- ncol(X[[1]])


  # Obtaining the list of reduced matrices and the number p_s

  auxiliary_object <- auxiliary_vpca_function_1(X, var_rate = var_rate)
  list_reduced_matrices <- auxiliary_object$Y
  p_s <- auxiliary_object$p_s


  # Obtaining the matrix S

  S <- auxiliary_vpca_function_2(c, p_s)


  # Obtaining the distance matrix

  distance_matrix <- matrix(0, nrow = l, ncol = l)

  for (i in 1 : l) {

    if(i > 1) {for (j in 1 : (i - 1)) {

      distance_matrix[i, j] <- auxiliary_vpca_function_3(list_reduced_matrices[[i]],
                                                         list_reduced_matrices[[j]],
                                                         S = S)

    }

    }

  }


  if (features == TRUE) {

    return(listTomatrix(list_reduced_matrices))

  } else {

    return(stats::as.dist(distance_matrix)) # Computation of distance matrix

  }





}

