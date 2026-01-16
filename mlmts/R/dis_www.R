

#' Constructs a pairwise distance matrix based on feature extraction
#'
#' \code{dis_www} returns a pairwise distance matrix based on the feature
#' extraction procedure proposed by \insertCite{wang2007structure;textual}{mlmts}.
#'
#' @param X A list of MTS (numerical matrices).
#' @param h Maximum lag for the computation of the Box-Pierce statistic.
#' @param features Logical. If \code{features = FALSE} (default), a distance matrix is returned. Otherwise, the function
#' returns a dataset of feature vectors.
#' @return If \code{features = FALSE} (default), returns a distance matrix based on the distance \eqn{d_{WWW}}. Otherwise, the function
#' returns a dataset of feature vectors, i.e., each row in the dataset contains the features employed to compute the
#' distance \eqn{d_{WWW}}.
#' @examples
#' toy_dataset <- AtrialFibrillation$data[1 : 10] # Selecting the first 10 MTS from the
#' # dataset AtrialFibrillation
#' distance_matrix <- dis_www(toy_dataset) # Computing the pairwise
#' # distance matrix based on the distance dis_www
#' feature_dataset <- dis_www(toy_dataset, features = TRUE) # Computing
#' # the corresponding dataset of features
#' @details
#' Given a collection of MTS, the function returns the pairwise distance matrix,
#' where the distance between two MTS is defined as the Euclidean distance
#' between the corresponding feature vectors
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{wang2007structure}{mlmts}
#'
#' }
#' @export


dis_www <- function(X, h = 20, features = FALSE) {


  # Usual checkings

  check_mts(X)
  l <- length(X)
  c <- ncol(X[[1]])


  # Extracting the corresponding features from each MTS (component-wise)

  matrix_www <- matrix(0, nrow = l, ncol = 12 * c)

  for (i in 1 : l) {

    matrix_www[i,] <- auxiliary_www_function(X[[i]], h = h)

  }

  matrix_www[,(3 * c + 1) : (3 * c + 2 * c)] <- auxiliary_function_transformation_1(matrix_www[,(3 * c + 1) : (3 * c + 2 * c)],
                                                                                a = 0.069, b = 2.304)
  matrix_www[,(5 * c + 1) : (5 * c + 2 * c)] <- auxiliary_function_transformation_1(matrix_www[,(5 * c + 1) : (5 * c + 2 * c)],
                                                                                a = 0.069, b = 2.304)
  matrix_www[,(7 * c + 1) : (7 * c + 2 * c)] <- auxiliary_function_transformation_1(matrix_www[,(7 * c + 1) : (7 * c + 2 * c)],
                                                                                  a = 1.510, b = 5.993)
  matrix_www[,(9 * c + 1) : (9 * c + 2 * c)] <- auxiliary_function_transformation_1(matrix_www[,(9 * c + 1) : (9 * c + 2 * c)],
                                                                                  a = 2.273, b = 11567)

  if (features == TRUE) {

    return(matrix_www)

  } else {

    stats::dist(matrix_www) # Computation of distance matrix

  }






}
