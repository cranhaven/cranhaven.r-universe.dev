


#' Constructs a pairwise distance matrix based on feature extraction
#'
#' \code{dis_zagorecki} returns a pairwise distance matrix based on the feature
#' extraction procedure proposed by \insertCite{zagorecki2015versatile;textual}{mlmts}.
#'
#' @param set A list of MTS (numerical matrices).
#' @param features Logical. If \code{features = FALSE} (default), a distance matrix is returned. Otherwise, the function
#' returns a dataset of feature vectors.
#' @return If \code{features = FALSE} (default), returns a distance matrix based on the distance \eqn{d_{ZAGORECKI}}. Otherwise, the function
#' returns a dataset of feature vectors, i.e., each row in the dataset contains the features employed to compute the
#' distance \eqn{d_{ZAGORECKI}}.
#' @examples
#' toy_dataset <- AtrialFibrillation$data[1 : 10] # Selecting the first 10 MTS from the
#' # dataset AtrialFibrillation
#' distance_matrix <- dis_zagorecki(toy_dataset) # Computing the pairwise
#' # distance matrix based on the distance dis_zagorecki
#' feature_dataset <- dis_zagorecki(toy_dataset, features = TRUE) # Computing
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
#'   \insertRef{zagorecki2015versatile}{mlmts}
#'
#' }
#' @export


dis_zagorecki <- function(set, features = FALSE) {

  # Usual checkings

  check_mts(set)
  check_mts_rows(set)
  l <- length(set)
  c <- ncol(set[[1]])

  # Extracting the corresponding features from each MTS (component-wise)
  n_col_first <- length(auxiliary_zagorecki_function_2(set[[1]]))
  matrix_zagorecki <- matrix(0, nrow = l, ncol = n_col_first)

  for (i in 1 : l) {

    matrix_zagorecki[i,] <- auxiliary_zagorecki_function_2(set[[i]])

  }


  if (features == TRUE) {

    return(matrix_zagorecki)

  } else {

    return(stats::dist(matrix_zagorecki)) # Computation of distance matrix

  }


}
