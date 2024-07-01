


#' Constructs a pairwise distance matrix based on feature extraction
#'
#' \code{dis_hwl} returns a pairwise distance matrix based on the feature
#' extraction procedure proposed by \insertCite{hyndman2015large;textual}{mlmts}.
#'
#' @param X A list of MTS (numerical matrices).
#' @param features Logical. If \code{features = FALSE} (default), a distance matrix is returned. Otherwise, the function
#' returns a dataset of feature vectors.
#' @return If \code{features = FALSE} (default), returns a distance matrix based on the distance \eqn{d_{HWL}}. Otherwise, the function
#' returns a dataset of feature vectors, i.e., each row in the dataset contains the features employed to compute the
#' distance \eqn{d_{HWL}}.
#' @examples
#' toy_dataset <- AtrialFibrillation$data[1 : 10] # Selecting the first 10 MTS from the
#' # dataset AtrialFibrillation
#' distance_matrix <- dis_hwl(toy_dataset) # Computing the pairwise
#' # distance matrix based on the distance dis_hwl
#' #' feature_dataset <- dis_hwl(toy_dataset, features = TRUE) # Computing
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
#'   \insertRef{hyndman2015large}{mlmts}
#'
#' }
#' @export

dis_hwl <- function(X, features = FALSE) {

  # Usual checkings

  check_mts(X)
  l <- length(X)
  c <- ncol(X[[1]])


  # Extracting the corresponding features from each MTS (component-wise)

  matrix_hwl <- matrix(0, nrow = l, ncol = c * 18)

  for (i in 1 : l) {

    matrix_hwl[i,] <- c(apply(X[[i]], 2, auxiliary_hwl_function))

  }

  colnames(matrix_hwl) <- rep(c('Mean', 'Variance', 'ACF_1', 'Trend', 'Linearity', 'Curvature',
                                 'Season', 'Peak', 'Trough', 'Entropy', 'Lumpiness', 'Spikiness',
                                 'Lshift', 'Vchange', 'Fspots', 'Cpoints', 'KLscore', 'Change.idx'), c)

  if (features == TRUE) {

    return(matrix_hwl)

  } else {

    stats::dist(matrix_hwl) # Computation of distance matrix

  }


}
