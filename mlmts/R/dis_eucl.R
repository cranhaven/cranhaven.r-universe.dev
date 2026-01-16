
#' Constructs a pairwise distance matrix based on the Euclidean distance
#'
#' \code{dis_eucl} returns a pairwise distance matrix based on the Euclidean distance
#' between MTS
#'
#' @param X A list of MTS (numerical matrices).
#' @return The computed pairwise distance matrix.
#' @examples
#' toy_dataset <- AtrialFibrillation$data[1 : 10] # Selecting the first 10 MTS from the
#' # dataset AtrialFibrillation
#' distance_matrix <- dis_eucl(toy_dataset) # Computing the pairwise
#' # distance matrix based on the distance dis_eucl
#' @details
#' Given a collection of MTS, the function returns the pairwise distance matrix,
#' where the distance between two MTS is defined as the sum of the standard
#' Euclidean distances between each corresponding pair of dimensions (univariate
#' time series)
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @export

dis_eucl <- function(X) {

  check_mts(X)

  l <- length(X)
  distance_matrix <- matrix(0, nrow = l, ncol = l)

  for (i in 1 : l) {

  if(i > 1) {for (j in 1 : (i - 1)) {

      distance_matrix[i, j] <- sum(diag(Rfast::dista(t(X[[i]]), t(X[[j]]))))

    }

  }

  }

  return(stats::as.dist(distance_matrix))

}
