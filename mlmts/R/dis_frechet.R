
#' Constructs a pairwise distance matrix based on the Frechet distance
#'
#' \code{dis_frechet} returns a pairwise distance matrix based on the Frechet distance
#' between MTS
#'
#' @param X A list of MTS (numerical matrices).
#' @param ... Additional parameters for the function. See  \code{\link[TSclust]{diss.FRECHET}}.
#' @return The computed pairwise distance matrix.
#' @examples
#' toy_dataset <- Libras$data[1 : 5] # Selecting the first 5 MTS from the
#' # dataset Libras
#' distance_matrix <- dis_frechet(toy_dataset) # Computing the pairwise
#' # distance matrix based on the distance dis_frechet
#' @seealso \code{\link[TSclust]{diss.FRECHET}}
#' @details
#' Given a collection of MTS, the function returns the pairwise distance matrix,
#' where the distance between two MTS is defined as the sum of the standard
#' Frechet distances between each corresponding pair of dimensions (univariate
#' time series)
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @export

dis_frechet <- function(X,...) {

  check_mts(X)

  c <- ncol(X[[1]])
  l <- length(X)
  distance_matrix <- matrix(0, nrow = l, ncol = l)

  for (i in 1 : l) {

    if(i > 1) {for (j in 1 : (i - 1)) {

      distance <- numeric(c)

      for (k in 1 : c) {

          distance[k] <- TSclust::diss.FRECHET(X[[i]][,k], X[[j]][,k],...)

      }

      distance_matrix[i, j] <- sum(distance)

    }

    }

  }

  return(stats::as.dist(distance_matrix))


}
