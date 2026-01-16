
#' Constructs a pairwise distance matrix based on estimated
#' spectral matrices
#'
#' \code{dis_spectral} returns a pairwise distance matrix based on the
#' dissimilarities introduced by \insertCite{kakizawa1998discrimination;textual}{mlmts}.
#' @param X A list of MTS (numerical matrices).
#' @param method Parameter indicating the method to be used for the computation
#' of the distance. If \code{method="j_divergence"} (default), the J divergence is
#' considered. If \code{method="chernoff_divergence"}, the Chernoff information divergence
#' is considered
#' @param alpha If \code{method="chernoff_divergence"}, parameter alpha in (0,1) used for the computation of the
#' Chernoff divergence (default is 0.5).
#' @param features Logical. If \code{features = FALSE} (default), a distance matrix is returned. Otherwise, the function
#' returns a dataset of feature vectors.
#' @return If \code{features = FALSE} (default), returns a distance matrix based on the distance
#' \eqn{d_{JSPEC}} as long as we set \code{method="j_divergence"}, and based on the alternative distance \eqn{d_{CSPEC}} as long as we set \code{method=}
#' \code{"chernoff_divergence"}.
#' Otherwise, if \code{features = TRUE}, the function returns a dataset of feature vectors, i.e., each row in the dataset
#' contains the features employed to compute either \eqn{d_{JSPEC}} or \eqn{d_{CSPEC}}. These vectors
#' are vectorized versions of the estimated spectral matrices.
#' @examples
#' toy_dataset <- Libras$data[1 : 10] # Selecting the first 10 MTS from the
#' # dataset Libras
#' distance_matrix_j <- dis_spectral(toy_dataset) # Computing the pairwise
#' # distance matrix based on the distance dis_jspec
#' distance_matrix_c <- dis_spectral(toy_dataset,
#' method = 'chernoff_divergence') # Computing the pairwise
#' # distance matrix based on the distance dis_cspec
#' feature_dataset <- dis_qcd(toy_dataset, features = TRUE) # Computing
#' # the corresponding dataset of features for d_cpec
#' @details
#' Given a collection of MTS, the function returns a pairwise distance matrix. If \code{method="j_divergence"}
#' then the distance between two MTS \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T} is defined as
#' \deqn{d_{JSPEC}(\boldsymbol X_T, \boldsymbol Y_T)=\frac{1}{2T}
#' \sum_{k=1}^{K}\bigg(tr\Big(\widehat{\boldsymbol f}_{\boldsymbol X_T}(\omega_k)
#' \widehat{\boldsymbol f}_{\boldsymbol Y_T}^{-1}(\omega_k)\Big)
#' +tr\Big(\widehat{\boldsymbol f}_{\boldsymbol Y_T}(\omega_k)
#' \widehat{\boldsymbol f}_{\boldsymbol X_T}^{-1}(\omega_k)\Big)-2d\bigg),}
#' where \eqn{\widehat{\boldsymbol f}_{\boldsymbol X_T}(\omega_k)} and
#' \eqn{\widehat{\boldsymbol f}_{\boldsymbol Y_T}(\omega_k)} are the estimated
#' spectral density matrices from the series \eqn{\boldsymbol X_T} and
#' \eqn{\boldsymbol Y_T}, respectively, evaluated at frequency \eqn{\omega_k},
#' and \eqn{tr(\cdot)} denotes the trace of a square matrix. If
#' \code{method="chernoff_divergence"},  then the distance between two MTS
#' \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T} is defined as
#' \deqn{d_{CSPEC}(\boldsymbol X_T, \boldsymbol Y_T)=}
#' \deqn{\frac{1}{2T}
#' \sum_{k=1}^{K}\bigg(\log{\frac{\Big|\alpha\widehat{\boldsymbol f}^{\boldsymbol X_T}(\omega_k)
#' +(1-\alpha)\widehat{\boldsymbol f}^{\boldsymbol Y_T}(\omega_k)\Big |}
#' {\Big|\widehat{\boldsymbol f}^{\boldsymbol Y_T}(\omega_k)\Big|}}+ \log{\frac{\Big|\alpha\widehat{\boldsymbol f}^{\boldsymbol Y_T}(\omega_k) +
#' (1-\alpha)\widehat{\boldsymbol f}^{\boldsymbol X_T}(\omega_k)\Big |}
#' {\Big|\widehat{\boldsymbol f}^{\boldsymbol X_T}(\omega_k)\Big|}}\bigg),}
#' where \eqn{\alpha \in (0,1)}.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{kakizawa1998discrimination}{mlmts}
#'
#' }
#' @export


dis_spectral <- function(X, method = 'j_divergence', alpha = 0.5, features = FALSE) {

  # Usual checkings

  check_mts(X)
  check_mts_rows(X)
  l <- length(X)
  c <- ncol(X[[1]])


  # Computing the distance matrix or the spectral matrix

  distance_matrix <- matrix(0, nrow = l, ncol = l)
  max_length <- 2 * c ^ 2 * (ceiling(nrow(X[[1]])/2) + 1)
  spectral_matrix <- matrix(0, nrow = l, ncol = max_length)

  if (features == FALSE) {

    if (method == 'j_divergence') {

      for (i in 1 : l) {

        if(i > 1) {for (j in 1 : (i - 1)) {

          distance_matrix[i, j] <- j_divergence(X[[i]], X[[j]])

        }

        }

      }

      return(stats::as.dist(distance_matrix))

    } else {


      for (i in 1 : l) {

        if(i > 1) {for (j in 1 : (i - 1)) {

          distance_matrix[i, j] <- chernoff_divergence(X[[i]], X[[j]], alpha = alpha)

        }

        }

      }

      return(stats::as.dist(distance_matrix))

    }

  } else {

    for (i in 1 : l) {

      spectral_matrix[i,] <- spectral_features(X[[i]])

    }

    return(spectral_matrix)

  }


}
