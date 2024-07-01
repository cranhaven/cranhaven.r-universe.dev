

#' Performs the crisp clustering algorithm of Li (2019)
#'
#' \code{mc2pca_clustering} performs the clustering algorithm proposed by
#' \insertCite{li2019multivariate;textual}{mlmts}, which is based on common principal component analysis (CPCA).
#' @param X A list of MTS (numerical matrices).
#' @param k The number of clusters.
#' @param var_rate Rate of retained variability concerning the
#' reconstructed MTS samples (default is 0.90).
#' @param max_it The maximum number of iterations (default is 1000).
#' @param tol The tolerance (default is 1e-5).
#' @return A list with two elements:
#' \itemize{
#' \item \code{cluster}. A vector defining the clustering solution.
#' \item \code{iterations}. The number of iterations before the algorithm
#' stopped.
#' }
#' @examples
#' clustering_algorithm <- mc2pca_clustering(BasicMotions$data, k = 4, var_rate = 0.30)
#' # Executing the clustering algorithm in the dataset BasicMotions (var_rate = 0.30,
#' # i.e., we keep only a few principal components for computing the reconstructed series)
#' clustering_algorithm$cluster # The clustering solution
#' clustering_algorithm$iterations # The number of iterations before the algorithm
#' library(ClusterR)
#' external_validation(clustering_algorithm$cluster, BasicMotions$classes,
#' summary_stats = TRUE) # Evaluating the clustering algorithms vs the true partition
#' # stopped
#' @details
#' This function executes the crisp clustering method proposed by
#' . The algorithm is a \eqn{K}-means-type procedure where the distance
#' between a given MTS and a centroid is given by the reconstruction error
#' taking place when the series is reconstructed from the common space obtained
#' by considering all the series in the cluster associated with the corresponding
#' centroid (the common space is the centroid).
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{li2019multivariate}{mlmts}
#'
#' }
#' @export

mc2pca_clustering <- function(X, k, var_rate = 0.90, max_it = 1000, tol = 1e-5){

  check_mts(X)
  l <- length(X)
  c <- ncol(X[[1]])


  # Initialization

  X <- lapply(X, function(x) {scale(x, scale = FALSE)})
  r <- floor(l/k)
  v_vector <- rep(1 : k, each = r)
  remaining_categories <- sample(1 : k, l - k * r, replace = FALSE)
  initial_cluster_vector_prev <- c(v_vector, remaining_categories)
  initial_cluster_vector <- sample(initial_cluster_vector_prev)
  cluster_vector <- initial_cluster_vector


  # Iterations

  list_centroids <- list()
  sum_errors_vector <- numeric()
  count_iterations <- 1

  for (p in 1 : max_it) {

    # Creating the clusters (one list per cluster)

    list_clusters <- list()

    for (i in 1 : k) {

      indexes_k <- which(cluster_vector == i)
      list_clusters[[i]] <- X[indexes_k]

    }


    # Recomputation of centroids

    list_centroids <- list()

    for (i in 1 : k) {

      list_centroids[[i]] <- auxiliary_common_space_function(list_clusters[[i]], var_rate = var_rate)

    }


    # Reassignation

    sum_errors_vector_auxiliary <- numeric()

    for (i in 1 : l) {

      auxiliary_reconstructed <- auxiliary_reconstructed_series_function(X[[i]], list_centroids)
      cluster_vector[i] <- auxiliary_reconstructed$cluster
      sum_errors_vector_auxiliary[i] <- auxiliary_reconstructed$min_error

    }

    u_cluster_vector <- unique(cluster_vector)
    if (length(u_cluster_vector) != k) {

      n_clusters <- length(unique(cluster_vector))

      for (i in 1 : l) {

        cluster_vector[i] <- which(cluster_vector[i] == u_cluster_vector)

      }

      k <- n_clusters

    }


    # Termination

    sum_errors_vector[p] <- sum(sum_errors_vector_auxiliary)


    if (p > 1) {

      if (abs(sum_errors_vector[count_iterations] - sum_errors_vector[count_iterations - 1]) < tol) {

        return_list <- list(cluster = cluster_vector, iterations = count_iterations)
        return(return_list)

     }

    }

    count_iterations <- count_iterations + 1

  }

  return(cluster_vector)

}
