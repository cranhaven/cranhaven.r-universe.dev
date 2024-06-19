#' SpeakEasy 2 community detection
#'
#' @description
#' Group nodes into communities.
#'
#' @param graph A graph or adjacency matrix in a form that can be converted to
#'   `matrix` or `Matrix::dgCMatrix` using an `as.matrix()` coercion method.
#'   Accepted types include `matrix`, `dgCMatrix`, `ngCMatrix`, and
#'   `igraph::graph`s.
#' @param discard_transient The number of partitions to discard before tracking.
#' @param independent_runs How many runs SpeakEasy2 should perform.
#' @param max_threads The maximum number of threads to use. By default this is
#'   the same as the number of independent runs. If max_threads is greater than
#'   or equal to the number of processing cores, all cores may run. If
#'   max_threads is less than the number of cores, at most max_threads cores
#'   will run.
#' @param seed Random seed to use for reproducible results. SpeakEasy2 uses a
#'   different random number generator than R, but if the seed is not
#'   explicitly set, R's random number generator is used create one. Because of
#'   this, setting R's RNG will also cause reproducible results.
#' @param subcluster Depth of clustering. If greater than 1, perform recursive
#'   clustering.
#' @param min_clust Smallest clusters to recursively cluster. If subcluster not
#'   set to a value greater than 1, this has no effect.
#' @param target_clusters The number of random initial labels to use.
#' @param target_partitions Number of partitions to find per independent run.
#' @param verbose Whether to provide additional information about the
#' clustering or not.
#' @param is_directed Whether the graph should be treated as directed or not.
#'   By default, if the graph is symmetric it is treated as undirected.
#'
#' @return A membership vector. If subclustering, returns a matrix with number
#'   of rows equal to the number of recursive clustering. Each row is the
#'   membership at different hierarchical scales, such that the last rows are
#'   the highest resolution.
#' @export
#'
#' @examples
#' graph <- igraph::graph.famous("zachary")
#' membership <- cluster(graph, max_threads = 2)
cluster <- function(graph, discard_transient = 3, independent_runs = 10,
                    max_threads = 0, seed = 0, target_clusters = 0,
                    target_partitions = 5, subcluster = 1, min_clust = 5,
                    verbose = FALSE, is_directed = "detect") {
  adj <- se2_as_matrix_i(graph)

  if (is_directed == "detect") {
    is_directed <- adj$is_directed
  }

  if (seed == 0) {
    seed <- sample.int(9999, 1)
  }

  if (subcluster > 1) {
    membership <- matrix(as.integer(0), nrow = subcluster, ncol = adj$n_nodes)
  } else {
    membership <- integer(adj$n_nodes)
  }

  .C(
    C_speakeasy2, as.integer(adj$se2_i), as.integer(adj$se2_p),
    as.double(adj$values), as.integer(adj$n_nodes),
    as.integer(discard_transient), as.integer(independent_runs),
    as.integer(max_threads), as.integer(seed), as.integer(target_clusters),
    as.integer(target_partitions), as.integer(subcluster),
    as.integer(min_clust), as.logical(verbose), as.logical(is_directed),
    membership = membership
  )$membership
}
