#' Group nodes by community
#'
#' @description
#' Reorders the graph to group nodes in the same community together.
#' Useful for viewing community structure of a graph using a `heatmap()`.
#'
#' @details
#' Communities are ordered by size, so nodes in the largest community are
#' first. Within a community, nodes are order by highest-to-lowest degree.
#'
#' If membership is in matrix form (the output from [speakeasyR::cluster()]
#' with `subcluster` > 1) a matrix is returned with the indices for level one
#' in row 1 and level n in row n. Each row reorders the communities of the
#' previous row such that, at the second level, nodes are still grouped by
#' the first level communities. This allows the hierarchical structure to be
#' viewed.
#'
#' See vignette for a multilevel example.
#'
#' @param graph The graph or adjacency matrix the membership vector was created
#'   for.
#' @param membership A vector or matrix listing node communities. The output
#'   from [speakeasyR::cluster()] (should also work for other clustering
#'   algorithms that return membership in the same format).
#' @param is_directed Whether the graph should be treated as directed or not.
#'   By default, if the graph is symmetric it is treated as undirected.
#'
#' @return An index vector or matrix. The number of rows are equal to the value
#'   of `subcluster` passed to [speakeasyR::cluster()].
#' @export
#'
#' @examples
#' n_nodes <- 100
#' n_types <- 3
#' mu <- 0.3 # Mixing parameter (likelihood an edge is between communities).
#' pref <- matrix(mu, n_types, n_types)
#' diag(pref) <- 1 - mu
#' g <- igraph::preference.game(n_nodes, types = n_types, pref.matrix = pref)
#' # Use a dense matrix representation to easily apply index.
#' adj <- as(g[], "matrix")
#' memb <- speakeasyR::cluster(adj, seed = 222, max_threads = 2)
#' ordering <- speakeasyR::order_nodes(adj, memb)
#' \donttest{
#' heatmap(adj[ordering, ordering], scale = "none", Rowv = NA, Colv = NA)
#' }
order_nodes <- function(graph, membership, is_directed = "detect") {
  adj <- se2_as_matrix_i(graph)
  if (is_directed == "detect") {
    is_directed <- adj$is_directed
  }

  if (is.vector(membership)) {
    n_levels <- 1
    n_nodes <- length(membership)
  } else {
    n_levels <- nrow(membership)
    n_nodes <- ncol(membership)
  }

  if (n_levels > 1) {
    ordering <- matrix(as.integer(0), nrow = n_levels, ncol = n_nodes)
  } else {
    ordering <- integer(n_nodes)
  }

  .C(
    C_order_nodes, as.integer(adj$se2_i), as.integer(adj$se2_p),
    as.double(adj$values), as.integer(n_nodes), as.integer(membership),
    as.integer(n_levels), as.logical(is_directed),
    ordering = ordering
  )$ordering
}
