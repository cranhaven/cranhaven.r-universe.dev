#' Simplify a built network object by removing parallel edges (and loops)
#'
#' @param net A network list returned by [build_network()].
#' @param keep_edge Which edge to keep when multiple edges connect the same
#'   unordered node pair. One of "first" or "shortest".
#'
#' @return A network list with updated `edges`, `graph`, and `A`
#'   (and the same `nodes`).
#' @export
simplify_network <- function(net, keep_edge = c("first", "shortest")) {
  keep_edge <- match.arg(keep_edge)

  stopifnot(is.list(net))
  req <- c("roads", "nodes", "edges", "graph", "A")
  if (!all(req %in% names(net)))
    stop("`net` must be a network list from build_network().")

  edges <- net$edges
  nodes <- net$nodes

  if (!inherits(edges, "sf"))
    stop("`net$edges` must be an sf object.")
  if (!all(c("from", "to") %in% names(edges)))
    stop("`net$edges` must have `from` and `to` columns.")

  ## empty edge set
  if (nrow(edges) == 0L) {
    g <- igraph::make_empty_graph(n = nrow(nodes), directed = FALSE)
    A0 <- Matrix::Matrix(0, nrow(nodes), nrow(nodes), sparse = TRUE)
    net$edges <- edges
    net$graph <- g
    net$A <- A0
    return(net)
  }

  ## drop loops defensively
  edges <- edges[
    !is.na(edges$from) &
      !is.na(edges$to) &
      edges$from != edges$to,
  ]

  if (nrow(edges) == 0L) {
    g <- igraph::make_empty_graph(n = nrow(nodes), directed = FALSE)
    A0 <- Matrix::Matrix(0, nrow(nodes), nrow(nodes), sparse = TRUE)
    net$edges <- edges
    net$graph <- g
    net$A <- A0
    return(net)
  }

  ## undirected key for parallel edges
  a <- pmin(edges$from, edges$to)
  b <- pmax(edges$from, edges$to)
  key <- paste0(a, "-", b)

  if (keep_edge == "first") {
    keep_idx <- !duplicated(key)
  } else {
    len <- as.numeric(sf::st_length(edges))
    ord <- order(key, len)
    keep_idx <- rep(FALSE, length(key))
    keep_idx[ord[!duplicated(key[ord])]] <- TRUE
  }

  edges_s <- edges[keep_idx, ]

  ## rebuild graph (keep isolates)
  g <- igraph::make_empty_graph(n = nrow(nodes), directed = FALSE)
  edge_mat <- as.matrix(sf::st_drop_geometry(edges_s[, c("from", "to")]))
  if (nrow(edge_mat) > 0L) {
    g <- igraph::add_edges(g, as.vector(t(edge_mat)))
  }

  ## adjacency: binary, symmetric, zero diagonal
  A <- igraph::as_adjacency_matrix(g, sparse = TRUE)
  Matrix::diag(A) <- 0
  A <- A + Matrix::t(A)
  A@x[A@x != 0] <- 1
  A <- Matrix::drop0(A, tol = 0)

  net$edges <- edges_s
  net$graph <- g
  net$A <- A
  net
}

