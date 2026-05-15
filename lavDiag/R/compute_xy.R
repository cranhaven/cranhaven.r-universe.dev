#' Compute 2D coordinates for nodes using igraph
#' @noRd
#' @keywords internal
.compute_xy <- function(nodes, edges) {
  # ensure required columns exist
  stopifnot(all(c("id") %in% names(nodes)))
  stopifnot(all(c("from","to") %in% names(edges)))

  # build an undirected graph for layout purposes
  verts <- unique(nodes["id"])
  el    <- edges[, c("from","to")]
  g     <- igraph::graph_from_data_frame(el, directed = FALSE, vertices = verts)

  # pick a deterministic layout (nicely = FR+Kamada combo)
  xy <- igraph::layout_nicely(g)

  tibble::tibble(
    id = igraph::V(g)$name,
    x  = as.numeric(xy[,1])*100,
    y  = as.numeric(xy[,2])*100
  )
}
