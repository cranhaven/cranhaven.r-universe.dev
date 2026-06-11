#' Check Graph Network
#'
#' \code{check_network} returns processed graph.
#'
#' This is a pre-processing function that turns raw input into directed
#' networks with edge weights.
#'
#' @param graph igraph
check_network <- function(graph) {
  graph <- graph %>%
    igraph::simplify(remove.loops = FALSE) %>%
    igraph::as.directed()

  nodes <- igraph::V(graph)
  out_edges <- igraph::incident_edges(graph, nodes, mode = "out")

  for (i in seq_along(out_edges)) {
    out_edges_i <- out_edges[[i]]

    graph_weights <- igraph::get.edge.attribute(graph, "weight", out_edges_i)
    has_weights <- any(sapply(graph_weights, is_truthy))

    if (has_weights) {
      graph <- graph %>%
        igraph::set_edge_attr(
          "weight",
          index = out_edges_i,
          value = graph_weights / sum(graph_weights)
        )
    } else if (length(out_edges_i) > 0) {
      non_zero <- Filter(function(oe) length(oe) > 0, out_edges_i) %>%
        unlist %>%
        as.numeric

      weight_value <- 1 / length(out_edges_i)

      graph <- graph %>%
        igraph::set_edge_attr("weight", out_edges_i, 0) %>%
        igraph::set_edge_attr(
          "weight",
          index = non_zero,
          value = weight_value
        )
    }
  }

  graph %>%
    igraph::simplify(remove.loops = FALSE)
}
