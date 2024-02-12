print.summary.vertex.sort <- function(x, ...) {
  # Print a summary of a vertex.sort object
  # Args:
  #   x: a vertex.sort object

  cat("Call:\n")
  print(x$call)
  cat("\nGraph description:\n")
  print(x$graph_description)
  cat("\nLayers description:\n")
  print(x$layers_description)
  cat("\nExcluded and disconnected nodes:\n")
  print(x$excluded_disconnected_nodes)
}
