summary.vertex.sort <- function(object, ...) {
  # The class summary.vertex.sort defined in order to use it for printing a summary of a vertex.sort
  # object on calling the general function summary()
  # Args:
  #   object: a vertex.sort object
  # Returns:
  #   res: an object of class summary.vertex.sort

  if ( class(object)!="vertex.sort" )
    return("Error: first argument should be a vertex.sort object")

  nodes_no  <- vcount(object$graph)
  edges_no  <- ecount(object$graph)
  levels_no <- object$levels.no

  top_actors_no      <- length(object$top.actors)
  core_actors_no     <- length(object$core.actors)
  bottom_actors_no   <- length(object$bottom.actors)
  targets_no         <- length(object$targets)
  excluded_actors_no <- length(object$excluded.actors)
  disconnected_actors_no  <- length(object$disconnected.actors)
  disconnected_targets_no <- length(object$disconnected.targets)

  layers_no                 <- (top_actors_no!=0) + (core_actors_no!=0) + (bottom_actors_no!=0)
  graph_description         <- c(nodes_no, edges_no, layers_no, levels_no)
  names(graph_description)  <- c("Nodes no", "Edges no", "Layers no", "Levels no")
  layers_description        <- c(top_actors_no, core_actors_no, bottom_actors_no, targets_no)
  names(layers_description) <- c("Top actors no", "Core actors no", "Bottom actors no", "Targets no")
  excluded_disconnected_nodes        <- c(excluded_actors_no, disconnected_actors_no, disconnected_targets_no)
  names(excluded_disconnected_nodes) <- c("Excluded actors no", "Disconnected actors no", "Disconnected targets no")

  res <- list(graph_description=graph_description, layers_description=layers_description,
           excluded_disconnected_nodes=excluded_disconnected_nodes)
  res$call   <- match.call()
  class(res) <- "summary.vertex.sort"
  res
}
