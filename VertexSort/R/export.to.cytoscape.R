export.to.cytoscape <- function(vs_object) {
  # Exprots a graph with its node attributes to cytoscape.
  # Args:
  #   vs_object: an object of class vertex.sort. This is a network that has been sorted by the
  #              vertex sort algorithm
  # Returns:
  #   A list containg: edges of the sorted network, nodes of the sorted network and node attributes
  #     node attributes are: identifier (orf), node_type (actor/target), node_layer (top, core or
  #                          bottom) and node_level.

  if ( class(vs_object)!="vertex.sort" )
    return("Error: first argument should be a vertex.sort object")

  # edges data frame
  edges <- vs_object$edges

  # nodes attribute (node type: actor/target)
  node_attribute <- data.frame(identifier=vs_object$traits$orf, node_type=vs_object$traits$type,
                               node_layer=vs_object$traits$layer, node_level=vs_object$traits$level,
                               stringsAsFactors=F)
  return(list(edges=edges, node_attribute=node_attribute))
}
