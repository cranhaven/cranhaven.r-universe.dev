Transpose_graph <- function(vgraph) {
  # Transposes a graph, that is switching the direction of the network edges
  # Args:
  #   vgraph: a graph object representing the graph to transpose
  # Returns:
  #   vgraph: a graph object representing the transposed graph

  edges  <- get.edgelist(vgraph, names=F)
  edges  <- edges[,2:1]
  vgraph <- delete.edges(vgraph, E(vgraph))
  vgraph <- add.edges(vgraph, t(edges))
  return(vgraph)
}
