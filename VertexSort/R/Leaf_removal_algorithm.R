Leaf_removal_algorithm <- function(vgraph, nodes_nb, nodes_orf) {
  # Apply the leaf removal algorithm on a network to sort netowk nodes in different levels
  # Args:
  #   vgraph: a graph object representing the network to be sorted
  #   nodes_nb: integer the number of nodes in vgraph
  #   nodes_orf: orf of the nodes including orfs of collapse nodes
  # Returns:
  #   levels: a numeric vector containing the level number of each node. Names of the levels vector
  #           represent node orfs.

  levels <- vector(mode="numeric", length=nodes_nb)
  names(levels) <- nodes_orf
  level <- 1
  repeat {
    leaves <- which(degree(vgraph,V(vgraph)$id,"out")==0)  # all node ids of nodes having no out arrow
    for ( i in 1:length(leaves) ) {
      leaf_id <- leaves[i]  # id of the node that has no out arrows. This node is called a leaf
      node <- unlist(strsplit(V(vgraph)[V(vgraph)$id==leaf_id]$orf,","))  # strsplit is needed in case the node was collapsed so it might correspond to an scc
      levels[node] <- level
    }
    vgraph <- delete.vertices(vgraph, V(vgraph)[V(vgraph)$id %in% leaves]$id)
    V(vgraph)$id <- as.vector(V(vgraph))
    level <- level+1
    if ( vcount(vgraph)==0 | ecount(vgraph)==0 ) break
  }
  if ( vcount(vgraph) ) {
    # strsplit is needed in case the node was collapsed so it might correspond to an scc
    node <- unlist( sapply( V(vgraph)$orf, function(x) unlist(strsplit(x,","))), use.names=F)
    levels[node] <- level
  }
  return(levels)
}
