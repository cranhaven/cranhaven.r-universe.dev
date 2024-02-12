Collapse_subgraph <- function(vgraph, vnodes) {
  # Collapse a subgraph of a graph in a super node. The super node will be labeled by a
  # concatination of the labels of the nodes of the subgraph. It will preserve the in- and out- edges
  # of the collapsed subgraph.
  # Args:
  #   vgraph: a graph object containing the subgraph that needs to be collapsed
  #   vnodes: a vector containing the nodes of the subgraph that needs to be collapsed
  # Returns:
  #   vgraph: a graph object containing the graph after collapsing the subgraph in a super node

  new_node_orf <- paste(V(vgraph)[V(vgraph)$id %in% vnodes]$orf, collapse=",")  # collapse nodes orf

  # vertices id of the in neighbors of vnodes
  from <- setdiff(unlist(sapply(vnodes, function(x) neighbors(vgraph, x, mode="in")$id)), vnodes)
  # vertices id of the out neighbors of vnodes
  to   <- setdiff(unlist(sapply(vnodes, function(x) neighbors(vgraph, x, mode="out")$id)), vnodes)

  vgraph <- delete.vertices(vgraph, V(vgraph)[V(vgraph)$id %in% vnodes]$id)
  # replace the old ids by the new ids after deleting vnodes in from
  from   <- sapply(from, function(x) as.vector(V(vgraph)[V(vgraph)$id==x]))
  # replace the old ids by the new ids after deleting vnodes in to
  to     <- sapply(to, function(x) as.vector(V(vgraph)[V(vgraph)$id==x]))
  new_node_id  <- vcount(vgraph)+1
  V(vgraph)$id <- as.vector(V(vgraph))

  temp_in  <- matrix( c(from, rep(new_node_id,length(from))), ncol=2)
  temp_out <- matrix( c(rep(new_node_id,length(to)), to), ncol=2)
  if ( nrow(temp_in)!=0 & nrow(temp_out)!=0 ) {
    edges <- rbind(temp_in,temp_out)
  } else {
    if ( nrow(temp_in)==0 & nrow(temp_out)!=0 ) {
      edges <- temp_out
    } else {
      if ( nrow(temp_in)!=0 & nrow(temp_out)==0 ) {
        edges <- temp_in
      }
    }
  }

  # add one new vertex representing the collapsed vnodes
  vgraph <- add.vertices(vgraph, nv=1, id=new_node_id, orf=new_node_orf)
  vgraph <- add.edges(vgraph,t(edges))                      # add the in and out edges to this vertex
  return(vgraph)
}
