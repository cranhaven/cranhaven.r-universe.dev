Sub_switch_edges <- function(j, vgraph, vswaping_no, vedges_no, vfrom, vto, vfrom_to) {
  # Rewires pair of edges of network by switching or swaping edge end nodes
  # Args:
  #   j: an integer indicating the number of randomized networks that we need to generate
  #   vgraph: a graph object representing the graph to randomize
  #   vswaping_no: an integer indicating how many times edges should be switched
  #   vedges_no: an integer indicating the number of edges of vgraph
  #   vfrom: a character vector including the actor ids of vgraph
  #   vto: a character vector including the target ids of vgraph
  #   vfrom_vto: a character vector including the concatenation of actor and target ids of vgraph
  # Returns:
  #   vgraph: a graph object representing the rewired network


  for ( i in 1:vswaping_no ) {
    rand1 <- sample(1:vedges_no,1)
    rand2 <- sample(1:vedges_no,1)
    while ( rand1==rand2 || vfrom[rand1]==vfrom[rand2] || vto[rand1]==vto[rand2] ) {
      rand2 <- sample(1:vedges_no,1)
    }
    vto[c(rand1,rand2)] <- vto[c(rand2,rand1)]
    vfrom_to[rand1] <- paste("t",vfrom[rand1],vto[rand1],"t")
    vfrom_to[rand2] <- paste("t",vfrom[rand2],vto[rand2],"t")
  }

  duplicated_edges <- which(duplicated(cbind(vfrom_to)))
  if ( length(duplicated_edges)!=0 ) {
    for ( i in 1:length(duplicated_edges) ) {
      rand1 <- duplicated_edges[i]
      rand2 <- sample(setdiff(1:vedges_no,duplicated_edges),1)
      while ( rand1==rand2 || vfrom[rand1]==vfrom[rand2] || vto[rand1]==vto[rand2] || length(grep(paste("t",vfrom[rand1],vto[rand2],"t"), vfrom_to))!=0 || length(grep(paste("t",vfrom[rand2],vto[rand1],"t"), vfrom_to))!=0 ) {# || edge1[1]==edge2[2] || edge1[2]==edge2[1]) {
        rand2 <- sample(1:vedges_no,1)
      }
      vto[c(rand1,rand2)] <- vto[c(rand2,rand1)]
    }
  }

  vgraph <- delete.edges(vgraph, E(vgraph))
  vgraph <- add.edges(vgraph, t(cbind(vfrom,vto)))

  return(vgraph)
}
