Create_rand_g_without_preserving_degrees <- function(j) {
  # Randomize a network without preserving its node in- and out-degrees
  # Args:
  #   j: integer representing the number of randomized networks that we need to generate
  #   N.B.: the orginal network that will be radomized is taken from the calling function (parent.frame)
  # Returns:
  #   A graph object that has been randomized

  actors_ids    <- get("actors_ids", parent.frame())
  nodes_no      <- get("nodes_no", parent.frame())
  edges_no      <- get("edges_no", parent.frame())
  swaping_no    <- get("swaping_no", parent.frame())
  viteration_no <- get("viteration_no", parent.frame())
  log_name      <- get("log_name", parent.frame())

  if ( j == 1 ) {
    msg <- sprintf(paste0(as.character(Sys.time()), ": Randomizing network %d of %d"), j, viteration_no)
    write(msg, file=log_name, append=FALSE)
  }
  if ( j %% 10 == 0 ) {
    msg <- sprintf(paste0(as.character(Sys.time()), ": Randomizing network %d of %d"), j, viteration_no)
    write(msg, file=log_name, append=TRUE)
  }

  permuted_edges <- unique(cbind(sample(actors_ids, swaping_no, replace=T), sample(1:nodes_no, swaping_no, replace=T)))
  permuted_edges <- permuted_edges[sample(1:nrow(permuted_edges), edges_no),]

  vgraph <- delete.edges(vgraph, E(vgraph))
  vgraph <- add.edges(vgraph, t(permuted_edges))

  return(vgraph)
}
