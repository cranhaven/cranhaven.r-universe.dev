Create_rand_g_with_preserving_in_degrees <- function(j) {
  # Randomize a network by preserving its node in-degrees
  # Args:
  #   j: integer representing the number of randomized networks that we need to generate
  #   N.B.: the orginal network that will be radomized is taken from the calling function (parent.frame)
  # Returns:
  #   A graph that results from calling the Sub_switch_edges function. This function randomizes
  #   a network with preserving its node degrees

  to         <- get("to", parent.frame())
  edges_no   <- get("edges_no", parent.frame())
  swaping_no <- get("swaping_no", parent.frame())

  from_to       <- NULL
  regulators    <- unique(from)
  from[1]       <- sample(regulators, 1)
  from_to[1]    <- paste("t",from[1],to[1],"t")
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

  for ( i in 2:edges_no ) {
    rand_from <- sample(regulators, 1)
    while ( length(grep(paste("t",rand_from,to[i],"t"), from_to))!=0 ) {
      rand_from <- sample(regulators, 1)
    }
    from[i] <- rand_from
    from_to[i] <- paste("t",from[i],to[i],"t")
  }

  vgraph <- delete.edges(vgraph, E(vgraph))
  vgraph <- add.edges(vgraph, t(cbind(from,to)))

  return(Sub_switch_edges(1, vgraph, swaping_no, edges_no, from, to, from_to))
}
