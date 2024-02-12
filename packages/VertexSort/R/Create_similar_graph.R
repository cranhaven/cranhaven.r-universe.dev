Create_similar_graph <- function(j) {
  # Randomize a network by preserving similar in- and out-degrees of each of its nodes.
  # Similar netwoks means that the difference between the in-degree of nodes in the orginal network
  # and the randomized network is equal to 0, 1 or -1. Same thing for node out-degrees
  # Args:
  #   j: integer representing the number of randomized networks that we need to generate
  #   N.B.: the orginal network that will be radomized is taken from the calling function (parent.frame)
  # Returns:
  #   A graph that results from calling the Sub_switch_edges function. This function randomizes
  #   a network with preserving its node degrees

  actors_ids                <- get("actors_ids", parent.frame())
  actors_ids_zero_in_degree <- get("actors_ids_zero_in_degree", parent.frame())
  nodes_no                  <- get("nodes_no", parent.frame())
  edges_no                  <- get("edges_no", parent.frame())
  swaping_no                <- get("swaping_no", parent.frame())
  viteration_no             <- get("viteration_no", parent.frame())
  log_name                  <- get("log_name", parent.frame())

  if ( j == 1 ) {
    msg <- sprintf(paste0(as.character(Sys.time()), ": Randomizing network %d of %d"), j, viteration_no)
    write(msg, file=log_name, append=FALSE)
  }
  if ( j %% 10 == 0 ) {
    msg <- sprintf(paste0(as.character(Sys.time()), ": Randomizing network %d of %d"), j, viteration_no)
    write(msg, file=log_name, append=TRUE)
  }

  sample_size <- round(length(setdiff(actors_ids,actors_ids_zero_in_degree))/3)
  sample_decrease_in_degrees <- sample(setdiff(actors_ids,actors_ids_zero_in_degree), sample_size)
  sample_increase_in_degrees <- sample(setdiff(actors_ids,sample_decrease_in_degrees), sample_size)
  all_in_degree[sample_decrease_in_degrees] <- all_in_degree[sample_decrease_in_degrees]-1
  all_in_degree[sample_increase_in_degrees] <- all_in_degree[sample_increase_in_degrees]+1

  sample_decrease_out_degrees <- sample(actors_ids, sample_size)
  sample_increase_out_degrees <- sample(setdiff(actors_ids,sample_decrease_out_degrees), sample_size)
  all_out_degree[sample_decrease_out_degrees] <- all_out_degree[sample_decrease_out_degrees]-1
  all_out_degree[sample_increase_out_degrees] <- all_out_degree[sample_increase_out_degrees]+1

  k <- 1
  rand_edges <- matrix(ncol=2, nrow=edges_no)
  temp_all_in_degree  <- rep(0,nodes_no)
  temp_all_out_degree <- rep(0,nodes_no)
  while ( k <= edges_no ) {
    potential_regulators <- which((all_out_degree-temp_all_out_degree)>0)
    potential_targets    <- which((all_in_degree-temp_all_in_degree)>0)
    # if the program can't find the last interactor using the sample command
    if ( length(potential_regulators)==1 & length(potential_targets)==1 ) {
      rand_edges[k,] <- c(potential_regulators, potential_targets)
      temp_all_out_degree[potential_regulators] <- temp_all_out_degree[potential_regulators]+1
      temp_all_in_degree[potential_targets] <- temp_all_in_degree[potential_targets]+1
      break
    }

    rand_node1 <- sample(potential_regulators, 1, prob=(all_out_degree[potential_regulators]-temp_all_out_degree[potential_regulators])^2)
    rand_node2 <- sample(potential_targets,1, prob=(all_in_degree[potential_targets]-temp_all_in_degree[potential_targets])^2)

    rand_edges[k,1] <- rand_node1
    rand_edges[k,2] <- rand_node2
    temp_all_out_degree[rand_node1] <- temp_all_out_degree[rand_node1]+1
    temp_all_in_degree[rand_node2]  <- temp_all_in_degree[rand_node2]+1
    k <- k+1
  }

  duplicated_edges <- which(duplicated(rand_edges))
  from    <- rand_edges[,1]
  to      <- rand_edges[,2]
  from_to <- paste("t",from,to,"t")
  for ( i in 1:length(duplicated_edges) ) {
    rand1 <- duplicated_edges[i]
    rand2 <- sample(setdiff(1:edges_no,duplicated_edges),1)
    while ( rand1==rand2 || from[rand1]==from[rand2] || to[rand1]==to[rand2] || length(grep(paste("t",from[rand1],to[rand2],"t"), from_to))!=0 || length(grep(paste("t",from[rand2],to[rand1],"t"), from_to))!=0 ) {
      rand2 <- sample(1:edges_no,1)
    }
    to[c(rand1,rand2)] <- to[c(rand2,rand1)]
    from_to[rand1] <- paste("t",from[rand1],to[rand1],"t")
    from_to[rand2] <- paste("t",from[rand2],to[rand2],"t")
  }
  vgraph <- delete.edges(vgraph, E(vgraph))
  vgraph <- add.edges(vgraph, t(cbind(from,to)))

  return(Sub_switch_edges(1, vgraph, swaping_no, edges_no, from, to, from_to))
}
