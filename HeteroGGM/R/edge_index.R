edge_index <- function(opt_Theta_hat, data){
  variable_names <- names(data)
  if(length(variable_names) == 0){variable_names <- as.character(c(1:dim(data)[2]))}
  p <- dim(opt_Theta_hat)[1]
  K_hat <- dim(opt_Theta_hat)[3]

  network_edge_summary <- list()
  for (k in 1:K_hat) {
    network_edge_subgroup <- list()
    for (j in 1:p) {
      linked_node_num <- setdiff(which(opt_Theta_hat[,j,k] != 0),j)
      linked_node_names <- variable_names[linked_node_num]
      linked_node_info <- as.data.frame(cbind(linked_node_num,linked_node_names))
      network_edge_subgroup[[j]] <- linked_node_info
    }
    network_edge_summary[[k]] <- network_edge_subgroup
  }
  return(network_edge_summary)
}

