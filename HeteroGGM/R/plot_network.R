#' Visualization of network structures.
#'
#' @name plot_network
#' @usage plot_network(summ, num_subgroup = 1, plot.mfrow,
#'                     vertex.size=2,vertex.label.cex=0.7,
#'                     vertex.label.dist=0.75, edge.width = 0.1, l=0)
#'
#' @param summ A list, the summary of the resulting network structures.
#' @param num_subgroup Int/vector, the subgroup numbering.
#' @param plot.mfrow Figure Layout.
#' @param vertex.size The vertex size.
#' @param vertex.label.cex The vertex label size.
#' @param vertex.label.dist The distance of vertex labels.
#' @param edge.width The edge width.
#' @param l Node Coordinates.
#'
#' @return Visualization of network structure
#' @export
#' @importFrom graphics par
#'
plot_network <- function(summ, num_subgroup = 1, plot.mfrow,
                         vertex.size=2,vertex.label.cex=0.7,
                         vertex.label.dist=0.75, edge.width = 0.1, l=0){

  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## The name of the function: linked_node_names
  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## Description:
  ##            Visualization of network structure.
  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## Required preceding functions or packages:
  ##            R functions: summary_network()
  ## ------------------------------------------------------------------------------------------------------------------------------------------
  variable_names = summ$variable_names
  K_hat = length(summ$Mu_summary)
  p = length(variable_names)
  opt_Theta_hat = summ$opt_Theta_hat

  # plot: identified subgrouping network structures
  network.list <- list()
  net.plot.list <- list()
  for (k in 1:K_hat) {
    network = as.data.frame(opt_Theta_hat[,,k])
    names(network) = variable_names
    row.names(network) = variable_names
    network = as.matrix(network)
    network[which(network != 0)] <- 1
    network.list[[k]] <- network
    net.plot.list[[k]] <- graph_from_adjacency_matrix(network,mode = "undirected",diag = FALSE)
  }

  network.joint = matrix(0,p,p)
  for (k in 1:K_hat) {
    network.joint = network.joint + network.list[[k]]
  }
  network.joint[which(network.joint != 0)] <- 1
  set.seed(12)
  net.plot = graph_from_adjacency_matrix(network.joint,mode = "undirected",diag = FALSE)
  if(sum(abs(l))==0){
    l = layout.fruchterman.reingold(net.plot)
  }

  par(mfrow = plot.mfrow)
  for (k in num_subgroup) {
    plot.igraph(net.plot.list[[k]],vertex.size=vertex.size,vertex.label.cex=vertex.label.cex,
                vertex.label.dist=vertex.label.dist, layout = l, edge.width = edge.width)
  }
  return(l)
}

