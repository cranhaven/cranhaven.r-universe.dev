#' Indexes the names of all nodes connected to some particular nodes in a subgroup.
#'
#' @name linked_node_names
#' @usage linked_node_names(summ, va_names, num_subgroup=1)
#'
#' @param summ A list, the summary of the resulting network structures.
#' @param va_names A vector, the names of nodes of interest.
#' @param num_subgroup Int, the subgroup numbering.
#'
#' @return A list including the names of connected nodes to the nodes of interest in a subgroup.
#' @export
#'
linked_node_names <- function(summ, va_names, num_subgroup=1){

  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## The name of the function: linked_node_names
  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## Description:
  ##            Indexes the names of all nodes connected to some particular nodes in a subgroup.
  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## Required preceding functions or packages:
  ##            R functions: summary_network()
  ## ------------------------------------------------------------------------------------------------------------------------------------------

  variable_names <- summ$variable_names
  len_va <- match(va_names,variable_names)
  linked.node.names <- list()
  for (j in 1:length(len_va)) {
    linked.node.names[[j]] <- as.data.frame(summ$Theta_summary$network_edge_summary[[num_subgroup]][[len_va[j]]])
  }
  names(linked.node.names) <- va_names
  return(linked.node.names = linked.node.names)
}

