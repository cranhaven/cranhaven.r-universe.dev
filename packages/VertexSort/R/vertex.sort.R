vertex.sort <- function(edges) {
  # The class vertex.sort. It calls the apply.vertex.sort function to apply the verstex sort algorithm
  # on a network to elucidate its hierarchical structure
  # Args:
  #   edges: a dataframe of 2 columns (actors, targets)
  # Returns:
  #   vs_results: an object of class vertex.sort

  if ( class(edges)!="data.frame" || ncol(edges)!=2 )
    return("Error: first argument should be a data frame having 2 columns")

  names(edges) <- c("actor_id", "target_id")
  edges[,1]    <- as.character(edges[,1])
  edges[,2]    <- as.character(edges[,2])
  vs_results   <- apply.vertex.sort(edges)
  if ( is.character(vs_results) )
      return(vs_results)

  vs_results$call   <- match.call()
  class(vs_results) <- "vertex.sort"
  vs_results
}
