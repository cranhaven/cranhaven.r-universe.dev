#' Spectral clustering for \code{igraph} objects
#' 
#' This function implements the network clustering algorithm described in
#' (M. E. J. Newman, 2006). 
#' 
#' The complete iterative algorithm comprises of two steps. In the
#' first step, the network is expressed in terms of its leading eigenvalue and eigenvector
#' and recursively partition into two communities. Partitioning occurs if the maximum
#' positive eigenvalue is greater than the tolerance (\code{tol=10-5}) for the current
#' partition, and if it results in a positive contribution to the Modularity.
#'
#' Given an initial separation using the leading eigen step, the function then continues to
#' maximise for the change in Modularity using a fine-tuning step - or variate thereof. The
#' first stage here is to find the node which, when moved from one community to another,
#' gives the maximum change in Modularity. This node’s community is then fixed and we repeat
#' the process until all nodes have been moved. The whole process is repeated from this new
#' state until the change in the Modularity, between the new and old state, is less than the
#' predefined tolerance (\code{tol}).
#'
#' A slight variant of the fine-tuning step, which can reduce execution time by factor 2 to
#' 5, is also provided. Instead of moving each node into each community in turn, we only
#' consider moves of neighbouring nodes, found in different communities, to the community of
#' the current node of interest. This variant of the node-moving algorithm effectively `fixes`
#' neigbouring nodes \code{fix_neig} in the community being considered.
#'
#' The two steps process is repeatedly applied to each new community found, subdivided each community
#' into two new communities, until we are unable to find any division that results in a positive change
#' in Modularity. An additional stopping criteria, based on the minimum cluster size \code{Cn_min}, is
#' also provided.
#'
#'
#' @param g \code{igraph} object
#' @param Cn_min minimum cluster size
#' @param tol tolerance
#' @param names are we dealing with alphaNumeric (1) or numeric (!1) ids
#' @param fix_neig whether to fix neighbouring nodes found in same community
#' 
#' @return \code{data.frame} with node names and membership information
#' @export
#' @importFrom igraph get.edgelist V
#'
#' @examples
#' data(karate,package='igraphdata')
#' df.mem<-spectral_igraph_membership(karate)
spectral_igraph_membership<-function(g,Cn_min = 1L, tol = 0.00001, names = 1L, fix_neig = 0L){
  if(!inherits(g,'igraph')){
    stop('Graph should be "igraph" object.')
  }
  el = as.data.frame(igraph::get.edgelist(g,names=TRUE))
  load_data(df=el)
  status = spectral(Cn_min=Cn_min,tol=tol,names=names,fix_neig=fix_neig)
  spec   = membership(detach_graph=1)
  idx<-match(igraph::V(g)$name,spec$ID)
  spec.df<- data.frame(names=spec$ID[idx],membership=spec$K[idx])
  return(spec.df)
}

#' Spectral clustering for \code{igraph} objects
#' 
#' This function invoke \code{\link{spectral_igraph_membership}} to calculate
#' clustering and convert it into \code{\link[igraph]{communities}} object for
#' seamless work with native \code{\link{igraph}} clustering functions.
#'
#' @inheritParams spectral_igraph_membership
#' @return \code{\link[igraph]{communities}} object
#' @export
#' @importFrom igraph modularity vcount V
#'
#' @examples
#' data(karate,package='igraphdata')
#' c<-spectral_igraph_communities(karate)
spectral_igraph_communities<-function(g,Cn_min = 1L, tol = 0.00001, names = 1L, fix_neig = 0L){
  if(!inherits(g,'igraph')){
    stop('Graph should be "igraph" object.')
  }
  df.mem<-spectral_igraph_membership(g,Cn_min=Cn_min,tol=tol,names=names,fix_neig=fix_neig)
  res<-list()
  res$vcount <- igraph::vcount(g)
  res$algorithm <- "spectral"
  res$membership <- df.mem$membership
  res$modularity <- igraph::modularity(g,df.mem$membership)
  res$names <- df.mem$names
  class(res) <- "communities"
  return(res)
}