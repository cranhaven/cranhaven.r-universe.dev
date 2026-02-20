#' plotINet
#'
#' @description The function plots a beginning network and the consensus in
#' one graph with different edge colours: red edges represent edges of the
#'  consensus already present in the beginning one, while light blue edges
#'  represent new edges constructed from the consensus.
#' @param adj one of the beginning adjacency matrices
#' @param graph.consensus consensus network, output of the
#' \code{\link{consensusNet}} function
#' @param edge.width the edge width (default 3)
#' @param vertex.label.cex the size of the vertex label (default 0.8)
#' @param vertex.size the size of the vertex (default 10)
#' @param edge.curved to make the edge curved (default 0.2)
#' @param method community detection method to color the nodes one of "walktrap",
#' "edgeBetweenness", "fastGreedy", "louvain", "spinglass", "leadingEigen",
#' "labelProp", "infomap", "optimal" and "leiden" (default no method)
#' @param ... other parameter
#'
#' @return Union graph beginning and consensus edge coloured, green edges
#' consensus already present in the beginning, blue edges new of the consensus.
#' Community detection of the beggining graph if added.
#' @import igraph robin
#' @export
#'
#' @examples
#' data("adjL_data")
#' con <- consensusNet(adjL_data)
#' plotINet(adjL_data[[1]], con$graphConsensus)




plotINet <- function (adj, graph.consensus, edge.width=3,
                       vertex.label.cex=0.5, vertex.size=10, edge.curved=0.2,
                       method="NA", ...)


{

  ##### Convert adjacency Matrix in graph as it need it

      graph <- igraph::graph_from_adjacency_matrix(adj,
                                                        mode = "upper",
                                                        diag = FALSE,
                                                        weighted = TRUE)


  # Adding vertex new:
  # If no vertex name
  if(length(igraph::V(graph.consensus)$name)==0){
    igraph::V(graph.consensus)$name <- as.character(as.vector(igraph::V(graph.consensus)))
  }

  # If no vertex name
  if(length(V(graph)$name)==0){
    igraph::V(graph)$name <- as.character(as.vector(igraph::V(graph)))
  }


  daAggiungere <- setdiff(igraph::V(graph.consensus)$name, igraph::V(graph)$name)
  Graph <-  igraph::add_vertices(graph,length(daAggiungere))
  V(Graph)$name <- c(igraph::V(graph)$name,daAggiungere)



  UnionGraph <- igraph::union(Graph,graph.consensus)


  Diff <- igraph::difference(graph.consensus,Graph)
  Inter <- igraph::intersection(graph.consensus,Graph)


  #UnionGraph
  ecol <- rep("gray80", igraph::ecount(UnionGraph))

  if(ecount(Diff)>0)
  {
    #Diff:
    edgeDiff <- igraph::as_edgelist(Diff, names = TRUE)
    for (i in 1:(dim(edgeDiff)[1]))
    {
      eI <- igraph::get.edge.ids(UnionGraph , edgeDiff[i,])
      ecol[eI] <- "#619CFF" #"#00abff"
    }
  }


  #Intersect:
  if(ecount(Inter)>0)
  {
    edgeInter <- igraph::as_edgelist(Inter, names = TRUE)
    for (i in 1:(dim(edgeInter)[1]))
    {
      eII <- igraph::get.edge.ids(UnionGraph , edgeInter[i,])
      ecol[eII] <- "#F8766D"
    }
  }


  # Community detection method:
  if(method=="NA"){
      members <- "#00BA38" #"#7CAE00"
  }else{
     members <- robin::membershipCommunities(graph=Graph, method=method, ...)
  }

  # "#C77CFF"

  Isolated <- which(igraph::degree(UnionGraph)==0)
  UnionGraphI <- igraph::delete.vertices(UnionGraph, Isolated)


  plot(UnionGraphI, vertex.size=vertex.size, vertex.label.cex=vertex.label.cex,
       vertex.color=members,edge.color=ecol,edge.curved=edge.curved,
       edge.width= edge.width,
       ...)
}





#' plotL
#' @description This function plots all the layers in one plot.
#' @param graphL List of graphs
#' @param ... other parameter
#'
#' @return plot of graphs
#' @import multinet igraph
#' @export
#'
#' @examples
#' data("graphL_data")
#' plotL(graphL_data)

plotL <- function(graphL, ...)
{
  #it neeeds the names of the actors
   for (z in 1:length(graphL))
   {
     if(is.null(igraph::V(graphL[[z]])$name)){

       igraph::V(graphL[[z]])$name <- seq(1:igraph::vcount(graphL[[z]]))
     }

   }


  ###### CHANGE A IGRAPH IN MULTILAYER:
  n <-  multinet::ml_empty()
  for (l in 1:length(graphL))
  {
    multinet::add_igraph_layer_ml(n, graphL[[l]], name=as.character(l))

  }
  nlayers <- seq(1:length(graphL))
  plot(x=n,layers=nlayers,...)

}





#' plotC
#'
#' @description The function plots the network without isolated nodes.
#' @param graph a graph
#' @param ... other parameter
#'
#' @return plot
#' @import igraph
#' @export
#'
#' @examples
#' data("graphL_data")
#' plotC(graphL_data[[1]])

 plotC <- function(graph, ...)
{
   Isolated <- which(igraph::degree(graph)==0)
   graph <- igraph::delete.vertices(graph, Isolated)
   plot(graph, ...)

}





