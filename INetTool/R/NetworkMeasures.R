#' measuresNet
#'
#' @description This function computes graphs and nodes measures to analyse all
#'  the layers in one shot.
#' @param graphL a list of graphs as igraphs objects.
#' @param nodes.measures logical, if falso it computes only graph measures, if
#' true it computes also nodes measures (default TRUE).
#'
#' @return list of measure for each layer.
#' @import igraph
#' @export
#'
#' @examples
#' data("graphL_data")
#' measuresNet(graphL_data)
#'
measuresNet <- function(graphL, nodes.measures=TRUE)
{

    MeasuresGraphs <- vector(mode = "list", length = length(graphL))
   for (i in 1:length(graphL))
   {
       # graphs Measures
       verte <- igraph::vcount(graphL[[i]])
       edge <- igraph::ecount(graphL[[i]])
       tran <- igraph::transitivity(graphL[[i]],type="global")#Transitivity measures the probability that the adjacent vertices of a vertex are connected. This is sometimes also called the clustering coefficient.
       diam <- igraph::diameter(graphL[[i]])#The diameter of a graphs is the length of the longest between the shortest path
       modl <- igraph::modularity(graphL[[i]],cluster_louvain(graphL[[i]],weights = NULL)$membership)
       den <- igraph::edge_density(graphL[[i]]) #The density of a graphs is the ratio of the number of edges and the number of possible edges.
       as <- igraph::assortativity_degree(graphL[[i]]) #The assortativity coefficient is positive if similar vertices (based on some external property) tend to connect to each, and negative otherwise.
       ceD <- igraph::centr_degree(graphL[[i]])$centralization#Centralize a graphs according to the degrees of vertices
       ceB <- igraph::centr_betw(graphL[[i]])$centralization

       graphsMeasures <- c(verte,edge,tran,diam,modl,den,as,ceD,ceB)
       names(graphsMeasures) <- c("vertices","edges","transitivity","diameter","modularityLouvain","edgeDensity","assortativity","centrDegree","centrBetween")
       graphsMeasures <- as.matrix(graphsMeasures)
       colnames(graphsMeasures) <- c("values")

       if(nodes.measures==TRUE)
       {
       # Node Measures
       d <- igraph::degree(graphL[[i]])
       tranloc <- igraph::transitivity(graphL[[i]],type="local")
       bet <- igraph::betweenness(graphL[[i]])#defined by the number of geodesics (shortest paths) going through a vertex or an edge
       hub <- igraph::hub_score(graphL[[i]], weights = NULL)$vector#The hub scores of the vertices are defined as the principal eigenvector of A*t(A), where A is the adjacency matrix of the graphs.
       NodeMeasures <- cbind(d,tranloc,bet,hub)
       colnames(NodeMeasures) <- c("degree", "transitivityLocal","betweenness", "hub")

       #  library(ggplot2)
       # HistMeasure <- as.data.frame(NodeMeasures)
       # p <- list()
       # for (j in 1:(dim(HistMeasure)[2]))
       # {
       #     p[[j]] <- ggplot(HistMeasure, aes(x=HistMeasure[,j])) + geom_histogram(color="#e9ecef",fill="#69b3a2")+labs(title=colnames(HistMeasure)[j])
       #     print(j)
       # }
       #
       # print(ggpubr::ggarrange(plotlist=p))
       #
       #

           MeasuresGraphs[[i]] <- list( graphsMeasures=graphsMeasures,
                                    nodeMeasures=NodeMeasures)
       }else{

           MeasuresGraphs[[i]] <- list( graphsMeasures=graphsMeasures)
       }



   }






    return(MeasuresGraphs)
}
