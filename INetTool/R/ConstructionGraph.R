#' constructionGraph
#'
#' @description This function constructs graphs from data with pearson correlation and
#' proportional thresholding (the data should be with the same names (the nodes)
#' in columns for all the matrices).
#' @param data a list of datasets
#' @param perc percentile (default 0.95 it takes the 5 percent of the highest
#' weights)
#'
#' @return Threshold information (highest weight, number of edges, number of nodes,
#' modularity with louvain method), graphs in a list for each layer and weighted
#' adjacency matrices in a list for each layer.
#' @export
#' @import ggpubr
#' @importFrom stats cor quantile
#'
#' @examples
#' data("exampleL_data")
#' constructionGraph(exampleL_data)
constructionGraph <- function(data, perc=0.95)
{


  #Aggiungere plot Multilayer network magnani
  Graphs <- list()
  CorrM <- list()
  Threshold <- list()
  p <- list()
  for (i in 1:length(data))
  {
    CorrM[[i]] <- as.matrix(stats::cor(data[[i]]))

    vet <- as.vector(get_lower_tri_noDiag(CorrM[[i]]))
    vect <- vet[!is.na(vet)]
    weight <- as.data.frame(vect)

    p[[i]] <- ggplot2::ggplot(weight, aes(x=vect)) +
      geom_histogram(color="#e9ecef",fill="#69b3a2")+
      xlab(paste("Weights",i))

    Thresh <- stats::quantile(CorrM[[i]],perc)
    CorrM[[i]][CorrM[[i]]<Thresh] <- 0
    Graphs[[i]] <- igraph::graph_from_adjacency_matrix(CorrM[[i]],
                                                       mode = "upper",
                                                       diag = FALSE,
                                                       weighted=TRUE)
    mod <- igraph::modularity(cluster_louvain(Graphs[[i]]))
    e <- format(igraph::ecount(Graphs[[i]]), scientific = FALSE)
    v <- format(igraph::vcount(Graphs[[i]]), scientific = FALSE)
    Threshold[[i]] <- c(Thresh,e,v,mod)
    names(Threshold[[i]]) <- c(as.character(perc),"edge","node",
                               "modularity Louvain")
  }

  print(ggpubr::ggarrange(plotlist=p))

  output <- list(Threshold=Threshold,
                 Graphs=Graphs,
                 Adj=CorrM)

  return(output)
}


