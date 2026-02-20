#' specificNet
#'
#' @description The function creates Case Specific Networks one for each layer
#' to give information of the peculiar layer not present in the Consensus.
#' @param graphL a list of graphs as igraphs objects.
#' @param graph.consensus graphConsensus output of the
#'  \code{\link{consensusNet}} function.
#'
#' @return Case Specific Networks one for each layer and percentage of
#' specificity.
#' @export
#' @import igraph
#'
#' @examples
#' data("graphL_data")
#' data("adjL_data")
#' myConsensus <- consensusNet(adjL_data)
#' specificNet(graphL_data, myConsensus$graphConsensus)


specificNet <- function (graphL, graph.consensus)


{

  # ##### Convert adjacency Matrix in graph as it need it
  # graph <- vector(mode = "list", length = length(adjL))
  # for (t in 1:length(adjL))
  # {
  #   if(length(rownames(adjL[[1]]))>0)
  #   {
  #     graph[[t]] <- igraph::graph_from_adjacency_matrix(adjL[[t]],
  #                                                       mode = "upper",
  #                                                       diag = FALSE,
  #                                                       add.colnames = "NA",
  #                                                       weighted = TRUE)
  #
  #   }else{
  #     graph[[t]] <- igraph::graph_from_adjacency_matrix(adjL[[t]],
  #                                                       mode = "upper",
  #                                                       diag = FALSE,
  #                                                       weighted = TRUE)
  #   }
  # }


  GraphsDifference <- list()
    percentageOfSpecificity <- NULL
    ###### SpecificNet
    for (t in 1:length(graphL))
    {

        GraphsDifference[[t]] <- igraph::difference(graphL[[t]], graph.consensus)
        names(GraphsDifference)[[t]] <- names(graphL)[[t]]
        percentageOfSpecificity <- c(percentageOfSpecificity,
                                     ecount(GraphsDifference[[t]])/ecount(graphL[[t]]))

    }


    output <- list(GraphsDifference=GraphsDifference,
                   percentageOfSpecificity=percentageOfSpecificity)

    return(output)

    }
