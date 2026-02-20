#' thresholdNet
#'
#' @description The function reconstructs the Consensus Network with different
#' thresholding after the \code{\link{consensusNet}} function starting from
#' similar graphs.
#' @param sim.graphL a list of similarGraphs output of the
#' \code{\link{consensusNet}} function.
#' @param threshold different threshold to compute.
#'
#' @return a new consensus network igraph object.
#' @export
#' @import igraph
#'
#' @examples
#' data("adjL_data")
#' myConsensus <- consensusNet(adjL_data)
#' thresholdNet(myConsensus$similarGraphs)


thresholdNet <- function (sim.graphL, threshold=0.5)


{

###### Function Consensus
Mat <- vector(mode = "list", length = length(sim.graphL))
for (z in 1:length(sim.graphL))
{
  Mat[[z]] <- as.matrix(igraph::as_adjacency_matrix(sim.graphL[[z]], names = TRUE,
                                                    attr="weight"))
}

matrixMean <- matrix(0, nrow=dim(Mat[[1]])[1], ncol=dim(Mat[[1]])[1])

for (i in 1:dim(Mat[[1]])[1])
{
  for(j in 1:dim(Mat[[1]])[2]){

    vect <- NULL
    for(k in 1:length(Mat))
    {
      Weig <- c(Mat[[k]][i,j])
      vect <- c(vect,Weig)
    }

    matrixMean[i,j] <- mean(vect)


  }
}
matrix <- as.matrix(matrixMean)
matrix[matrix < threshold] <- 0
if(length(colnames(Mat[[1]]))!=0){
  rownames(matrix) <- rownames(Mat[[1]])
  colnames(matrix) <- colnames(Mat[[1]])
}


graphConsensus <- igraph::graph_from_adjacency_matrix(matrix, mode = "upper",
                                                      diag = FALSE,
                                                      weighted = TRUE)



return(graphConsensus)
}
