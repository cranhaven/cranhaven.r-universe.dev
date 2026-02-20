#' densityNet
#'
#' @description This function creates a density plot of the different graphs mean
#' weights. It can be used to search the final Threshold for the Consensus
#' Network starting from similar networks.
#' @param graphL the list of weighted graphs in igraph format.
#'
#' @return
#' the quantile of the mean density distribution,
#' the quantile of the mean density distribution without the zeros,
#' plot density distribution without the zeros
#' @export
#' @import igraph ggplot2
#'
#' @examples
#' data("graphL_data")
#' densityNet(graphL_data)



densityNet <- function (graphL)


{

  ###### Function Consensus
  Mat <- vector(mode = "list", length = length(graphL))
  for (z in 1:length(graphL))
  {
    Mat[[z]] <- as.matrix(igraph::as_adjacency_matrix(graphL[[z]], attr="weight"))
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


  triA <- get_lower_tri_noDiag(matrix)
  vettriA <- as.vector(triA)
  vect <- vettriA[!is.na(vettriA)]




  #plot <- ggplot(weight, aes(x=vect)) + geom_histogram(color="#e9ecef",fill="#69b3a2")
  #print(plot)
  quant <- stats::quantile(vect, probs = seq(0, 1, 0.05))


  vect0 <- vect[vect>0]
  weight <- as.data.frame(vect0)
  quant0 <- stats::quantile(vect0, probs = seq(0, 1, 0.05))
  plot0 <- ggplot2::ggplot(weight, ggplot2::aes(x=vect0)) +
    ggplot2::geom_density(color="#e9ecef",fill="#69b3a2")+
    ggplot2::xlab("mean weights without 0")
  print(plot0)

  output <- list(quantile=quant,
                 quantileNo0=quant0)

  return(output)
}
