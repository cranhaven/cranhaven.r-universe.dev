#' @export
#' @import igraph
#' @param rank This is the rank of the maze.
#' @param satPercent Percentage of saturation.
#' @param seed To always get the same position for a local computer.
#' @description Returns the colour node position. You need to use the node position function first.
#' @details This function will not sample from the first node position. If you consider sampling from the first node, then in javascript, the summing of the black dotes need to begin from 1 rather than 0. To keep it simple, always ensure that the first node is not sampled as a black dot.
#' @author Aiden Loe
#' @return A 'np' class which will be used for other functions in the package.
#' @seealso \code{\link{mazeEst}}, \code{\link{genPathSeed}}
#' @title Colour Node Position
#' @examples
#'
#' np(rank=3,satPercent=0.5,seed=1)
#'

#### Colour Node Position #####
np <- function(rank=3, satPercent=0.5, seed=1){
saturation<- ceiling(length(lowerGrid(rank))*satPercent)
set.seed(seed)
nodePosition <- sample(lowerGrid(rank)[-1], saturation, replace=FALSE)

nodePosition <- list(seed = seed,
     rank = rank,
     satPercent= satPercent,
     nodePosition=nodePosition)

class(nodePosition) <- "np"
return(nodePosition)
}



