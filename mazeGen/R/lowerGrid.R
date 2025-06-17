#' @export
#' @param rank This is the rank of the maze.
#' @description This tells you all the node position in the maze.
#' @details The construction of the maze is first created in a symmetrical format. However, only half of the nodes are kept in order to create the actual maze. Hence, this function calculates the nodePosition of the actual maze.
#' @author Aiden Loe
#' @title lowerGrid
#' @examples
#' lowerGrid(3)

#returns the lower grid
lowerGrid <-function(rank=5){
  M<-c()
  for (j in 1:rank){
    k<-rank-j
    M<-c(M,((j-1)*rank+1):((j*rank)-(j-1)))
  }
return(M)
}




