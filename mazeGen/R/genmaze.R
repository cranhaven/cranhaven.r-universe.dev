#' @export
#' @param rank This is the Rank of the maze.
#' @description This function generates the list of edges.
#' @details The Genmaze function generates the list of edges. The edges will be used to construct the maze.
#' @author Aiden Loe
#' @title genMaze
#' @examples
#' genMaze(rank=5)


genMaze<- function(rank=5){
  lista<-c()
  RR<-as.numeric(rank)^2
  for (i in 1:RR){
    col<-colleage(i,rank)
    k<-length(colleage(i,rank))
    for (j in 1:k){
      t<-c(i,colleage(i,rank)[j])
      lista<-c(lista,t)
      }
  }
  A<-length(lista)
  AA<-A-2
  return(lista[1:AA])
}




