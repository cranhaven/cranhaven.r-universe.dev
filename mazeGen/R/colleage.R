# ' @export
# ' @param n This is the node number.
# ' @param rank This is the Rank of the maze.
# ' @description This function tells us which nodes are connected to the nodes.
# ' @details This function tells us which nodes are connected to the node you are interested in based on a given Grid.
# ' @author Aiden Loe
# ' @title colleage

colleage <- function(n,rank){
  a<-n+1
  b<-n+rank
  c<-rank*(rank-1)
  d<-c(a,b)
  if(n>c)
    d<-a
  if (n%%rank==0)
    d<-b
  if (n==rank^2)
    d<-c()
  return(d)
}

