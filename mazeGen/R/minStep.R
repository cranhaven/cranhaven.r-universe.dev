# ' @export
# ' @param nodePosition  Tells you all the position of the black dots.
# ' @description This function tells us the minimum steps to achieve the maximums core given the colour node positions.
# ' @details This function tells us the minimum steps to achieve the maximums core given the colour node positions.
# ' @author Aiden Loe
# ' @title minStep
# ' @examples \dontrun{
# '
# ' rank <- 5
# ' satPercent <- 0.5
# '
# ' nodeLength(rank)# longest row
# '
# ' nodePosition <- np(rank, satPercent, 2) ### always set seed.
# '
# ' #Get Max Score
# ' maxScore(nodePosition)
# '
# ' maxRoutes <- maxScoreRoutes(nodePosition)
# '
# ' # Get minimum steps required to get maximum score
# ' minStep(nodePosition)
#
# ' }

#number of steps
minStep <- function(nodePosition){

  if("np" %in% class(nodePosition) == FALSE){
    stop("nodePosition must be calculated using the np function.")
  }

  rank <- nodePosition$rank
  nodePosition <- nodePosition$nodePosition
#allPaths
allPaths<-c()
G <- graph(genMaze(rank), directed = TRUE )
allPaths <- all_simple_paths(G,1,lowerGrid(rank))

#### max colour gives you the points for every route on a black dot ####
maxColour <- NULL
for(i in 1:length(allPaths)){
  maxColour[[i]] <- ifelse(as.numeric(allPaths[[i]]) %in% nodePosition,1,0)
}

#### summing up the total score ####
totalScore <- NULL
for(i in 1:length(allPaths)){
  totalScore[i]<- sum(maxColour[[i]])
}

totalScore.df <- as.data.frame(totalScore)
index <- 1:nrow(totalScore.df)
totalScore.df.1<- cbind.data.frame(index,totalScore.df)
optimisedScore <- totalScore.df.1[which(totalScore.df.1$totalScore == max(totalScore.df.1$totalScore, na.rm = TRUE)), ]

n<-nrow(optimisedScore)
#### All the optimised routes #####
#number of steps & optimal paths
LL<-c()
for (j in 1:n){
  M<-matrix(unlist(optimisedScore),ncol=n,byrow=TRUE)
  N<-unlist(maxColour[M[1,j]])
  LL<-c(LL,length(N))
}
#print("the minimum number of steps for the optimal solution is: ")
return(min(LL)-1)



}






