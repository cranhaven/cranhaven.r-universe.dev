# ' @export
# ' @import igraph
# ' @param nodePosition tells you all the position of the black dots
# ' @description This tells you all the possible routes to achieve a maximum score based on the colourNode position and a given rank.
# ' @details The maxScoreRoutes function tells you the possible routes to achieve a maximum score.
# ' You need to use the nodePosition function first prior to using this.
# ' @author Aiden Loe and Maria
# ' @title maxScoreRoutes
# ' @examples
# ' a <- np(rank=3,satPercent=0.5,seed=1)
# ' maxScoreRoutes(a)


maxScoreRoutes <- function(nodePosition){

  if("np" %in% class(nodePosition) == FALSE){
    stop("nodePosition must be calculated using the np function.")
  }

  #set.seed(set.seed)
  #nodePosition <- colourNodePosition(rank, satPercent,set.seed)
  rank <- nodePosition$rank
  nodePosition <- nodePosition$nodePosition

  ##COMPUTE THE PATHS

  #all of them
  #allPaths
  G <- graph(genMaze(rank), directed = TRUE )
  allPaths<-c()
  allPaths <- all_simple_paths(G,1,lowerGrid(rank))

  #### max colour gives you the points for every route on a black dot ####
  maxColour <- NULL
  for(i in 1:length(allPaths)){
    maxColour[[i]] <- ifelse(as.numeric(allPaths[[i]]) %in% nodePosition,1,0)
  }

  ##allPaths
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
  M<-matrix(unlist(optimisedScore),ncol=n,byrow=TRUE)


  maxnu<-M[2,1]


  #number of steps & optimal paths
  LL<-c()

  for (j in 1:n){
    M<-matrix(unlist(optimisedScore),ncol=n,byrow=TRUE)
    N<-unlist(maxColour[M[1,j]])
    LL<-c(LL,length(N))
  }

  print("The optimal paths are: ")
  W<-which( LL == rank)
  print(allPaths[M[1,W]])

  #print("The minimum number of steps for the optimal solution is: ")
  #print(min(LL)-1)

  print(("the number of solutions is: "))

  return(length(W))
}


# rank <- 8
# nodePosition <- colourNodePosition(rank=8,satPercent=0.5,seed=1)
# maxScoreRoutes(rank,a)
# solution(rank,a)
#
# a <- allPaths[M[1,W]]
# do.call("rbind",a)
