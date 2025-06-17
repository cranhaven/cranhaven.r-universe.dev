# ' @export
# ' @param rank This is the Rank of the maze
# ' @param nodePosition Tells you all the position of the black dots.
# ' @description This function tells us the solution for all series of routes including both black and non black dots.
# ' @details This function tells us the solution for all series of routes including both black and non black dots.
# ' @author Aiden Loe and Maria Sanchez
# ' @title solution
# ' @examples
# ' a <- np(rank=3,satPercent=0.5,seed=1)
# ' solution(a)

solution <- function(nodePosition){
  if("np" %in% class(nodePosition) == FALSE){
    stop("nodePosition must be calculated using the np function.")
  }


  rank <- nodePosition$rank
  nodePosition <- nodePosition$nodePosition

  print("The black points are in nodes: ")
  print(nodePosition)

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

  print("The maximum number of black points you can get is: ")

  maxnu<-M[2,1]
  print(M[2,1])

  #number of steps & optimal paths
  LL<-c()

  for (j in 1:n){
    M<-matrix(unlist(optimisedScore),ncol=n,byrow=TRUE)
    N<-unlist(maxColour[M[1,j]])
    LL<-c(LL,length(N)) # calculates the length of all the routes
  }

  print("The optimal paths are: ")
  W<-which( LL == rank) #only select those that reaches to the top
  minPath <- allPaths[M[1,W]]
  minPath<- do.call("rbind",minPath)
  m2<- 1:nrow(minPath)
  rownames(minPath) <- rownames(m2, do.NULL=FALSE, prefix="min.Routes.")
  print(minPath)
  print("The minimum number of steps for the optimal solution is: ")
  print(min(LL)-1)
  print(("The number of solutions is: "))

  return(length(W))
}


