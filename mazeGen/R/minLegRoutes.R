# ' @export
# ' @import igraph
# ' @param nodePosition Tells you all the position of the black dots
# ' @description The minLegRoutes function tells you the possible routes to achieve a maximum score based on the colourNode position with a minimum number legs.
# ' @details The minLegRoutes function tells you the possible routes to achieve a maximum score based on the colourNode position with a minimum number legs.
# ' You need to use the nodePosition function first prior to using this.
# ' @author Aiden Loe and Maria Sanchez
# ' @title minLegRoutes
# ' @examples
# ' rank <- 3
# ' a <- np(rank=3,satPercent=0.5,seed=1)
# ' minLegRoutes(a)

minLegRoutes <- function(nodePosition){

  if("np" %in% class(nodePosition) == FALSE){
    stop("nodePosition must be calculated using the np function.")
  }
  #nodePosition <- np(rank=3,satPercent=0.5,seed=1)
  rank <- nodePosition$rank
  nodePosition <- nodePosition$nodePosition


  #### Lower Grid Maze Nodes ####
  G <- graph(genMaze(rank), directed = TRUE )

  #### Calculate all Path ####
  allPaths <- all_simple_paths(G, 1,lowerGrid(rank))

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

  # print("The optimium path(s) with minimum legs is: ")
   W<-which( LL == min(LL))
  minPaths <- do.call("rbind",allPaths[M[1,W]]) #minimum Paths
  m2 <- 1:nrow(minPaths)
  rownames(minPaths) <- rownames(m2, do.NULL = FALSE, prefix = "pos.Route.")
  colnames(minPaths) <- paste("Step_",0:(ncol(minPaths)-1), sep = "")


  mLRoutes <- list(minSteps = min(LL)-1,
                    minPaths = minPaths,
                    totalminPaths = length(W))

  class(mLRoutes) <- "min"
  return(mLRoutes)

}

 # a <- np(rank=5,satPercent=0.5,seed=1)
 # b<- minLegRoutes(a)
 # b
 # str(b)

