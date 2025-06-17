# ' @export
# ' @import igraph
# ' @param nodePosition tells you all the position of the black dots
# ' @description This function returns the frequently of paths going through at least one black dots
# ' @details This function returns the frequently of paths going through at least one of black dots
# ' @author Aiden Loe and Maria
# ' @title blackNodeRoutes
# ' @examples
# ' a <- np(rank=5,satPercent=0.5,seed=1)
# ' blackNodeRoutes(a)



blackNodeRoutes <- function(nodePosition){


  if("np" %in% class(nodePosition) == FALSE){
    stop("nodePosition must be calculated using the np function.")
  }

#   if(rank != nodePosition$rank){
#     stop("The input rank and the rank to calculate the colour node positions are not the same.")
#   }

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
n<-nrow(totalScore.df.1)
M<-matrix(unlist(totalScore.df.1),ncol=n,byrow=TRUE)


#number of steps & optimal paths
LL<-c()

for (j in 1:length(totalScore)){
  M<-matrix(unlist(totalScore.df.1),ncol=n,byrow=TRUE)
  N<-unlist(maxColour[M[1,j]])
  LL<-c(LL,length(N)) #length of every route
}


# We only want routes that is the same as the rank
W<-which( LL == rank)

endScore <- totalScore.df.1[which(totalScore.df.1$index %in% W),]

table(endScore$totalScore)
return(table(endScore$totalScore))
}


#  rank <- 5
# a <- np(rank=5,satPercent=0.5,seed=1)
# a$nodePosition
#  blackNodeRoutes(a)
