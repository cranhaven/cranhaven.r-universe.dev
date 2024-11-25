#'Shortest Path Upper Bound
#'
#'\code{mstUB} Compute the shortest path upper bound for all unknown entries in a partial distance matrix
#'
#'@details
#'
#'This function uses the shortest.paths() function, available in the igraph package.
#'
#'@param A A (connected) partial distance matrix, with unknown entries set to Inf
#'
#'@return UB A matrix containing the upper bounds for only the unknown entries. All other entries will be set to Inf.
#'
#'@examples
#'
#'A <- dist(cbind(rnorm(10,0,1),rnorm(10,0,1)))
#'mstUB(as.matrix(A))
#'
#'@export
#'@import igraph
mstUB <- function(A){
  
  ############# INPUT CHECKING ###############
  
  #General Checks for A
  if(nrow(A) != ncol(A)){
    stop("A must be a symmetric matrix")
  }else if(!is.numeric(A)){
    stop("A must be a numeric matrix")
  }else if(!is.matrix(A)){
    stop("A must be a numeric matrix")
  }else if(any(diag(A) != 0)){
    stop("A must have a zero diagonal")
  }else if(!isSymmetric(A,tol=1e-8)){
    stop("A must be a symmetric matrix")
  }
  
  ###############################################
  ############ MAIN FUNCTION START ##############
  ###############################################
  
  n <- nrow(A)
  diag(A) <- Inf
  
  GraphDF <- matrix(rep(0,3*n*(n-1)/2),nrow=n*(n-1)/2)
  
  PlaceHolder <- 1
  
  for(i in 2:nrow(A)){
    for(j in 1:(i-1)){
      GraphDF[PlaceHolder,1] <- j
      GraphDF[PlaceHolder,2] <- i
      GraphDF[PlaceHolder,3] <- A[i,j]
      PlaceHolder <- PlaceHolder + 1
    }
  }
  Graph <- as.data.frame(GraphDF)
  
  NewGraph <- graph.data.frame(Graph[,c(1,2)])
  ShortestPath <- shortest.paths(NewGraph,weights = Graph[,3])
  
  return(ShortestPath)
}