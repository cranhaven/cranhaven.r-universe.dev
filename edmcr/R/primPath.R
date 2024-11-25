#' Minimum Spanning Tree Path
#' 
#' \code{primPath} Given a starting node, creates the minimum spanning tree path through a point configuration.
#' 
#' @details 
#' 
#' Given a starting node, compute Prim's algorithm, resulting in the path taken to construct the minimum spanning tree.
#' 
#' @param A the distance matrix for which the minimum spanning tree path will be created
#' @param start the starting node for the path
#' 
#' @return return a 2x(n-1) matrix, where row 1 contains the parent nodes of the MST path, and row 2 contains the corresponding child nodes.
#' 
#' @examples 
#' A <- dist(cbind(rnorm(100,0,1),rnorm(100,0,1)))
#' primPath(as.matrix(A),1)
#' primPath(as.matrix(A),2)
#' 
#' @export

primPath <- function(A,start){
  
  A[which(is.na(A))] <- Inf
  
  ######## INPUT CHECKING ###############
  
  if(nrow(A) != ncol(A)){
    stop("A must be a symmetric matrix")
  }else if(!is.numeric(A)){
    stop("A must be a numeric matrix")
  }else if(!is.matrix(A)){
    stop("A must be a numeric matrix")
  }else if(!isSymmetric(A,tol=1e-8)){
    stop("A must be a symmetric matrix")
  }else if(any(A < 0)){
    stop("All entries in A must be non-negative")
  }
  
  if(!is.numeric(start)){
    stop("start must be a numeric integer")
  }else if(length(start) != 1){
    stop("start must be a numeric integer")
  }else if(!(start %% 2 == 1 | start %% 2 == 0)){
    stop("start must an integer")
  }else if(start > nrow(A)){
    stop("invalid start, must be in [1,nrow(A)]")
  }
  
  #################################################
  ########### MAIN FUNCTION START #################
  #################################################
  
  #Add a small amount of noise to all entries to avoid equality issues
  A <- A + matrix(runif(nrow(A)^2,0,.00000001),nrow=nrow(A))
  
  diag(A) <- Inf
  n <- nrow(A)
  InMST <- rep(0,n)
  
  Parent <- start
  Kid <- as.vector(which(A[start,] == min(A[start,])))
  
  if(length(Kid) > 1){
    Kid <- Kid[1]
  }
  
  InMST[c(Parent,Kid)] <- 1
  
  for(j in 1:(n-2)){
    
    NewDist <- Inf
    NewKid <- NULL
    
    for(k in which(InMST == 1)){
      B <- A
      B[k, which(InMST == 1)] <- Inf
      ProposedKid <- as.vector(which.min(B[k,]))
      
      if(length(ProposedKid) > 1){
        ProposedKid <- ProposedKid[1]
      }
      
      if(A[k,ProposedKid] < NewDist){
        NewDist <- A[k,ProposedKid]
        NewKid <- ProposedKid
        NewParent <- k
      }
    }
    
    Parent <- c(Parent,NewParent)
    Kid  <- c(Kid,NewKid)
    InMST[NewKid] <- 1
  
  }
  
  Path <- rbind(Parent,Kid)
  return(Path)
}