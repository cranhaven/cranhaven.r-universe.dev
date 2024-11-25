#' Guided Random Search
#'
#' \code{grs} performs Euclidean Distance Matrix Completion using the guided random search algorithm
#' of Rahman & Oldford. Using this method will preserve the minimum spanning tree in the partial distance 
#' matrix.
#' 
#' @details 
#' The matrix D is a partial-distance matrix, meaning some of its entries are unknown. 
#' It must satisfy the following conditions in order to be completed:
#' \itemize{
#' \item{diag(D) = 0}
#' \item{If \eqn{a_{ij}} is known, \eqn{a_{ji} = a_{ij}}}
#' \item{If \eqn{a_{ij}} is unknown, so is \eqn{a_{ji}}}
#' \item{The graph of D must contain ONLY the minimum spanning tree distances}
#' }
#' 
#' @param D An nxn partial-distance matrix to be completed. D must satisfy a list of conditions (see details), with unkown entries set to NA
#' @param d The dimension for the resulting completion.
#' 
#' @return 
#' \item{P   }{The completed point configuration in dimension d}
#' \item{D   }{The completed Euclidean distance matrix}
#' 
#' @examples 
#' #D matrix containing only the minimum spanning tree
#' D <- matrix(c(0,3,NA,3,NA,NA,
#'               3,0,1,NA,NA,NA,
#'               NA,1,0,NA,NA,NA,
#'               3,NA,NA,0,1,NA,
#'               NA,NA,NA,1,0,1,
#'               NA,NA,NA,NA,1,0),byrow=TRUE, nrow=6)
#'               
#' edmc(D, method="grs", d=3)
#' 
#' @references
#' Rahman, D., & Oldford, R.W. (2016). Euclidean Distance Matrix Completion and Point Configurations from the Minimal Spanning Tree. 
#' 
#' @export
#' @importFrom MASS mvrnorm
#' @importFrom vegan spantree

grs <- function(D,d){
  
  ############ INPUT CHECKING ##################
  
  #General Checks for D
  if(nrow(D) != ncol(D)){
    stop("D must be a symmetric matrix")
  }else if(!is.numeric(D)){
    stop("D must be a numeric matrix")
  }else if(!is.matrix(D)){
    stop("D must be a numeric matrix")
  }else if(any(diag(D) != 0)){
    stop("D must have a zero diagonal")
  }else if(!isSymmetric(unname(D),tol=1e-8)){
    stop("D must be a symmetric matrix")
  }else if(!any(is.na(D))){
    stop("D must be a partial distance matrix. Some distances must be unknown.")
  }
  
  #Check that D is connected
  Test <- D
  Test[which(is.na(D))] <- 0
  TestSums <- rowSums(Test)
  
  if(any(TestSums == 0)){
    stop("D must be a connected matrix")
  }
  
  #Check if there are more than MST entries in D
  
  Count <- length(which(!is.na(D)))
  
  if(Count > 2*(nrow(D)-1) + nrow(D)){
    stop("D contains non-MST entries")
  }
  
  #Check that d is numeric and >0
  
  if(!is.numeric(d)){
    stop("d must be a numeric integer")
  }else if(length(d) != 1){
    stop("d must be a numeric integer")
  }else if(!(d %% 2 == 1 | d %% 2 == 0)){
    stop("d must an integer")
  } 
  
  ###########################################################
  ################ MAIN FUNCTION START ######################
  ###########################################################

  tol <- 1e-8
  
  n <- nrow(D)
  AssignedPoints <- rep(0,n)
  AssignedDistances <- c()
  P <- matrix(rep(NA,n*d),nrow=n)
  
  #Compute the Index matrix to determine the degree of each node in D
  
  IndexMatrix <- matrix(rep(0,n^2), nrow=n)
  IndexMatrix[which(D > 0)] <- 1
  Degree <- rowSums(IndexMatrix)
  
  #WLOG Assign the point with highest degree to the origin
  MaxDegree <- which(Degree == max(Degree))
  
  if(length(MaxDegree) > 1){
    MaxDegree <- sample(MaxDegree,1)
  }
  
  P[MaxDegree,] <- rep(0,d)
  AssignedPoints[MaxDegree] <- 1
  
  #Place all points attached to this point
  
  Fail <- TRUE
  
  StoredP <- P
  StoredAssignedDistances <- AssignedDistances
  StoredAssignedPoints <- AssignedPoints
  
  while(Fail){
  
    Fail <- FALSE
    Tries <- 0
    
    P <- StoredP
    AssignedDistances <- StoredAssignedDistances
    AssignedPoints <- StoredAssignedPoints
    
  for(i in 1:Degree[MaxDegree]){
    
    Accept <- FALSE
    
    Base <- P[MaxDegree,]
    AttachedPoints <- which(IndexMatrix[MaxDegree,] == 1)
    
    while(!Accept){
      
      TestScatter <- P 
      TestDistances <- AssignedDistances
      
      TestScatter[AttachedPoints[i],] <- rgrsNewPoint(Base,D[MaxDegree,AttachedPoints[i]],d)
      TestDistances <- c(TestDistances,D[MaxDegree,AttachedPoints[i]])
    
      #Check MST
    
      PlacedPoints <- TestScatter[which(!is.na(TestScatter[,1])),]
      Accept <- rgrsCheckMST(PlacedPoints,TestDistances,sum(AssignedPoints)-1,tol)
    
      if(Accept){
        P <- TestScatter
        AssignedDistances <- TestDistances
        AssignedPoints[AttachedPoints[i]] <- 1
      }else{
        Tries <- Tries + 1
        if(Tries > 10000){
          i <- Degree[MaxDegree]
          Accept <- TRUE
          Fail <- TRUE
        }
      }
    }
  }
  }
  
  Degree[MaxDegree] <- -1
  
  #Assign the remaining points
    
  while(sum(AssignedPoints) < n){
    
    #Find base
    PIndex <- c(1:n)
    AvailablePoints <- P[which(AssignedPoints == 1),]
    WhichP <- PIndex[which(AssignedPoints == 1)]
    
    AssignedDegrees <- Degree[which(AssignedPoints == 1)]
    MaxAssignedDegrees <- unique(AssignedDegrees[which(AssignedDegrees == max(AssignedDegrees))])
    
    Base <- AvailablePoints[which(AssignedDegrees == MaxAssignedDegrees)[1],]
    ChosenP <- WhichP[which(AssignedDegrees == MaxAssignedDegrees)[1]]
    
    for(i in 1:(MaxAssignedDegrees[1]-1)){
      
      Accept <- FALSE
      
      AttachedPoints <- which(IndexMatrix[ChosenP,] == 1)
      NextPoint <- AttachedPoints[which(AssignedPoints[AttachedPoints] == 0)][1]
      
      Tries <- 0
      
      while(!Accept){
        
        Tries <- Tries + 1
        
        if(Tries > 10000){
          return(FALSE)
          break
        }
        
        TestScatter <- P 
        TestDistances <- AssignedDistances
        
        TestScatter[NextPoint,] <- rgrsNewPoint(Base,D[ChosenP,NextPoint],d)
        TestDistances <- c(TestDistances,D[ChosenP,NextPoint])
        
        #Check MST
        
        PlacedPoints <- TestScatter[which(!is.na(TestScatter[,1])),]
        Accept <- rgrsCheckMST(PlacedPoints,TestDistances,sum(AssignedPoints)-1,tol)
        
        if(Accept){
          P <- TestScatter
          AssignedDistances <- TestDistances
          AssignedPoints[NextPoint] <- 1
        }
      }
      
    }
    
    Degree[ChosenP] <- -1
  }
  
  DStar <- as.matrix(dist(P))

  return(list(P=P,D=DStar))
  
}
