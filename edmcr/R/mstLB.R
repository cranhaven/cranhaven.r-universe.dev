#' Minimum Spanning Tree Preserving Lower Bound
#'
#' \code{mstLB} Returns an nxn matrix containing the lower bounds for all unknown entries
#' in the partial distance matrix D such that the minimum spanning tree of the partial matrix D
#' is preserved upon completion.
#' 
#' @details 
#' The insight in constructing the lower bound is drawn from single-linkage clustering. 
#' Every edge in a spanning tree separates the vertices into two different groups, 
#' depending on which points remain connected to either one vertex or the other of that edge.
#' Because the tree is a minimum spanning tree, if we select the largest edge, then the distance
#' between any vertex of one group and any vertex of the other group must be at least as large as
#' that of the the largest edge.  This gives a lower bound for these distances that will preserve 
#' that edge in the minimum spanning tree.  The same reasoning is applied recursively to each separate
#' group, thus producing a lower bound on all edges.
#' 
#' The details of the algorithm can be found in Rahman & Oldford (2016).
#'
#' @param D An nxn partial distance matrix to be completed
#' 
#' @return Returns an nxn matrix containing the lower bound for the unknown entries in D
#' 
#' @examples 
#' 
#' D <- matrix(c(0,3,4,3,4,3,
#'              3,0,1,NA,5,NA,
#'              4,1,0,5,NA,5,
#'              3,NA,5,0,1,NA,
#'              4,5,NA,1,0,5,
#'              3,NA,5,NA,5,0),byrow=TRUE, nrow=6)
#' mstLB(D)
#' 
#' @references
#' Rahman, D., & Oldford R.W. (2016). Euclidean Distance Matrix Completion and Point Configurations from the Minimal Spanning Tree.
#' 
#' @export 

mstLB <- function(D){
  
  ############# INPUT CHECKING ###############
  
  #General Checks for D
  if(nrow(D) != ncol(D)){
    stop("D must be a symmetric matrix")
  }else if(!is.numeric(D)){
    stop("D must be a numeric matrix")
  }else if(!is.matrix(D)){
    stop("D must be a numeric matrix")
  }else if(any(diag(D) != 0)){
    stop("D must have a zero diagonal")
  }else if(!isSymmetric(D,tol=1e-8)){
    stop("D must be a symmetric matrix")
  }else if(!any(is.na(D))){
    stop("D must be a partial distance matrix. Some distances must be unknown.")
  }
  
  ###############################################
  ############ MAIN FUNCTION START ##############
  ###############################################
  
  diag(D) <- Inf
  n <- nrow(D)
  LB <- matrix(rep(0,n^2),nrow=n)
  
  for(i in 1:n){
    Pathi <- primPath(D,i)
    Parents <- Pathi[1,]
    Kids <- Pathi[2,]
    
    InMST <- rep(0,n)
    InMST[i] <- 1
    
    for(j in 1:(n-1)){
      ActuallyInMST <- which(InMST == 1)
      ActuallyNotInMST <- which(InMST == 0)
      for(k in 1:length(ActuallyInMST)){
        for(l in 1:length(ActuallyNotInMST)){
          if(LB[ActuallyInMST[k],ActuallyNotInMST[l]] < D[Parents[j],Kids[j]]){
            LB[ActuallyInMST[k],ActuallyNotInMST[l]] <- D[Parents[j],Kids[j]]
          }
        }
      }
      InMST[Kids[j]] <- 1
    }
  }
  
  #Add a small amount of noise to avoid equality issues
  
  LB <- LB + abs(matrix(runif(n^2,0,.00000001), nrow=n))
  
  #Make the Lower Bound Symmetric
  
  Index1 <- which(LB[upper.tri(LB)] < t(LB)[upper.tri(LB)])
  Index2 <- which(LB[lower.tri(LB)] < t(LB)[lower.tri(LB)])
  
  LB[upper.tri(LB)][Index1] <- t(LB)[upper.tri(LB)][Index1]
  LB[lower.tri(LB)][Index2] <- t(LB)[lower.tri(LB)][Index2]
  
  #Set diagonals to 0
  
  diag(LB) <- 0
  
  return(LB)
  
}