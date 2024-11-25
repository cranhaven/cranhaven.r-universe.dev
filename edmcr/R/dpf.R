#' Dissimilarity Parameterization Formulation
#' 
#' \code{dpf} returns a completed Euclidean Distance Matrix D, with dimension d,
#' from a partial Euclidean Distance Matrix using the methods of Trosset (2000)
#' 
#' @details This is an implementation of the Dissimilarity Parameterization Formulation (DPF)
#' for Euclidean Distance Matrix Completion, as proposed in 'Distance Matrix Completion by Numerical
#' Optimization' (Trosset, 2000).
#' 
#' The method seeks to minimize the following:
#' 
#' \deqn{\sum_{i=1}^{d}(\lambda_{i} - \lambda_{max}) + \sum_{i=d+1}^{n}\lambda_{i}^{2}}
#' 
#' where \eqn{\lambda_{i}} are the ordered eigenvalues of \eqn{\tau(\Delta)}. For details, see Trosset(2000)
#' 
#' The matrix D is a partial-distance matrix, meaning some of its entries are unknown. 
#' It must satisfy the following conditions in order to be completed:
#' \itemize{
#' \item{diag(D) = 0}
#' \item{If \eqn{a_{ij}} is known, \eqn{a_{ji} = a_{ij}}}
#' \item{If \eqn{a_{ij}} is unknown, so is \eqn{a_{ji}}}
#' \item{The graph of D must be connected. If D can be decomposed into two (or more) subgraphs, 
#'    then the completion of D can be decomposed into two (or more) independent completion problems.}
#' }
#' 
#' @param D An nxn partial-distance matrix to be completed. D must satisfy a list of conditions (see details), with unkown entries set to NA
#' @param d The dimension for the resulting completion.
#' @param toler The convergence tolerance of the algorithm. Set to a default value of 1e-8
#' @param lower An nxn matrix containing the lower bounds for the unknown entries in D. 
#' If NULL, lower is set to be a matrix of 0s.
#' @param upper An nxn matrix containing the upper bounds of the unknown entries in D. 
#' If NULL, upper[i,j] is set to be the shortest path between node i and node j.
#' @param retainMST D logical input indicating if the current minimum spanning tree structure in D should be retained.
#' If TRUE, a judicious choice of Lower is calculated internally such that the MST is retained.
#' 
#' @return 
#' \item{D}{The completed distance matrix with dimensionality d}
#' \item{optval}{The minimum function value achieved during minimization (see details)}
#' 
#' @examples 
#' set.seed(1337)
#' D <- matrix(c(0,3,4,3,4,3,
#'              3,0,1,NA,5,NA,
#'              4,1,0,5,NA,5,
#'              3,NA,5,0,1,NA,
#'              4,5,NA,1,0,5,
#'              3,NA,5,NA,5,0),byrow=TRUE, nrow=6)
#' 
#' edmc(D, method="dpf", d=3, toler=1e-8)
#' 
#' @references 
#' Trosset, M.W. (2000). Distance Matrix Completion by Numerical Optimization.Computational Optimization and Applications, 17, 11–22, 2000.
#' 
#' @export

dpf <- function(D,
                d,
                toler = 1e-8,
                lower=NULL,
                upper=NULL, 
                retainMST=FALSE){
  
  #compute lower bound so it can be checked before function start
  
  ##Check retainMST input first
  if(retainMST != TRUE && retainMST != FALSE){
    stop("retainMST must be either TRUE or FALSE")
  }
  
  if(retainMST){
    lower <- mstLB(D)
  }
  
  ################## Input Checking #################
  
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
  
  #Check that d is numeric and >0
  if(!is.numeric(d)){
    stop("d must be a numeric integer")
  }else if(length(d) != 1){
    stop("d must be a numeric integer")
  }else if(!(d %% 2 == 1 | d %% 2 == 0)){
    stop("d must be a numeric integer")
  } 
  
  #General Checks for lower bound
  if(!is.null(lower)){
    if(nrow(lower) != ncol(lower)){
      stop("lower must be a symmetric matrix")
    }else if(!is.numeric(lower)){
      stop("lower must be a numeric matrix")
    }else if(!is.matrix(lower)){
      stop("lower must be a numeric matrix")
    }else if(any(diag(lower) != 0)){
      stop("lower must have a zero diagonal")
    }else if(!isSymmetric(lower,tol=1e-8)){
      stop("lower must be a symmetric matrix")
    }
  }
  
  #General Checks for upper bound
  if(!is.null(upper)){
    if(nrow(upper) != ncol(upper)){
      stop("upper must be a symmetric matrix")
    }else if(!is.numeric(upper)){
      stop("upper must be a numeric matrix")
    }else if(!is.matrix(upper)){
      stop("upper must be a numeric matrix")
    }else if(any(diag(upper) != 0)){
      stop("upper must have a zero diagonal")
    }else if(!isSymmetric(upper,tol=1e-8)){
      stop("upper must be a symmetric matrix")
    }
  }
  
  #Check that is lower and upper bounds are provided, the upper bound is bigger than the lower bound
  if(!is.null(lower) & !is.null(upper) && any(lower > upper)){
    stop("lower bounds must be greater than or equal to upper bound")
  }
  
  ############## FUNCTION START #############
  
  #Extract Information from user provided Matrix
  
  #Calculate the Index for the lower triangular unknown entries
  k <- nrow(D)
  Index <- which(is.na(D))
  Rows <- Index %% k
  Rows[which(Rows==0)] <- k
  Cols <- ceiling(Index/k)
  Keep <- which(Rows > Cols)
  
  #Find the Known Entries
  KnownEntries <- D[lower.tri(D)][which(!is.na(D[lower.tri(D)]))]
    
  #Store the Number of Rows
  n <- nrow(D)
  
  if(is.null(lower)){
    lower <- rep(0,length(Index)/2)
  }else{
    DST1 <- sqrt(buildMatrix(rep(Inf,length(Index)),KnownEntries,Index,n))
    
    PLower <- lower.tri(DST1)
    Index2 <- which(is.infinite(DST1[PLower]))
    lower <- (lower[PLower][Index2])^2
  }
  
  if(is.null(upper)){
    DST1 <- sqrt(buildMatrix(rep(Inf,length(Index)),KnownEntries,Index,n))

    MaxMSTDist <- mstUB(DST1)
    diag(MaxMSTDist) <- Inf
    
    PLower <- lower.tri(DST1)
    Index2 <- which(is.infinite(DST1[PLower]))
    upper <- (MaxMSTDist[PLower][Index2])^2
  }else{
    DST1 <- sqrt(buildMatrix(rep(Inf,length(Index)),KnownEntries,Index,n))
    
    PLower <- lower.tri(DST1)
    Index2 <- which(is.infinite(DST1[PLower]))
    upper <- (upper[PLower][Index2])^2
  }
  
  if(any(lower > upper)){
    lower[which(lower > upper)] <- upper[which(lower > upper)] - runif(length(which(lower > upper)),0,.00000001)
  }
  
  #Get lower triangular index values only
    Rows <- Index %% n
    Rows[which(Rows==0)] <- n
    Cols <- ceiling(Index/n)
    Keep <- which(Rows > Cols)
    Index <- Index[Keep]
  
  #DPF Optimization
  
  RandomStart <- sapply(c(1:length(lower)), function(Ind,LB,UB){runif(1,LB[Ind],UB[Ind])},LB=lower,UB=upper)
  
  #Get The Inputs in the correct form
  KnownEntries <- KnownEntries
  Index <- Index
  NRows <- n
  Dimension <- d
  
  Optimized <- optim(RandomStart,fn=dpfFn,gr=dpfGr,Index=Index, KnownEntries=KnownEntries,NRows=NRows,Dimension=Dimension,lower=lower,upper = upper,method="L-BFGS-B")
  MinVal <- Optimized$value
  
  if(MinVal > 1e-8){
    
    NewOptimized <- optim(Optimized$par,fn=dpfFn,gr=dpfGr,Index=Index, KnownEntries=KnownEntries,NRows=NRows,Dimension=Dimension,lower=lower,upper = upper,method="L-BFGS-B")
    NewMinVal <- NewOptimized$value
    
    while(NewMinVal < MinVal){
      
      MinVal <- NewMinVal
      Optimized <- NewOptimized
      
      NewOptimized <- optim(Optimized$par,fn=dpfFn,gr=dpfGr,Index=Index, KnownEntries=KnownEntries,NRows=NRows,Dimension=Dimension,lower=lower,upper = upper,method="L-BFGS-B")
      NewMinVal <- NewOptimized$value
      
      #Stopping Criteria
      if((MinVal-NewMinVal)/MinVal < toler | NewMinVal < toler){
        MinVal <- NewMinVal
        Optimized <- NewOptimized
        break
      }
    }
  }
  
  ### Put the Completed Matrix together, and decompose into a point scatter
  
  Completed <- buildMatrix(Optimized$par,KnownEntries, Index,NRows)
  D <- sqrt(Completed)
  
  return(list(D=D,optval=MinVal))
}