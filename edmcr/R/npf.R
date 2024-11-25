#'Nonparametric Position Formulation
#'
#'\code{npf} returns a completed Euclidean Distance Matrix D, with dimension d,
#' from a partial Euclidean Distance Matrix using the methods of Fang & O'Leary (2012) 
#'
#'@details
#'
#'This is an implementation of the Nonconvex Position Formulation (npf)
#' for Euclidean Distance Matrix Completion, as proposed in 'Euclidean 
#' Distance Matrix Completion Problems' (Fang & O'Leary, 2012).
#' 
#' The method seeks to minimize the following:
#' 
#' \deqn{||A \cdot (D - K(XX'))||_{F}^{2}}
#' 
#' where the function K() is that described in gram2edm, and the norm is Frobenius. Minimization is over X, the nxp matrix of node locations.
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
#'@param D An nxn partial-distance matrix to be completed. D must satisfy a list of conditions (see details), with unkown entries set to NA.
#'@param A a weight matrix, with \eqn{h_{ij} = 0} implying \eqn{a_{ij}} is unknown. Generally, if \eqn{a_{ij}} is known, \eqn{h_{ij} = 1}, although any non-negative weight is allowed.
#'@param d the dimension of the resulting completion
#'@param dmax the maximum dimension to consider during dimension relaxation
#'@param decreaseDim during dimension reduction, the number of dimensions to decrease each step
#'@param stretch should the distance matrix be multiplied by a scalar constant? If no, stretch = NULL, otherwise stretch is a positive scalar
#'@param method The method used for dimension reduction, one of "Linear" or "NLP".
#'@param toler convergence tolerance for the algorithm
#'
#'@return
#'\item{D}{an nxn matrix of the completed Euclidean distances}
#'\item{optval}{the minimum value achieved of the target function during minimization}
#'
#'@examples
#'
#'D <- matrix(c(0,3,4,3,4,3,
#'              3,0,1,NA,5,NA,
#'              4,1,0,5,NA,5,
#'              3,NA,5,0,1,NA,
#'              4,5,NA,1,0,5,
#'              3,NA,5,NA,5,0),byrow=TRUE, nrow=6)
#'              
#'A <- matrix(c(1,1,1,1,1,1,
#'              1,1,1,0,1,0,
#'              1,1,1,1,0,1,
#'              1,0,1,1,1,0,
#'              1,1,0,1,1,1,
#'              1,0,1,0,1,1),byrow=TRUE, nrow=6)
#'
#'edmc(D, method="npf", d=3, dmax=5)
#'
#'@seealso \code{\link{gram2edm}}
#'
#'@export
#'@import truncnorm
#'@import lbfgs
#'@importFrom nloptr cobyla

npf <- function(D,
                A=NA,
                d,
                dmax=(nrow(D)-1),
                decreaseDim=1,
                stretch = NULL,
                method = "Linear",
                toler=1e-8){
  
  ############ Input Checking #############
  
  #Checks for D
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
  
  #General Checks for A
  if(!any(is.na(A))){
    if(!is.numeric(A)){
      stop("A must be a numeric matrix")
    }else if(!is.matrix(A)){
      stop("A must be a numeric matrix")
    }else if(any(A < 0)){
      stop("All entries in A must be non-negative")
    }else if(any(rowSums(A) == 0)){
      stop("A must be a connected matrix")
    }
  }
  
  #General Checks for d
  if(!is.numeric(d)){
    stop("d must be a numeric integer")
  }else if(length(d) != 1){
    stop("d must be a numeric integer")
  }else if(!(d %% 2 == 1 | d %% 2 == 0)){
    stop("d must an integer")
  } 
  
  #General Checks for dmax
  if(!is.numeric(dmax)){
    stop("dmax must be a numeric integer")
  }else if(length(dmax) != 1){
    stop("dmax must be a numeric integer")
  }else if(!(dmax %% 2 == 1 | dmax %% 2 == 0)){
    stop("dmax must an integer")
  } 
  
  #General Checks for Stretch
  if(is.null(stretch)){
    S <- 1
  }else if(length(stretch) == 1){
    S <- stretch
  }else if(length(stretch) == 2){
    S <- runif(stretch[1],stretch[2])
  }else if(stretch == "Unif"){
    S <- runif(1,1.25,2.5)
  }else{
    stop("Invalid stretch argument")
  }
  
  #General checks for toler
  if(!is.numeric(toler)){
    stop("The completion tolerance, toler, must be a numeric object")
  }else if(toler <= 0){
    stop("The completion tolerance, toler, must be greater than 0")
  }
  
  ############## FUNCTION START ###################
  
  #Initialization
  
  Unknown <- which(is.na(D))
  
  D[Unknown] <- 0
  D <- D * D
  
  if(is.na(A[1])){
    A <- matrix(rep(1,nrow(D)^2),nrow=nrow(D))
    A[Unknown] <- 0
  }
  
  #Create Graph Object and shortest path object for use later
  
  GraphA <- graph.adjacency(sqrt(D),mode="undirected",weighted=TRUE)
  ShortestPaths <- shortest.paths(GraphA)
  
  #Get pre=EDM F by solving all-pairs shortest path problem for D
  
  FInit <- npfCreateF(D, GraphA, ShortestPaths)
  
  POptim <- npfDimRelax(D, A, FInit,d,dmax,decreaseDim=1, S, toler, Method=method)
  
  if(is.null(POptim)){ #If Dim Relax was unsuccessful, do a local minimization
    P <- npfConfigInit(FInit,d)
    P <- P*S
    PVector <- as.vector(P)
    POptim <- lbfgs(npfLocalMin,npfGr,vars= PVector,A=D,H=A,invisible=1)
    PSolVect <- as.vector(POptim$par)
    PSol <- matrix(PSolVect,nrow=nrow(A), byrow=FALSE)
    fPSol <- npffAH(D,A,PSol)
  }else{
    PSolVect <- as.vector(POptim)
    PSol <- matrix(PSolVect,nrow=nrow(D), byrow=FALSE)

    fPSol <- npffAH(D,A,PSol)
  }
  
  while(fPSol > toler){
    
    fPSolOld <- fPSol
    
    fAHP <- fPSol
    FHat <- FInit
    
    if(is.null(stretch)){
      S <- 1
    }else if(length(stretch) == 1){
      S <- stretch
    }else if(length(stretch) == 2){
      S <- runif(stretch[1],stretch[2])
    }else if(stretch == "Unif"){
      S <- runif(1,1.25,2.5)
    }
    
    for(i in 1:100){
      FHatTemp <- npfCreateFHat(D,GraphA, ShortestPaths)
      PTemp <- npfConfigInit(FHatTemp,d)    
      
      fAHPTemp <- npffAH(D,A,PTemp)
      
      if(fAHPTemp < fAHP){
        P <- PTemp
        fAHP <- fAHPTemp
        FHat <- FHatTemp
      }
    }
    
    POptim <- npfDimRelax(D, A, FHat,d,dmax,decreaseDim=1,S,toler, Method=method)
    if(is.null(POptim)){ #If DimRelax is unsuccessful, do a local minimization
      P <- npfConfigInit(FHat,d)
      P <- P*S
      PVector <- as.vector(P)
      POptim <- lbfgs(npfLocalMin,npfGr,vars= PVector,A=D,H=A,invisible=1)
    }
    
    PSolVect <- as.vector(POptim$par)
    PSol <- matrix(PSolVect,nrow=nrow(D), byrow=FALSE)
    fPSol <- npffAH(D,A,PSol) 
    
    if((fPSol-fPSolOld)/fPSolOld < sqrt(toler) | fPSol < toler){
      break
    }
    
  }
  
  DSol <- sqrt(gram2edm(PSol %*% t(PSol)))
  return(list(D=DSol,optval=fPSol, PSol))
}
