#'Semi-Definite Programming Algorithm
#'
#'\code{sdp} returns a completed Euclidean Distance Matrix D, with dimension d,
#' from a partial Euclidean Distance Matrix using the methods of Alfakih et. al. (1999) 
#'
#'@details
#'
#'This is an implementation of the Semi-Definite Programming Algorithm (sdp)
#' for Euclidean Distance Matrix Completion, as proposed in 'Solving Euclidean 
#' Distance Matrix Completion Problems via Semidefinite Programming' (Alfakih et. al., 1999).
#' 
#' The method seeks to minimize the following:
#' 
#' \deqn{||A \cdot (D - psd2edm(S))||_{F}^{2}}
#' 
#' where the function psd2edm() is that described in psd2edm(), and the norm is Frobenius. Minimization is over S, a positive semidefinite matrix.
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
#'@param toler convergence tolerance for the algorithm
#'
#'@return
#'\item{D}{an nxn matrix of the completed Euclidean distances}
#'\item{optval}{the minimum value achieved of the target function during minimization}
#'
#'@examples
#'
#'\donttest{
#'D <- matrix(c(0,3,4,3,4,3,
#'              3,0,1,NA,5,NA,
#'              4,1,0,5,NA,5,
#'              3,NA,5,0,1,NA,
#'              4,5,NA,1,0,5,
#'              3,NA,5,NA,5,0), byrow=TRUE, nrow=6)
#'A <- matrix(c(1,1,1,1,1,1,
#'              1,1,1,0,1,0,
#'              1,1,1,1,0,1,
#'              1,0,1,1,1,0,
#'              1,1,0,1,1,1,
#'              1,0,1,0,1,1), byrow=TRUE, nrow=6)
#'              
#'edmc(D, method="sdp", A=A, toler=1e-2)
#'}
#'
#'@seealso \code{\link{psd2edm}}
#'
#'@export
#'@import Matrix
sdp <- function(D,A,toler=1e-8){
  
  ########## Input Checking #############
  
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
  
  if(!is.numeric(toler)){
    stop("The completion tolerance, toler, must be a numeric object")
  }else if(toler <= 0){
    stop("The completion tolerance, toler, must be greater than 0")
  }
  
  ###############################################
  ############ MAIN FUNCTION START ##############
  ###############################################
  
  #Function Definition
  
  BaseNorm <- base::norm
  
  #Parameter Initialization
  
  sigmaa <- 1      #centering parameter
  alpha <- 1.5     #Initial stepsize
  
  #Initialization
  D[is.na(D)] <- 0
  
  A <- Matrix(A, sparse=TRUE)
  A <- A/BaseNorm(A, type="2")

  #If A has any zero rows or comlumns, remove them (and the corresponding rows from D)
  
  IndRows <- rowSums(A)
  IndCols <- colSums(A)
  
  if(any(IndRows == 0)){
    A <- A[-which(IndRows == 0),]
    D <- D[-which(IndRows == 0),]
  }
  
  if(any(IndCols == 0)){
    A <- A[,-which(IndCols == 0)]
    D <- D[,-which(IndCols == 0)]
  }
  
  #continue initialization
  
  D[which(A == 0)] <- 0   #set free elements to 0
  D <- Matrix(D, sparse=TRUE)
  n <- nrow(D)
  n1 <- n-1
  n2 <- n1^2
  tn1 <- n * (n-1)/2
  on1 <- matrix(1,n1,n1)
  indn1 <- which(upper.tri(on1,diag=TRUE))   #upper triangular indices
  
  ###################################################
  
  #construct orthogonal V s.t. V'e = 0
  
  y <- -1/sqrt(n)
  x <- -1/(n + sqrt(n))
  
  Y <- y * rep(1,n-1)
  X <- x * matrix(1,n-1,n-1) + diag(n-1)
  
  V <- as.matrix(rbind(Y,X)) # defunct as.matrix(rBind(Y,X))
  V <- Matrix(V, sparse=TRUE)
  
  B <- -.5*(t(V) %*% D %*% V)
  B <- Matrix(B, sparse=TRUE)
  
  #Initial Estimates
  
  X <- Matrix(B + 1.2 * sdpNormestfro(B) * diag(n1), sparse=TRUE)
  #X <- Matrix(B + 1.2 * norm(B,"F") * diag(n1), sparse=TRUE)
  Lam <- Matrix(2 * KWs(KW(X-B,V,A),V,A), sparse=TRUE)
  
  #Check that Lam is positive Definite
  
  LamEigs <- eigen(Lam)$values
  if(any(LamEigs <= 0)){
    Lam <- Lam + 1.2 * BaseNorm(Lam)
  }
  
  XEigs <- eigen(X)$values
  if(any(XEigs <= 0)){
    X <- X + 1.2 * BaseNorm(X)
  }
  
  ###
  
  gap <- sum(diag(Lam %*% X))    #duality gap
  muu <- gap/(2*n1)              #conservative decrease for first iteration
  
  normHA <- sdpNormestfro(A*D)
  
  Fd <- Matrix(2 * KWs(KW((X-B),V,A),V,A) - Lam, sparse=TRUE)
  Fc <- Matrix(sdpFmerit(muu,Lam,X,B,V,A,sigmaa), sparse=TRUE)
  optval <- norm(A * (D-psd2edm(X,V)), type="F")^2
  
  #Construct upper fixed part of operator for lss system F'muu*ds=-r
  
  Result <- sdpFpmuup(V,A,n1)
  
  Ku <- Result[[1]]
  lowinds <- Result[[2]]
  
  Kop <- rbind(Ku, matrix(0,nrow=nrow(Ku),ncol=ncol(Ku))) 
         # defunct rBind(Ku, matrix(0,nrow=nrow(Ku),ncol=ncol(Ku)))
  
  noiter <- 0
  
  while(min((max((gap/(optval+1)), (norm(Fd,'F')/normHA))),optval ) > toler){
    noiter <- noiter + 1
    
    #Compute Least Squares Direction
    
    #Find the RHS of the linear system Fd, Fc calculated below during update
    
    rhs <- c(Fd[1:length(Fd)],Fc[1:length(Fc)])
    rhs <- Matrix(rhs, ncol=1, sparse=TRUE)
    if(any(is.na(rhs))){
      stop("NaN found in RHS")
    }
  
    Kd <- sdpFpmulow(Lam,X,lowinds,V,A)
    Kop <- Matrix(rbind(Kop[1:(n1^2),1:(2*tn1)], Kd), sparse=TRUE) # defunct rBind
    
    if(any(is.na(Kop))){
      stop("NaN found in Kop")
    }
    
    #Clean up environment
    rm(Kd)
    
    dir <- -sdpQLS(Kop,rhs)
    dir <- Matrix(dir,sparse=TRUE)
    
    if(any(is.na(dir))){
      stop("NaN found in dir")
    }
  
    #reshape separately
    
    dX <- matrix(0,nrow=n1,ncol=n1)
    dX[indn1] <- dir[1:tn1]
    dXX <- matrix(0,nrow=n1,ncol=n1)
    dXX[upper.tri(dXX)] <- dX[upper.tri(dX)]
    dX <- Matrix(dX + t(dXX), sparse=TRUE)
    dX <- .5 * Matrix((dX + t(dX)),sparse=TRUE)
    #
    dLam <- matrix(0,nrow=n1,ncol=n1)
    dLam[indn1] <- dir[(tn1+1):(2*tn1)]
    dLamm <- matrix(0,nrow=n1,ncol=n1)
    dLamm[upper.tri(dLam)] <- dLam[upper.tri(dLam)]
    dLam <- Matrix(dLam+t(dLamm), sparse=TRUE)
    dLam <- .5 * Matrix((dLam +t(dLam)), sparse=TRUE)  
    
    #Check with the normal operator equation for lss
    
    Result <- sdpFpmu(dLam,dX,Lam,X,V,A)
    tFd <- Result[[1]]
    tFc <- Result[[2]]
    
    Result2 <- sdpFpmus(tFd,tFc,Lam,X,V,A)
    tdX <- Result2[[1]]
    tdLam <- Result2[[2]]
    
    check1 <- rbind(tdX,tdLam)# defunct rBind
    
    Result3 <- sdpFpmus(Fd,Fc,Lam,X,V,A)
    trhs1 <- Result3[[2]]
    trhs2 <- Result3[[1]]
    
    check2 <- rbind(trhs2, trhs1)  # defunct rBind
    
    check <- check1 + check2
    #normcheck <- sdpnormest(check)/max(1,sdpnormest(check2))
    normcheck <- max(svd(check, 0, 0)$d)/(max(1,max(svd(check2,0,0)$d)))

    if(normcheck > 1e-10){
      Kop <- as.matrix(Kop)
      rhs <- as.matrix(rhs)
      
      ANew <- t(Kop) %*% Kop
      b <- t(Kop) %*% rhs
      dir <- -solve(ANew,b)
    }
    
    #reshape separately
    
    dX <- matrix(0,nrow=n1,ncol=n1)
    dX[indn1] <- dir[1:tn1]
    dXX <- matrix(0,nrow=n1,ncol=n1)
    dXX[upper.tri(dXX)] <- dX[upper.tri(dX)]
    dX <- dX + t(dXX)
    dX <- .5 * Matrix((dX + t(dX)), sparse=TRUE)
    #
    dLam <- matrix(0,nrow=n1,ncol=n1)
    dLam[indn1] <- dir[(tn1+1):(2*tn1)]
    dLamm <- matrix(0,nrow=n1,ncol=n1)
    dLamm[upper.tri(dLam)] <- dLam[upper.tri(dLam)]
    dLam <- Matrix(dLam+t(dLamm), sparse=TRUE)
    dLam <- .5 * Matrix((dLam +t(dLam)), sparse=TRUE)  
    
    #Check with the normal operator equation for lss
    
    Result <- sdpFpmu(dLam,dX,Lam,X,V,A)
    tFd <- Result[[1]]
    tFc <- Result[[2]]
    
    Result2 <- sdpFpmus(tFd,tFc,Lam,X,V,A)
    tdX <- Result2[[1]]
    tdLam <- Result2[[2]]
    
    check1 <- rbind(tdX,tdLam) # defunct rBind
    
    Result3 <- sdpFpmus(Fd,Fc,Lam,X,V,A)
    trhs1 <- Result3[[2]]
    trhs2 <- Result3[[1]]
    
    check2 <- rbind(trhs2, trhs1)# defunct rBind
    
    check <- check1 + check2
    #normcheck <- sdpnormest(check)/max(1,sdpnormest(check2))
    normcheck <- max(svd(check, 0, 0)$d)/(max(1,max(svd(check2,0,0)$d)))
    
    if(normcheck > 1e-10){
      warning("normcheck > 1e-10")
    }
    
    #Symmetrize to avoid complex eigs
    
    X <- .5 * Matrix((X + t(X)),sparse=TRUE)
    Lam <- .5 * Matrix((Lam + t(Lam)),sparse=TRUE)
    dX <- .5 * Matrix((dX + t(dX)),sparse=TRUE)
    dLam <- .5 * Matrix((dLam + t(dLam)),sparse=TRUE)
    
    mineig <- 1
    
    while(mineig > 0){
      Xs <- X + alpha * dX
      Lams <- Lam + alpha * dLam
      
      LamEigs <- eigen(Lams,symmetric=TRUE)$values
      
      #To avoid computational issues, round eigenvalues in [-1x10^-8,0] to 0
      
      LamEigs[which(LamEigs < 0 & LamEigs > -1e-8)] <- 0
      
      if(any(LamEigs < 0)){
        mineig <- 1
      }else{
        mineig <- 0
      }
      
      if(mineig == 0){
        XsEigs <- eigen(Xs,symmetric = TRUE)$values
        
        XsEigs[which(XsEigs < 0 & XsEigs > -1e-8)] <- 0
  
        if(any(XsEigs < 0)){
          mineig <- 1
        }else{
          mineig <- 0
        }
      }
      
      alpha <- .95*alpha
      
      if(alpha < 10^(-5)){
        alpha <- 0
        break
      }
    }
      
    Xs <- Matrix(X + alpha * dX, sparse=TRUE)
    Lams <- Matrix(Lam + alpha*dLam,sparse=TRUE)
    
    #update
    X <- Xs
    Lam <- Lams
    gap <- sum(diag(Lam %*% X))
    muu <- gap/(2*n1)
    Fd <- Matrix(2*KWs(KW((X-B),V,A),V,A)-Lam, sparse=TRUE)
    Fc <- Matrix(sdpFmerit(muu,Lam,X,B,V,A,sigmaa), sparse=TRUE)
    
    OptMat <- A*(D-psd2edm(X,V))
    optval <- norm(OptMat,type="F")^2
    
    #Update step size
    
    if(alpha < 0.001){
      muu <- 2.1*muu
      sigmaa <- 1
      alpha <- .9
    }else if(alpha < .1){
      muu <- 2*muu
      sigmaa <- 1
      alpha <- 1
    }else if(alpha < .25){
      muu <- 1.8*muu
      sigmaa <- 1
      alpha <- 1
    }else if(alpha < .95){
      muu <- 1.5*muu
      sigmaa <- 1-.2*min(alpha,1)
      alpha <- 1.1
    }else if(alpha < 1.15){
      muu <- .5*muu
      sigmaa <- 1-.3*min(alpha,1)
      alpha <- 1.5
    }else{
      muu <- .5 * muu
      sigmaa <- .9
      alpha <- 2
    } 
    
  }
  
  #verify that a distance matrix was found
  
  D <- psd2edm(X,V)
  
  colnames(D) <- c()
  rownames(D) <- c()
  D <- as.matrix(D)

  return(list(D=D,optval = optval))
  
}