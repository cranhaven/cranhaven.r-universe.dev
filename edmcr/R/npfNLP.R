npfNLP <- function(W,A,H,r){
  
  WVec <- as.vector(W)
  
  #Constraint
  hinfun <- function(WVec,A,H,r){
    W <- matrix(WVec,nrow=nrow(A),byrow=FALSE)
  
    Result <- npffAH(A,H,W)
    
    if(Result < 1e-8){
      Result <- 0
    }
    
    return(Result)
  }
  
  #Objective
  fn <- function(WVec,r,A,H){
    W <- matrix(WVec,nrow=nrow(A),byrow=FALSE)
    W1 <- W[,1:r]
    W2 <- W[,-c(1:r)]
    Result <- sum(W2^2)
    
    return(Result)
  }
  
  #Optimization
  
  S <- cobyla(W, fn, hin = hinfun, control = list(xtol_rel = 1e-8, maxeval = 2000),A=A,H=H,r=r)
  
  #Results
    
  Wvec <- S$par
  W <- matrix(WVec,nrow=nrow(A),byrow=FALSE)
  W1 <- W[,1:r]
  
  return(W1)
} 