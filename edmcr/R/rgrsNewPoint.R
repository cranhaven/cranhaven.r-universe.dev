# Given a base point Xn, propose a d-dimensional point
# with distance r away from the base point. Direction is
# determined by simulating (and standardizing) a MVN(0,1).
# Direction can be restricted in 2D space by spcification of
# the NULL parameter

rgrsNewPoint <- function(Xn,r,d, Theta=NULL){
  
  if(!is.null(Theta)){
    V <- runif(1,0,1)
    
    if(V > Theta[3]){
      theta <- runif(1,-1,1)
      xnew <- Xn[1] + r * cospi(theta)
      ynew <- Xn[2] + r * sinpi(theta)
      NewX <- c(xnew,ynew)
    }else{
      theta1 <- runif(1,Theta[1],Theta[2])
      theta2 <- runif(1,Theta[1]+pi, Theta[2]+pi)
      
      U <- runif(1,0,1)
      
      if(U < 0.5){
        theta <- theta1
      }else{
        theta <- theta2
      }
      xnew <- Xn[1] + r * cos(theta)
      ynew <- Xn[2] + r * sin(theta)
      NewX <- c(xnew,ynew)
    }
    
  }else{
    
    MVN <- mvrnorm(1,rep(0,d),diag(rep(1,d)))
    Norm <- sqrt(sum(MVN^2))
    NormedMVN <- MVN/Norm
    
    XnNorms <- r * NormedMVN
    
    NewX <- Xn + XnNorms
  }
  
  return(NewX)
  
}