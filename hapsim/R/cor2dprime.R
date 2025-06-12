"cor2dprime" <-
function(mat, probs){
  nloci <- ncol(mat)
  ldmat <- matrix(0, ncol=nloci, nrow=nloci)
  for (i in 1:(nloci-1)){
    for (j in i:nloci){
      p1 <- probs[i]
      q1 <- 1-p1
      p2 <- probs[j]
      q2 <- 1-p2
      D <- mat[i,j]*sqrt(p1*p2*q1*q2)
      if (D<0) D <- D/min(p1*q2,q1*p2) else D <- D/max(-p1*p2,-q1*q2)
      ldmat[i,j] <- D
      ldmat[j,i] <- ldmat[i,j]
    }  
  }
  return(ldmat)
}

