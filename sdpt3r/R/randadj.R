randomadj <- function(n, prob){
  B <- matrix(0,n,n)
  for(i in 2:n){
    for(j in 1:(i-1)){
    r <- runif(1,0,1)
    if(r < prob){
      B[i,j] <- 1
      B[j,i] <- 1
    }
    }
  }
}