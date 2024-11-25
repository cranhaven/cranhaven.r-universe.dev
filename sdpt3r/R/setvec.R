setvec <- function(n,x,alpha){
  for(k in 1:n){
    x[k] <- alpha
  }
  return(x)
}