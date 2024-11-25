randmat <- function(m,n,k,randtype){
  set.seed(k)
  if(randtype == "n"){
    v <- matrix(rnorm(m*n),nrow=m, ncol=n)
  }else{
    v <- matrix(runif(m*n), nrow=m, ncol=n)
  }
  return(v)
}