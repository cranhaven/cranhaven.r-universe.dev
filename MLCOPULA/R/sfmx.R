sfmx <- function(x){
  f <- exp(x - max(x)) / (sum (exp(x - max(x))) )
  return(f)
}
