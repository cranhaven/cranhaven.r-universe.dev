r.hc <-
function(x, weight, k){
  n <- length(x)
  if (is.null(weight)) weight <- rep(1, n)
  
  rhow <-k*weighted.median(x, weight)
  r.3 <- sum(weight[x > rhow])/sum(weight)
  return(list(count.rich = length(x[x>rhow]), r.hc=r.3))
}
