r.med <-
function(x, weight, k){
  n <- length(x)
  if (is.null(weight)) weight <- rep(1, n)
  
    rhow <- k*weighted.median(x, weight)
    gap <- sum(sapply(1:n, function(i){
      max(x[i] - rhow, 0)*weight[i]}))/sum(weight)
  return(gap)
}
