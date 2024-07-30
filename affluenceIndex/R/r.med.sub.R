r.med.sub <-
function(x.sub, x, weight.sub, weight, k){
  n <- length(x)
  if(is.null(weight)){
    weight <- rep(1, n)
    weight.sub <- rep(1, length(x.sub))
  }  
    rhow <- k*weighted.median(x, weight)
    gap <- sum(sapply(1:length(x.sub), function(i){
      max(x.sub[i] - rhow, 0)*weight.sub[i]}))/sum(weight.sub)
    return(gap)
}
