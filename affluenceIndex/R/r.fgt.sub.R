r.fgt.sub <- function(x.sub, x, weight.sub, weight, k, alpha){
  n <- length(x)
  if(is.null(weight)){
    weight <- rep(1, n)
    weight.sub <- rep(1, length(x.sub))
  }
  rhow <- k*weighted.median(x, weight)
  ind <- ifelse(x.sub > rhow, 1, 0)
  r.fgt <- sum(((((x.sub - rhow)/rhow)*ind)^alpha*weight.sub))/sum(weight.sub)
  return(r.fgt)
}
