r.hc.sub <-
function(x.sub, x, weight.sub, weight, k){
  n <- length(x)
  if(is.null(weight)){
    weight <- rep(1, n)
    weight.sub <- rep(1, length(x.sub))
  }
  rhow <- k*weighted.median(x, weight)
  r.3 <- sum(weight.sub[x.sub > rhow])/sum(weight.sub)
return(list(count.rich = length(x.sub[x.sub > rhow]), r.hc = r.3))

}
