r.is.sub <-
function(x.sub, x, weight.sub, weight, p){
  n <- length(x)
  if(is.null(weight)){
    weight <- rep(1, n)
    weight.sub <- rep(1, length(x.sub))
  }
  q.p <-rep(weighted.quantile(x, weight, c(p)), length(x.sub))
    ind <- ifelse(x.sub > q.p, 1, 0)
    r.2 <-sum(x.sub*weight.sub*ind)/sum(x.sub*weight.sub)
  return(r.2)
}
