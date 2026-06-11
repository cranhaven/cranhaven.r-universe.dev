wt.mean <-
function(x,wt){
  s = which(is.finite(x * wt))
  wt = wt[s]
  x = x[s]
  return(sum(wt * x)/sum(wt))
}
