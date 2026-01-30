trans.par = function(Z1, par){
  p = length(par)
  p1 = exp(Z1 %*% as.matrix(par[1:(p-1)]))
  p2 = 1/exp(par[(p)])
  cbind(p1,p2)
}
 

