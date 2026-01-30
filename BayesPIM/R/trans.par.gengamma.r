trans.par.gengamma = function(Z1, par){
  p = length(par)
  p1 = exp(-Z1 %*% as.matrix(par[1:(p-2)]))
  p2 = 1/exp(par[(p-1)])
  p3 = exp(par[p])
  cbind(p1,p2,p3)
}