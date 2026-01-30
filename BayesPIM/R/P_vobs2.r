P_vobs2 = function(Vobs, kappa, r){
  m = sapply(Vobs, length)-1
  cens =  sapply(Vobs, function(x) is.infinite(x[length(x)]))
  p.vec = geom(m+r, kappa)
  p.vec.inf = geom.inf(m+r, kappa)
  ifelse(cens, p.vec.inf, p.vec)
}
