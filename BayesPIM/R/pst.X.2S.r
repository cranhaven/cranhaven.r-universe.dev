pst.X.2S  = function(par, dist, S, d, L, R){
  q = rep(NA, length(d))
  q[d==1] = r.trdist(par[d==1,, drop=F], a = L[d==1], b = R[d==1], dist)
  q[d==2] = r.trdist(par[d==2,, drop=F], a = L[d==2], b = R[d==2], dist)
  return(q)
}

