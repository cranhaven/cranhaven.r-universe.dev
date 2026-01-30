P_vobs = function(Vobs, kappa){
  m = sapply(Vobs, length)-1
  cens =  sapply(Vobs, function(x) is.infinite(x[length(x)]))
  m.max = max(m)
  p.vec = geom(1:m.max, kappa)
  p.vec.inf = geom.inf(1:m.max, kappa)
  P = sapply( 1: length(m), function(x) if(!cens[x]) rev(p.vec[1:m[x]])
              else         rev(p.vec.inf[1:m[x]])
  )
  P
}
