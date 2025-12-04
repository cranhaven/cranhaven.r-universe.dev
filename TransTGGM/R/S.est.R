S.est = function(data, Omega.hat.list){
  # S.est: the function calculating the covariance matrix of each mode

  p.vec = dim(data)
  M = length(p.vec) - 1
  n.da = p.vec[M+1]
  p.vec = p.vec[-(M+1)]

  Omega.hat.list.sqrt = list()
  S.hat.list0 = list()
  for (m in 1:M) {
    Omega.hat.list.sqrt[[m]] = expm::sqrtm(Omega.hat.list[[m]])
    S.hat.list0[[m]] = solve(Omega.hat.list[[m]])
  }

  S.hat.list1 = list()
  for(m in 1:M){
    S.array = array(0,c(p.vec[m],p.vec[m],n.da))
    Omega.hat.list.sqrt.m = Omega.hat.list.sqrt
    Omega.hat.list.sqrt.m[[m]] = diag(p.vec[m])
    for(i in 1:n.da){
      d=0
      eval(parse(text=paste('d=data[',paste(rep(',',M),collapse=''),'i]')))
      Vi = k_unfold( as.tensor(ttl( as.tensor(d) , Omega.hat.list.sqrt.m , ms=1:M)@data) ,m=m)@data
      S.array[,,i] = Vi %*% t(Vi)
    }
    S.mat = apply(S.array,c(1,2),mean) * p.vec[m] / prod(p.vec)
    S.hat.list1[[m]] = S.mat
  }
  S.hat.list = list(sig0=S.hat.list0, sig1=S.hat.list1)
  return(S.hat.list)
}
