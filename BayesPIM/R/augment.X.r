augment.X = function(pobs_vec, Vobs, Vobs_L, Vobs_R, cur.par.X, dist.X, C, collapsed.g){
  m   = sapply(Vobs, length)-1
  par = apply( cbind(cur.par.X,m), 1, function(x) matrix( rep(x[1:2], x[3]) , nrow=x[3], ncol= 2, T) )
  par = do.call('rbind', par)
  
  Fxl = pdist(Vobs_L, par = par, dist = dist.X)
  Fxr = pdist(Vobs_R, par = par, dist = dist.X)
  pobs_ = (Fxr-Fxl) * pobs_vec
  
  pobs_ = split(x=pobs_, f=rep(1:length(m), m))
  
  if(!collapsed.g) {
    pobs_norm = lapply(pobs_, function(x) x/sum(x) )
  } else{
    sums_ <- vapply(pobs_, sum, FUN.VALUE = double(1))
    pobs_norm <- list()
    for(i in 1:length(Vobs)) pobs_norm[[i]] <- pobs_[[i]] / sums_[i]
  }
  
  k   = sapply(pobs_norm, function(x) sum( rmultinom(1,1,x) * 1:(length(x)) ))
  phi = look.up.mat(L = Vobs, a = k)
  
  x.update = rep(NA, length(C))
  x.update[C == 0] = r.trdist(cur.par.X[C==0,,drop=F], a = phi[C==0,1], b = phi[C==0,2], dist.X)
  x.update[C == 1] = rdist(n = nrow(cur.par.X[C==1,, drop=F]), par = cur.par.X[C==1,, drop=F], dist = dist.X)
  
  ret = list()
  ret$X  = x.update
  ret$k = k
  ret$phi = phi
  if(collapsed.g) ret$sums = sums_
  ret
}