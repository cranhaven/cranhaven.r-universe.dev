augment.X_rcpp = function(pobs_vec, Vobs, Vobs_L, Vobs_R, cur.par.X, dist.X, C, collapsed.g){
  pobs_norm = pnorm_rcpp(pobs_vec,
                         Vobs,
                         Vobs_L,
                         Vobs_R,
                         cur.par.X,
                         dist.X,
                         collapsed.g)

  k   = sample_k_rcpp(pobs_norm$pobs_norm)
  phi = lookUpMat_rcpp(Vobs, k)

  x.update = rep(NA, length(C))
  x.update[C == 0] = r.trdist(cur.par.X[C==0,,drop=F], a = phi[C==0,1], b = phi[C==0,2], dist.X)
  x.update[C == 1] = rdist(n = nrow(cur.par.X[C==1,, drop=F]), par = cur.par.X[C==1,, drop=F], dist = dist.X)

  ret = list()
  ret$X  = x.update
  ret$k = k
  ret$phi = phi
  if(collapsed.g) ret$sums = pobs_norm$sums_
  ret
}
