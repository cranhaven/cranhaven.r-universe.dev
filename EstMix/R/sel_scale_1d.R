#' select scale for 1 normal + 1 tumor case
#' @keywords internal
#' @export
sel_scale_1d = function(cnMax=6,pscnMax=6,ngrid = 100,nslaves = 50, B_new, IT_new, temp){
  ##load(fn);
  tt = exp(-(0:6-1)^2);prior_f = tt%*%t(tt);lp_2d = -log(prior_f)*1e-3
  rtt = exp(-(6:0-1)^2);rprior_f = rtt%*%t(rtt);rlp_2d = -log(rprior_f)*1e-3
  var_baf <- temp$var_baf
  var_tcn <- temp$var_tcn
  scale_max <- temp$scale_max
  scale_min <- temp$scale_min

  n_seg = nrow(IT_new)

  sim_func <- function(ss,scale_min=.5,scale_max=2,ngrid=100,pscnMax=6){
    scale = scale_min+ss*(scale_max-scale_min)/ngrid
    ## load(fn)
    n_seg = nrow(IT_new)
    L_tot = matrix(0,100,n_seg)
    for(i in 1:n_seg){
      L_tot[,i]=calcll_p1_cpp(as.numeric(IT_new[i,]),as.numeric(B_new[i,]),lp_2d,rlp_2d,var_baf,var_tcn,scale,pscnMax,cnMax)[[1]]
    }
    L_sum = apply(L_tot,1,sum)
    out <-min(L_sum)
    return(out)
  }

  ll <- rep(0, 100)
  for (i in 1:100){
    ll[i] <- sim_func(i, scale_min=scale_min,scale_max=scale_max)
  }

  ### find the local minimum
  ind = 3:(ngrid-2)
  id = which(ll[ind]<ll[ind+1]&ll[ind]<ll[ind+2]&ll[ind]<ll[ind-1]&ll[ind]<ll[ind-2])+2
  if(length(id)>1){id1 = which.min(ll[id]);id2 = which.min(ll[id[-id1]]);id2 = (id[-id1])[id2];id1 = id[id1];id=c(id1,id2)}
  scale1d = id*(scale_max-scale_min)/ngrid+scale_min
  res = list()
  res$ll = ll
  res$scale_max = scale_max
  res$scale_min = scale_min
  res$scale1d = scale1d
  return(res)
}
