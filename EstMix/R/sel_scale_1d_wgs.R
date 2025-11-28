#' select scale for 1 normal + 1 tumor case for wgs data
#' @keywords internal
#' @export
sel_scale_1d_wgs = function(cnMax=6,pscnMax=6,ngrid = 100,nslaves = 50,temp){
  ##load(fn)
  tt = exp(-(0:6-1)^2);prior_f = tt%*%t(tt);lprior_f_2d = -log(prior_f)*1e-3
  rtt = exp(-(6:0-1)^2);rprior_f = rtt%*%t(rtt);rlprior_f_2d = -log(rprior_f)*1e-3
  baf <- temp$baf
  lrr <- temp$lrr
  n_baf <- temp$n_baf
  nrc <- temp$nrc

  scale_max = mean(exp(lrr))*2
  scale_min = mean(exp(lrr))/3

  sim_func <- function(ss,ngrid=100,pscnMax=6){
    ##load(fn)
    scale = scale_min+ss*(scale_max-scale_min)/ngrid
    n_seg = length(baf)
    L_tot_1d = matrix(0,100,n_seg)
    for(i in which(!is.na(baf))){
      L_tot_1d[,i]=calcll_1d_baf(lrr[i],nrc[i],baf[i],n_baf[i],lprior_f_2d*0,rlprior_f_2d*0,scale,6,6)[[1]]
    }
    L_sum_1d = apply(L_tot_1d,1,sum)
    out = min(L_sum_1d)
    return(out)
  }

  ll <- lapply(1:100, sim_func)
  ll <-unlist(ll)

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
