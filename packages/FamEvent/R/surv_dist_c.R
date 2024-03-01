surv_dist_c <- function(t, base.dist, parms, xbeta, alpha, res, cuts=NULL){
  H1 <- cumhaz(dist=base.dist[1], t, parms[[1]], cuts=cuts)*exp(xbeta[1]+alpha[1])
  H2 <- cumhaz(dist=base.dist[2], t, parms[[2]], cuts=cuts)*exp(xbeta[2]+alpha[2])
  exp(-H1-H2)-res
} 
  	