integ_frailty <- function(u, event, base.dist, base.est, frailty.dist, xbeta, cuts, kappa) {
  H1 <- cumhaz(base.dist[1], u, base.est[[1]], cuts=cuts)*exp(xbeta[1]) 
  H2 <- cumhaz(base.dist[2], u, base.est[[2]], cuts=cuts)*exp(xbeta[2]) 
  He <- (event==1)*H1 + (event==2)*H2
  Hc <- (event==1)*H2 + (event==2)*H1
  cevent <- ifelse(event == 1, 2, 1)
  p1 <- dlaplace(dist=frailty.dist, g=He, d=1, k=kappa[event])
  p2 <- laplace(dist=frailty.dist, g=Hc, k=kappa[cevent])
  haz1 <- hazards(u, dist=base.dist[1], parms=base.est[[1]], cuts=cuts) * exp(xbeta[1])
  haz2 <- hazards(u, dist=base.dist[2], parms=base.est[[2]], cuts=cuts) * exp(xbeta[2])
  h <- (event == 1)* haz1 + (event == 2)*haz2
  integ_pen <- h * p1*p2 
  return(integ_pen)
}
