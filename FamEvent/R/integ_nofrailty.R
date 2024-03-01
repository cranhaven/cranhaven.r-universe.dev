integ_nofrailty <- function(u, event, base.dist, base.est, xbeta, cuts, kappa) {
  H1 <- cumhaz(base.dist[1], u, base.est[[1]], cuts=cuts)*exp(xbeta[1]) 
  H2 <- cumhaz(base.dist[2], u, base.est[[2]], cuts=cuts)*exp(xbeta[2]) 
  cevent <- ifelse(event == 1, 2, 1)
  haz1 <- hazards(u, dist=base.dist[1], parms=base.est[[1]], cuts=cuts) * exp(xbeta[1])
  haz2 <- hazards(u, dist=base.dist[2], parms=base.est[[2]], cuts=cuts) * exp(xbeta[2])
  h <- (event == 1)* haz1 + (event == 2)*haz2
  integ_pen <- h * exp(-H1-H2)
  return(integ_pen)
}
