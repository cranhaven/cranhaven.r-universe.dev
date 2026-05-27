LP.post.conv <-
function(theta.set, DS.GF.obj, y.0, n.0 = NULL, 
                         e.0 = NULL) {
  fam = DS.GF.obj$fam
  out <- list()
  lambda.i <- function(s.i, tau.2) {
    s.i^2/(s.i^2 + tau.2)
  }
  #switch(fam, Normal = {
  prior.type = "Normal"
  se.0 <- n.0
  post.mu.i <- lambda.i(se.0, DS.GF.obj$g.par[2]) * 
    DS.GF.obj$g.par[1] + (1 - lambda.i(se.0, DS.GF.obj$g.par[2])) * 
    y.0
  post.tau2.i <- (1 - lambda.i(se.0, DS.GF.obj$g.par[2])) * 
    se.0^2
  PEB.pos.den <- dnorm(theta.set, post.mu.i, sd = sqrt(post.tau2.i))
  if (sum(DS.GF.obj$LP.par^2) == 0) {
    post.fit <- data.frame(theta.vals = theta.set, 
                           parm.pos = PEB.pos.den)
  } else {
    unit.grid <- pnorm(theta.set, DS.GF.obj$g.par[1], 
                       sd = sqrt(DS.GF.obj$g.par[2]))
    wght.den <- BayesGOF::weight.fun.univ(unit.grid, DS.GF.obj$g.par[1], 
                                          DS.GF.obj$g.par[2], post.mu.i, post.tau2.i, 
                                          family = fam)
    if (DS.GF.obj$LP.type == "L2") {
      d.u <- 1 + BayesGOF::gLP.basis(unit.grid, c(1, 1), DS.GF.obj$m.val, 
                                     con.prior = "Beta") %*% DS.GF.obj$LP.par
    } else {
      d.u <- exp(cbind(1, BayesGOF::gLP.basis(unit.grid, c(1, 
                                                           1), DS.GF.obj$m.val, con.prior = "Beta")) %*% 
                   DS.GF.obj$LP.par)
    }
    denom <- Bolstad2::sintegral(unit.grid, d.u * wght.den)$int
    post.fit <- data.frame(theta.vals = theta.set, 
                           parm.pos = PEB.pos.den, ds.pos = PEB.pos.den * 
                             (d.u/denom))
  }
  return(post.fit)
  #})
}
