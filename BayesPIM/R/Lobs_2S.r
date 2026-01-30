Lobs_2S = function (est, mod, log.scale = T, sumup = T) 
{
  vanilla = mod$vanilla
  Vobs = mod$Vobs
  g.fixed = numeric(length(Vobs))
  for (i in 1:length(g.fixed)) g.fixed[i] = as.numeric(length(Vobs[[i]]) == 
                                                         1)
  for (i in 1:length(Vobs)) if (as.logical(g.fixed[i])) 
    Vobs[[i]] = c(0, Inf)
  dist.X = mod$dist.X
  Z1.X = cbind(1, mod$Z.X)
  p1.X = ncol(Z1.X)
  m = sapply(Vobs, length) - 1
  if (!vanilla) {
    Z1.W = cbind(1, mod$Z.W)
    p1.W = ncol(Z1.W)
    kappa = est[(length(est))]
    est.x = est[1:(p1.X + 1)]
    est.w = est[(p1.X + 2):(p1.X + p1.W + 1)]
  }
  if (vanilla) {
    kappa = est[(length(est))]
    est.x = est[1:(length(est) - 1)]
  }
  pobs_vec = unlist(P_vobs(Vobs, kappa = kappa))
  Vobs_L = unlist(lapply(Vobs, function(x) x[1:(length(x) - 
                                                  1)]))
  Vobs_R = unlist(lapply(Vobs, function(x) x[2:(length(x))]))
  if (dist.X != "lognormal") 
    cur.par.X = trans.par(Z1.X, par = est.x)
  if (dist.X == "lognormal") 
    cur.par.X = trans.par.ind.norm(Z1 = Z1.X, p = est.x[1:(length(est.x) - 
                                                             1)], v = est.x[length(est.x)])
  par = apply(cbind(cur.par.X, m), 1, function(x) matrix(rep(x[1:2], 
                                                             x[3]), nrow = x[3], ncol = 2, T))
  par = do.call("rbind", par)
  Fxl = pdist(Vobs_L, par = par, dist = dist.X)
  Fxr = pdist(Vobs_R, par = par, dist = dist.X)
  pobs_ = (Fxr - Fxl) * pobs_vec
  pobs_ = split(x = pobs_, f = rep(1:length(m), m))
  if (!vanilla) {
    mu_w = as.numeric(Z1.W %*% as.matrix(as.numeric(est.w)))
    theta = pnorm(mu_w)
    q0 = sapply(pobs_, sum) * (1 - theta) * (1 - g.fixed)
    q1 = P_vobs2(Vobs, kappa, r = mod$r) * (theta) * (1 - g.fixed)
    S = q0 + q1
    S[g.fixed == 1] = (kappa * theta)[g.fixed == 1]
  }
  if (vanilla) {
    S = sapply(pobs_, sum)
  }
  if (log.scale & sumup) 
    r = sum(log(S))
  if (!log.scale & !sumup) 
    r = S
  if (log.scale & !sumup) 
    r = log(S)
  r
}