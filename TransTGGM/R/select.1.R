select.1 = function(t.sigma.tilde.list, t.Omega.hat.list, Theta.hat.list, mode.set, symmetric=TRUE){
  p.vec = NULL
  for(j in 1:length(t.Omega.hat.list)){
    p.vec[j] = dim(t.Omega.hat.list[[j]])[1]
  }

  Omega.hat.final.list = list()
  Omega.hat.final.sym.list = list()
  W.list = list()
  for (m in mode.set) {
    mi = match(m, mode.set)
    pm = p.vec[m]
    t.sigma.tilde.m = t.sigma.tilde.list[[m]]
    Theta.trans.m = Theta.hat.list[[mi]]
    t.Omega.hat.m = t.Omega.hat.list[[m]]

    resid.Omega0 = apply((t.sigma.tilde.m %*% t.Omega.hat.m - diag(pm))^2, 2, sum)
    resid.trans = apply((t.sigma.tilde.m %*% Theta.trans.m - diag(pm))^2, 2, sum)
    w.m = apply(rbind(resid.Omega0, resid.trans),2,which.min)
    Omega.final.m = t( t(t.Omega.hat.m) * c(w.m == 1) + t(Theta.trans.m) * c(w.m == 2) )

    Omega.hat.final.list[[mi]] = Omega.final.m
    W.list[[mi]] = w.m # 1: only target; 2: transfer

    if(symmetric){
      Omega.hat.final.sym.list[[mi]] = symmetric.mat(Omega.final.m)}
  }
  selec = list(Omega.hat.final.list = Omega.hat.final.list,
               Omega.hat.final.sym.list = Omega.hat.final.sym.list,
               W.list = W.list)
  return(selec)

}
