emfa <-
function(dta, nbf, min.err = 1e-06, verbose = FALSE, svd.method=c("fast.svd","irlba")) {
  svd.method = match.arg(svd.method)
  m = ncol(dta)
  n = nrow(dta)
  mdta = t(rep(1, n)) %*% dta/n
  vdta = (t(rep(1, n)) %*% dta^2/n) - mdta^2
  sddta = sqrt(n/(n - 1)) * sqrt(vdta)
  cdta = dta - rep(1, n) %*% mdta
  if (nbf == 0) {
    B = NULL
    Psi = rep(1, m)
    Factors = NULL
  }
  if (nbf > 0) {
    if (svd.method=="fast.svd") svddta = fast.svd(cdta/sqrt(n - 1))
    if (svd.method=="irlba") svddta = irlba(cdta/sqrt(n - 1),nv=nbf)
    evalues = (svddta$d[1:nbf])^2
    evectors = svddta$v[, 1:nbf, drop = FALSE]
    B = evectors %*% diag(sqrt(evalues),nrow=nbf,ncol=nbf)
    Psi = as.vector(sddta^2 - (B^2 %*% rep(1, nbf))[, 1])
    Psi[Psi<=1e-16] = 1e-16
    crit = 1
    while (crit > min.err) {
      iS = ifa(Psi, B)
      xiSB = cdta %*% iS$iSB
      Cyz = t(cdta) %*% xiSB/(n - 1)
      Czz = t(iS$iSB) %*% Cyz + diag(nbf) - t(B)%*%iS$iSB
      Bnew = Cyz %*% solve(Czz)
      Psinew = as.vector(sddta^2 - (Bnew^2 %*% rep(1, nbf))[,1])
      Psinew[Psinew<=1e-16] = 1e-16
      crit = mean((Psi - Psinew)^2)
      B = Bnew
      Psi = Psinew
      if (verbose) print(paste("Convergence criterion: ",signif(crit,digits=ceiling(-log10(min.err))),sep=""))
    }
    sB = scale(t(B), center = FALSE, scale = sqrt(Psi))
    G = solve(diag(nbf) + sB %*% t(sB))
    sB = scale(t(B), center = FALSE, scale = Psi)
    Factors = cdta %*% t(sB) %*% t(G)
  }
  res = list(B = B, Psi = as.vector(Psi), Factors = Factors, Objective=crit)
  return(res)
}
