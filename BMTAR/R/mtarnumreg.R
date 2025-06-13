#==================================================================================================#
# Date: 14/04/2020
# Description: Function for estimate number of regimes when unknown. Default method is
# Metropolized Carlin and Chib, when NAIC is TRUE number of regimes is choose only with this result.
# Function:
#==================================================================================================#
mtarnumreg = function(ini_obj, level = 0.95, burn_m = NULL, niter_m = 1000,
                      iterprev = 500, chain_m = FALSE, list_m = FALSE, NAIC = FALSE,
                      ordersprev = list(maxpj = 2,maxqj = 0,maxdj = 0), parallel = FALSE){
  compiler::enableJIT(3)
  if (!is.logical(chain_m)) {stop('chain_m must be a logical object')}
    if (!is.logical(parallel)) {stop('paralell must be a logical object')}
  if (!is.logical(NAIC)) {stop('NAIC must be a logical object')}
  # Checking
  if (!inherits(ini_obj, 'regime_inipars')) {
    stop('ini_obj must be a regime_inipars object')
  }
  if (is.null(ini_obj$tsregime_obj$Zt)) {
    stop('Threshold process must be enter in ini_obj for evaluate l0_max number of regimes')}
  if (is.null(ini_obj$l0_min)) {
    message('l0_min NULL default 2 \n')
    l0_min = 2
  }else{
    l0_min = ini_obj$l0_min
  }
  if (is.null(ini_obj$l0_max)) {
    message('l0_max NULL default 3 \n')
    l0 = 3
  }else{
    l0 = ini_obj$l0_max
  }
  if (is.null(ini_obj$method)) {
    message('method NULL default KUO \n')
    method = 'KUO'
  }else{
    method = ini_obj$method
  }
  # data
  Yt = ini_obj$tsregime_obj$Yt
  Ut = cbind(ini_obj$tsregime_obj$Zt,ini_obj$tsregime_obj$Xt)
  k = ini_obj$tsregime_obj$k
  N = ini_obj$tsregime_obj$N
  nu = ini_obj$tsregime_obj$nu
  if (is.null(nu)) {nu = 0}
  maxpj = ifelse(is.null(ordersprev$maxpj),2,ordersprev$maxpj)
  maxqj = ifelse(is.null(ordersprev$maxqj),0,ordersprev$maxqj)
  maxdj = ifelse(is.null(ordersprev$maxdj),0,ordersprev$maxdj)
  # Code
  burn_m = ifelse(is.null(burn_m),round(0.1*niter_m),burn_m)
  Yt = t(Yt)
  Zt = Ut[,1]
  if (nu == 0) {
    Xt = matrix(0,ncol = N,nrow = 1)
  }else{
    Xt = t(ini_obj$tsregime_obj$Xt)
  }
  rini = ini_obj$init$r
  a = ifelse(is.null(rini$za),min(Zt),stats::quantile(Zt,probs = rini$za))
  b = ifelse(is.null(rini$zb),max(Zt),stats::quantile(Zt,probs = rini$zb))
  ### FUNCTIONS
  dmunif = function(r, a, b){
    l = length(r) + 1
    names(a) = names(b) = NULL
    volume = ((b - a)^{l - 1})/(factorial(l - 1))
    for (i in 1:{l - 1}) {
      if (r[i] >= a & r[i] <= b) {
        if (l <= 2) {prob = 1}
        if (l > 2) {
          prob = 1
          for (j in 1:{l - 2}) {
            if (r[j] < r[j + 1]) {prob = prob*1}else{prob = prob*0}
          }
        }
      }else{prob = 0}
    }
    rj = matrix(nrow = 2,ncol = l)
    rj[,1] = c(-Inf,r[1])
    rj[,l] = c(rev(r)[1],Inf)
    if (l > 2) {
      for (i2 in 2:{l - 1}) {rj[,i2] = c(r[i2 - 1],r[i2])}
    }
    Ind = c()
    for (j in 1:l) {
      for (w in 1:N) {
        if (Zt[w] > rj[1,j] & Zt[w] <= rj[2,j]) {
          Ind[w] = j
        }
      }
    }
    Nrg = c()
    for (lj in 1:l) {
      Nrg[lj] = length(Ind[Ind == lj])
    }
    if (sum(Nrg/sum(Nrg) > 0.2) == l) {prob = 1*prob}else{prob = 0*prob}
    return(prob/volume)
  }
  dmunif = compiler::cmpfun(dmunif)
  rdunif = function(m,l0,...){
    sec = l0_min:l0
    sec = sec[sec != m]
    if (length(sec) == 1) {
      muestra = sec
    }else{
      muestra = sample(x = sec, size = 1)
    }
    return(muestra)
  }
  rdunif = compiler::cmpfun(rdunif)
  ### Function to create lists
  lists = function(l, r, pjmax, qjmax, djmax, etam, ...){
    rj = matrix(nrow = 2,ncol = l)
    rj[,1] = c(-Inf,r[1])
    rj[,l] = c(rev(r)[1],Inf)
    if (l > 2) {for (j2 in 2:{l - 1}) {rj[,j2] = c(r[j2 - 1],r[j2])}}
    # indicator variable for the regime
    Ind = vector(mode = 'numeric',length = N)
    for (j in 1:l) {
      Ind[Zt > rj[1,j] & Zt <= rj[2,j]] = j
    }
    Nrg = vector(mode = 'numeric')
    listaXj = listaYj = list()
    length(listaXj) = length(listaYj) = l
    Inj_X = function(ti,Yt,Zt,Xt,p,q,d){
      yti = vector(mode = "numeric")
      for (w in 1:p) {yti = c(yti,Yt[,ti - w])}
      xti = vector(mode = "numeric")
      for (w in 1:q) {xti = c(xti,Xt[,ti - w])}
      zti = vector(mode = "numeric")
      for (w in 1:d) {zti = c(zti,Zt[ti - w])}
      if (q == 0 & d != 0) {
        wtj = c(1,yti,zti)
      }else if (d == 0 & q != 0) {
        wtj = c(1,yti,xti)
      }else if (d == 0 & q == 0) {
        wtj = c(1,yti)
      }else{
        wtj = c(1,yti,xti,zti)}
      return(wtj)
    }
    Inj_X = Vectorize(Inj_X,vectorize.args = "ti")
    for (lj in 1:l) {
      p = pjmax[lj]
      q = qjmax[lj]
      d = djmax[lj]
      maxj = max(p,q,d)
      Inj = which(Ind == lj)
      Inj = Inj[Inj > maxj]
      Nrg[lj] = length(Inj)
      Yj = matrix(Yt[,Inj],nrow = k,ncol = Nrg[lj])
      if (identical(Inj,integer(0))) {
        Xj = matrix(nrow = 0,ncol = etam[lj]*k)
      }else{
        Wj = sapply(Inj,Inj_X,Yt = Yt,Zt = Zt,Xt = Xt,p = p,q = q,d = d)
        Xj = t(Wj) %x% diag(k)[1,]
        if (k != 1) {for (s in 2:k) {Xj = cbind(Xj,t(Wj) %x% diag(k)[s,])}}
      }
      listaXj[[lj]] = Xj
      listaYj[[lj]] = Yj
    }
    return(list(Nrg = Nrg,listaX = listaXj,listaY = listaYj))
  }
  lists = compiler::cmpfun(lists)
  ### Likelihood function for Yt
  fycond = function(ir, listar, gamma, theta, sigma){
    acum = 0
    l = length(listar$listaY)
    Nrg = listar$Nrg
    for (lj in 1:l) {
      yj = c(listar$listaY[[lj]])
      Xj = listar$listaX[[lj]]
      acum = acum + t(yj - Xj %*% diag(gamma[[lj]][,ir]) %*% theta[[lj]][,ir]) %*%
        {diag(Nrg[lj]) %x% solve(sigma[[lj]][[ir]])} %*%
        (yj - Xj %*% diag(gamma[[lj]][,ir]) %*% theta[[lj]][,ir])
    }
    sigmareg = lapply(sigma,function(x){x[[ir]]})
    cte = prodB(Brobdingnag::as.brob(sapply(sigmareg,function(x){
      return(c(determinant(x,logarithm = FALSE)$modulus))}))^{-Nrg/2})
    val = cte*exp(-1/2*Brobdingnag::as.brob(acum))
    return(val)
  }
  fycond = compiler::cmpfun(fycond)
  ### Previous iterations
  fill = function(m, iter = 500, burn = 1000, ...){
    i = 1
    ordersm = list(pj = rep(maxpj,m),qj = rep(maxqj,m),dj = rep(maxdj,m))
    etam = 1 + ordersm$pj*k + ordersm$qj*nu + ordersm$dj
    ini_obj_m = mtarinipars(tsregime_obj = ini_obj$tsregime_obj,
                            list_model = list(pars = list('l' = m),orders = ordersm),method = method)
    #Parameters priori
    theta0Pm = lapply(ini_obj_m$init$Theta,function(x){x$theta0j})
    sigma0Pm = lapply(ini_obj_m$init$Theta,function(x){x$cov0j})
    S0Pm = lapply(ini_obj_m$init$Sigma,function(x){x$S0j})
    nu0Pm = lapply(ini_obj_m$init$Sigma,function(x){x$nu0j})
    pij0Pm = ini_obj_m$init$Gamma
    # previous iterations
    par = mtarstr(ini_obj = ini_obj_m, niter = iter, chain = TRUE,burn = burn)
    #Parameters pseudo Theta
    theta0Sm = lapply(par$estimates$Theta,function(x){x[,2]})
    sigma0Sm = lapply(par$Chain$Theta,function(x){stats::cov(t(x))})
    #Parameters pseudo Sigma
    S0Sm = lapply(par$regime,function(x){x$sigma})
    nu0Sm = lapply(1:m,function(x){1000})
    #Parameters pseudo Gamma
    pij0Sm = lapply(par$Chain$Gamma,function(x){apply(x,1,mean)})
    #Parameters pseudo R
    if (m != 1) {
      rmean0Sm = par$estimates$r[,2]
      rcov0Sm = stats::cov(t(par$Chain$r))
    }else{
      rmean0Sm = rcov0Sm = NULL
    }
    # cadenas y primeros valores
    theta_iter = sigma_iter = gam_iter = Dj = Rj = vector('list',m)
    if (method == 'SSVS') {
      tauij = itauij = cij = vector('list',m)
    }
    if (m != 1) {
      r_iter = matrix(ncol = niter_m + burn_m,nrow = m - 1)
      r_iter[,1] = c(stats::quantile(Zt, probs = 1/m*(1:{m - 1})))
      r0iter = r_iter[,1]
    }else{
      r_iter = NULL
      r0iter = 0
    }
    for (lj in 1:m) {
      theta_iter[[lj]] = gam_iter[[lj]] = matrix(ncol = niter_m + burn_m,nrow = k*etam[lj])
      sigma_iter[[lj]] = vector('list',length = niter_m + burn_m)
      gam_iter[[lj]][,1] = rep(1,k*etam[lj])
      if (method == 'KUO') {
        theta_iter[[lj]][,1] = mvtnorm::rmvnorm(1,mean = theta0Pm[[lj]],sigma = sigma0Pm[[lj]])
      }
      if (method == 'SSVS') {
        cij[[lj]] = ini_obj_m$init$Theta[[paste0('R',lj)]]$Cij
        tauij[[lj]] = ini_obj_m$init$Theta[[paste0('R',lj)]]$Tauij
        itauij[[lj]] = cij[[lj]]*tauij[[lj]]
        Dj[[lj]] = diag(itauij[[lj]])
        Rj[[lj]] = diag(k*etam[lj])
        theta_iter[[lj]][,1] = mvtnorm::rmvnorm(1,mean = rep(0,k*etam[lj]),sigma = Dj[[lj]] %*% Rj[[lj]] %*% Dj[[lj]])
      }
      sigma_iter[[lj]][[1]] = MCMCpack::riwish(v = nu0Pm[[lj]],S = S0Pm[[lj]])
    }
    # LISTS
    if (method == 'SSVS') {
      iniP = list(Theta = list(mean = theta0Pm, cov = sigma0Pm), Sigma = list(cov = S0Pm,gl = nu0Pm),
                  Gamma = list(prob = pij0Pm, itauij = itauij, tauij = tauij, cij = cij, Rj = Rj))
    }else{
      iniP = list(Theta = list(mean = theta0Pm, cov = sigma0Pm), Sigma = list(cov = S0Pm,gl = nu0Pm),
                  Gamma = list(prob = pij0Pm))
    }
    iniS = list(Theta = list(mean = theta0Sm,cov = sigma0Sm), Sigma = list(cov = S0Sm,gl = nu0Sm),
                Gamma = list(prob = pij0Sm), r = list(mean = rmean0Sm, cov = rcov0Sm))
    listchain = list(Theta = theta_iter, Sigma = sigma_iter,
                     Gamma = gam_iter, r = r_iter)
    listr = lists(l = m,r = r0iter,pjmax = ordersm$pj,qjmax = ordersm$qj,djmax = ordersm$dj, etam)
    return(list(i = i,orders = ordersm,Priori = iniP,Pseudo = iniS,Chain = listchain,listr = listr,par = par))
  }
  fill = compiler::cmpfun(fill)
  ### Function to compute posterioris
  updatelist = function(l, ...){
    rgamber = function(pos, reg, ig, ...){
      gam_j = gam_iter
      gam_j[[reg]][pos,ig] = 1
      pycond1 = fycond(ig,listaj,gam_j,theta_iter,sigma_iter)
      gam_j[[reg]][pos,ig] = 0
      pycond0 = fycond(ig,listaj,gam_j,theta_iter,sigma_iter)
      if (method == 'KUO') {
        aij = pycond1*pij[[reg]][pos]
        bij = pycond0*(1 - pij[[reg]][pos])
      }else if (method == 'SSVS') {
        Xj = listaj$listaX[[reg]]
        yj = c(listaj$listaY[[reg]])
        gam_j[[reg]][pos,ig] = 1
        itauij[[reg]][gam_j[[reg]][,ig] == 0] = tauij[[reg]][gam_j[[reg]][,ig] == 0]
        itauij[[reg]][gam_j[[reg]][,ig] == 1] =
          cij[[reg]][gam_j[[reg]][,ig] == 1]*tauij[[reg]][gam_j[[reg]][,ig] == 1]
        Dj[[reg]] = diag(itauij[[reg]])
        pthetacond1 = dmnormB(x = theta_iter[[reg]][,ig],mean = rep(0,k*etam[reg]),sigma = Dj[[reg]] %*% Rj[[reg]] %*% Dj[[reg]])
        aij = pycond1*pthetacond1*pij[[reg]][pos]
        gam_j[[reg]][pos,ig] = 0
        itauij[[reg]][gam_j[[reg]][,ig] == 0] = tauij[[reg]][gam_j[[reg]][,ig] == 0]
        itauij[[reg]][gam_j[[reg]][,ig] == 1] =
          cij[[reg]][gam_j[[reg]][,ig] == 1]*tauij[[reg]][gam_j[[reg]][,ig] == 1]
        Dj[[reg]] = diag(itauij[[reg]])
        pthetacond0 = dmnormB(x = theta_iter[[reg]][,ig],mean = rep(0,k*etam[reg]),sigma = Dj[[reg]] %*% Rj[[reg]] %*% Dj[[reg]])
        bij = pycond0*pthetacond0*(1 - pij[[reg]][pos])
      }
      return(stats::rbinom(1,size = 1,prob = as.numeric((aij)/(aij + bij))))
    }
    rgamber = Vectorize(rgamber,"pos")
    listPr = listm[[paste0('m',l)]]
    Dj = vector('list')
    Rj = vector('list')
    i2 = listPr$i
    #creando temporales
    pjmax = listPr$orders$pj
    qjmax = listPr$orders$qj
    djmax = listPr$orders$dj
    etam = 1 + pjmax*k + qjmax*nu + djmax
    theta_iter = listPr$Chain$Theta
    sigma_iter = listPr$Chain$Sigma
    gam_iter = listPr$Chain$Gamma
    r_iter = listPr$Chain$r
    theta0j = listPr$Priori$Theta$mean
    sigma0j = listPr$Priori$Theta$cov
    S0j = listPr$Priori$Sigma$cov
    nu0j = listPr$Priori$Sigma$gl
    pij = listPr$Priori$Gamma$prob
    if (method == 'SSVS') {
      itauij = listPr$Priori$Gamma$itauij
      tauij = listPr$Priori$Gamma$tauij
      cij = listPr$Priori$Gamma$cij
      Rj = listPr$Priori$Gamma$Rj
    }
    #iterations update
    if (!is.null(r_iter)) {
      listaj = lists(l,r_iter[,i2],pjmax,qjmax,djmax,etam)
    }else{
      listaj = lists(l,0,pjmax,qjmax,djmax,etam)
    }
    for (lj in 1:l) {
      Xj = listaj$listaX[[lj]]
      Yj = listaj$listaY[[lj]]
      yj = c(Yj)
      Nj = listaj$Nrg[lj]
      if (method == 'SSVS') {
        itauij[[lj]][gam_iter[[lj]][,i2] == 0] = tauij[[lj]][gam_iter[[lj]][,i2] == 0]
        itauij[[lj]][gam_iter[[lj]][,i2] == 1] = cij[[lj]][gam_iter[[lj]][,i2] == 1]*tauij[[lj]][gam_iter[[lj]][,i2] == 1]
        Dj[[lj]] = diag(itauij[[lj]])
        theta0j = list()
        theta0j[[lj]] = rep(0,k*etam[lj])
      }else if (method == 'KUO') {
        Dj[[lj]] = diag(k*etam[lj])
        Rj[[lj]] = sigma0j[[lj]]}
      Vj = solve(t(diag(gam_iter[[lj]][,i2])) %*% t(Xj) %*% {diag(Nj) %x% solve(sigma_iter[[lj]][[i2]])} %*% Xj %*% diag(gam_iter[[lj]][,i2]) + solve(Dj[[lj]] %*% Rj[[lj]] %*% Dj[[lj]]))
      thetaj = Vj %*% {t(diag(gam_iter[[lj]][,i2])) %*% t(Xj) %*% {diag(Nj) %x% solve(sigma_iter[[lj]][[i2]])} %*% yj + solve(sigma0j[[lj]]) %*% theta0j[[lj]]}
      theta_iter[[lj]][,i2 + 1] = mvtnorm::rmvnorm(1,mean = thetaj,sigma = Vj)
      Hj = ks::invvec({Xj %*% diag(gam_iter[[lj]][,i2]) %*% theta_iter[[lj]][,i2 + 1]},nrow = k,ncol = Nj)
      Sj = (Yj - Hj) %*% t(Yj - Hj)
      sigma_iter[[lj]][[i2 + 1]] = MCMCpack::riwish(v = Nj + nu0j[[lj]],S = Sj + S0j[[lj]])
      gam_iter[[lj]][,i2 + 1] = gam_iter[[lj]][,i2]
    }
    #gamma
    for (jj in 1:l) {
      gam_iter[[jj]][,i2 + 1] = rgamber(pos = 1:{k*etam[jj]},reg = jj,ig = i2 + 1)
    }
    #r
    if (l != 1) {
      if (i2 < 70) {
          ek = mvtnorm::rmvnorm(1,mean = rep(0,l - 1),sigma = 0.5*diag(l - 1))
        }else{
          ek =  stats::runif(l - 1,-abs(rini$val_rmh),abs(rini$val_rmh))
          }
     # ek = runif(l - 1,-abs(rini$val_rmh),abs(rini$val_rmh))
      rk = r_iter[,i2] + ek
      listark = lists(l,rk,pjmax,qjmax,djmax,etam)
      pr = dmunif(rk,a,b)*fycond(i2 + 1,listark,gam_iter,theta_iter,sigma_iter)
      px = dmunif(r_iter[,i2],a,b)*fycond(i2 + 1,listaj,gam_iter,theta_iter,sigma_iter)
      alpha = min(1,as.numeric(pr/px))
      if (alpha >= stats::runif(1)) {
        r_iter[,i2 + 1] = rk
        listr = listark
      }else{
        r_iter[,i2 + 1] = r_iter[,i2]
        listr = listaj
      }
    }else{
      listr = lists(l,0,pjmax,qjmax,djmax,etam)
    }
    listPr$Chain = list(Theta = theta_iter, Sigma = sigma_iter,Gamma = gam_iter, r = r_iter)
    listPr$listr = listr
    listPr$i = i2 + 1
    return(listPr)
  }
  updatelist = compiler::cmpfun(updatelist)
  rpseudo = function(l,...){
    listPr = listm[[paste0('m',l)]]
    i2 = listPr$i + 1
    pjmax = listPr$orders$pj
    qjmax = listPr$orders$qj
    djmax = listPr$orders$dj
    etam = 1 + pjmax*k + qjmax*nu + djmax
    theta_iter = listPr$Chain$Theta
    sigma_iter = listPr$Chain$Sigma
    gam_iter = listPr$Chain$Gamma
    r_iter = listPr$Chain$r
    theta0jS = listPr$Pseudo$Theta$mean
    sigma0jS = listPr$Pseudo$Theta$cov
    S0jS = listPr$Pseudo$Sigma$cov
    nu0jS = listPr$Pseudo$Sigma$gl
    pijS = listPr$Pseudo$Gamma$prob
    rmeanS = listPr$Pseudo$r$mean
    rcovS = listPr$Pseudo$r$cov
    for (lj in 1:l) {
      theta_iter[[lj]][,i2] = mvtnorm::rmvnorm(1,mean = theta0jS[[lj]],sigma = sigma0jS[[lj]])
      sigma_iter[[lj]][[i2]] = 1/nu0jS[[lj]]*MCMCpack::rwish(v = nu0jS[[lj]] ,S = S0jS[[lj]])
      sigma_iter[[lj]][[i2]] = sigma_iter[[lj]][[i2]] %*% sigma_iter[[lj]][[i2]]
      for (iga in 1:{k*etam[lj]}) {
        gam_iter[[lj]][iga,i2] = stats::rbinom(n = 1,size = 1,prob = pijS[[lj]][iga])
      }
    }
    if (l != 1) {
      r_iter[,i2] = mvtnorm::rmvnorm(1,mean = rmeanS, sigma = as.matrix(rcovS))
      listPr$listr = lists(l,r_iter[,i2],pjmax,qjmax,djmax,etam)
    }else{
      listPr$listr = lists(l,0,pjmax,qjmax,djmax,etam)
    }
    listPr$Chain = list(Theta = theta_iter, Sigma = sigma_iter,Gamma = gam_iter, r = r_iter)
    listPr$i = i2
    return(listPr)
  }
  rpseudo = compiler::cmpfun(rpseudo)
  ### Function to compute quotient
  prodA = function(thetaym, thetaymp){
    pgammaPn = pthetaPn = psigmaPn = Brobdingnag::as.brob(1)
    pgammaPd = pthetaPd = psigmaPd = Brobdingnag::as.brob(1)
    pgammaSn = pthetaSn = psigmaSn = Brobdingnag::as.brob(1)
    pgammaSd = pthetaSd = psigmaSd = Brobdingnag::as.brob(1)
    lm = length(thetaym$listr$Nrg)
    lmp = length(thetaymp$listr$Nrg)
    theta_iterm = thetaym$Chain$Theta
    sigma_iterm = thetaym$Chain$Sigma
    gam_iterm = thetaym$Chain$Gamma
    r_iterm = thetaym$Chain$r
    iAm = thetaym$i
    iAmp = thetaymp$i
    theta_itermp = thetaymp$Chain$Theta
    sigma_itermp = thetaymp$Chain$Sigma
    gam_itermp = thetaymp$Chain$Gamma
    r_itermp = thetaymp$Chain$r
    theta0jmp = thetaymp$Priori$Theta$mean
    sigma0jmp = thetaymp$Priori$Theta$cov
    S0jmp = thetaymp$Priori$Sigma$cov
    nu0jmp = thetaymp$Priori$Sigma$gl
    pijmp = thetaymp$Priori$Gamma$prob
    theta0jm = thetaym$Priori$Theta$mean
    sigma0jm = thetaym$Priori$Theta$cov
    S0jm = thetaym$Priori$Sigma$cov
    nu0jm = thetaym$Priori$Sigma$gl
    pijm = thetaym$Priori$Gamma$prob
    theta0jSm = thetaym$Pseudo$Theta$mean
    sigma0jSm = thetaym$Pseudo$Theta$cov
    S0jSm = thetaym$Pseudo$Sigma$cov
    nu0jSm = thetaym$Pseudo$Sigma$gl
    pijSm = thetaym$Pseudo$Gamma$prob
    rmeanSm = thetaym$Pseudo$r$mean
    rcovSm = thetaym$Pseudo$r$cov
    theta0jSmp = thetaymp$Pseudo$Theta$mean
    sigma0jSmp = thetaymp$Pseudo$Theta$cov
    S0jSmp = thetaymp$Pseudo$Sigma$cov
    nu0jSmp = thetaymp$Pseudo$Sigma$gl
    pijSmp = thetaymp$Pseudo$Gamma$prob
    rmeanSmp = thetaymp$Pseudo$r$mean
    rcovSmp = thetaymp$Pseudo$r$cov
    for (lj in 1:lmp) {
      pgammaPn = pgammaPn*prodB(Brobdingnag::as.brob(stats::dbinom(gam_itermp[[lj]][,iAmp],size = 1,prob = pijmp[[lj]])))
      pthetaPn = pthetaPn*dmnormB(theta_itermp[[lj]][,iAmp],mean = theta0jmp[[lj]], sigma = sigma0jmp[[lj]])
      psigmaPn = psigmaPn*dwishartB(sigma_itermp[[lj]][[iAmp]], nu = nu0jmp[[lj]],S = solve(as.matrix(S0jmp[[lj]])))
      pgammaSd = pgammaSd*prodB(Brobdingnag::as.brob(stats::dbinom(gam_itermp[[lj]][,iAmp],size = 1,prob = pijSmp[[lj]])))
      pthetaSd = pthetaSd*dmnormB(theta_itermp[[lj]][,iAmp],mean = theta0jSmp[[lj]], sigma = sigma0jSmp[[lj]])
      psigmaSd = psigmaSd*dwishartB(expm::sqrtm(sigma_itermp[[lj]][[iAmp]]), nu = nu0jSmp[[lj]],S = as.matrix(S0jSmp[[lj]]))
    }
    fn = fycond(iAmp,thetaymp$listr,thetaymp$Chain$Gamma,thetaymp$Chain$Theta,thetaymp$Chain$Sigma)
    for (lj in 1:lm) {
      pgammaPd = pgammaPd*prodB(Brobdingnag::as.brob(stats::dbinom(gam_iterm[[lj]][,iAm],size = 1,prob = pijm[[lj]])))
      pthetaPd = pthetaPd*dmnormB(theta_iterm[[lj]][,iAm],mean = theta0jm[[lj]], sigma = sigma0jm[[lj]])
      psigmaPd = psigmaPd*dwishartB(sigma_iterm[[lj]][[iAm]], nu = nu0jm[[lj]],S = solve(as.matrix(S0jm[[lj]])))
      pgammaSn = pgammaSn*prodB(Brobdingnag::as.brob(stats::dbinom(gam_iterm[[lj]][,iAm],size = 1,prob = pijSm[[lj]])))
      pthetaSn = pthetaSn*dmnormB(theta_iterm[[lj]][,iAm],mean = theta0jSm[[lj]], sigma = sigma0jSm[[lj]])
      psigmaSn = psigmaSn*dwishartB(expm::sqrtm(sigma_iterm[[lj]][[iAm]]), nu = nu0jSm[[lj]],S = as.matrix(S0jSm[[lj]]))
    }
    if (lmp != 1) {
      prPn = dmunif(r_itermp[,iAmp],a,b)
      prSd = dmnormB(r_itermp[,iAmp],mean = rmeanSmp, sigma = as.matrix(rcovSmp))
    }else{
      prPn = prSd = 1
    }
    if (lm != 1) {
      prPd = dmunif(r_iterm[,iAm],a,b)
      prSn = dmnormB(r_iterm[,iAm],mean = rmeanSm, sigma = rcovSm)
    }else{
      prPd = prSn = 1
    }
    fd = fycond(iAm,thetaym$listr,thetaym$Chain$Gamma,thetaym$Chain$Theta,thetaym$Chain$Sigma)
    # Calculo para el valor del cociente
    vald = fd*(pgammaPd*pthetaPd*psigmaPd*prPd)*(pgammaSd*pthetaSd*psigmaSd*prSd)
    valn = fn*(pgammaPn*pthetaPn*psigmaPn*prPn)*(pgammaSn*pthetaSn*psigmaSn*prSn)
    val = valn/vald
    return(val)
  }
  prodA = compiler::cmpfun(prodA)
  # Previous runs
  message('Running previous chains ... \n')
  listm = vector('list')
  if (parallel) {
    micluster = parallel::makeCluster(2)
    doParallel::registerDoParallel(micluster)
    funcParallel = function(i,iterprev){return(fill(m = i,iter = iterprev, burn = round(0.3*iterprev)))}
    parallel::clusterEvalQ(micluster, library(BMTAR))
    obj_S = list('ini_obj','burn_m','niter_m','chain_m','list_m',
              'ordersprev','k','N','nu','method','fill','maxpj','maxqj','maxdj',
              'Zt','Yt','Xt','Ut','lists','fycond','rdunif','dmunif','l0_min')
    parallel::clusterExport(cl = micluster,varlist = obj_S,envir = environment())
    s = parallel::parLapply(micluster,as.list(l0_min:l0), funcParallel, iterprev = iterprev)
    cc = 1
    for (o in l0_min:l0) {
      listm[[paste0('m',o)]] = s[[cc]]
      cc = cc + 1
    }
    parallel::stopCluster(micluster)
  }else{
    for (o in l0_min:l0) {
      listm[[paste0('m',o)]] = fill(m = o,iter = iterprev, burn = round(0.3*iterprev))
    }
  }
  if (NAIC) {
    results = list(tsregim = ini_obj$tsregime_obj,list_m = listm)
    for (i in l0_min:l0) {
      results$NAIC[[paste0('m',i)]] = mtarNAIC(listm[[paste0('m',i)]]$par)
    }
    x_NAIC = unlist(sapply(results$NAIC,function(x){x$NAIC}))
    results$NAIC_final_m = as.numeric(gsub('m','',names(x_NAIC[which.min(x_NAIC)])))
    return(results)
  }
  # Code
  m_iter = vector('numeric')
  m_iter[1] = sample(l0_min:l0,1)
  acepm = 0
  pm_im = matrix(nrow = l0,ncol = niter_m + burn_m - 1)
  message('Estimating number of regimes ... \n')
  pb = utils::txtProgressBar(min = 2, max = niter_m + burn_m,style = 3)
  for (im in 2:{niter_m + burn_m}) {
    m_iter[im] = rdunif(m_iter[im - 1],l0)
    # Generamos valor para Theta_m y Theta_mp
    listm[[paste0('m',m_iter[im - 1])]] = updatelist(l = m_iter[im - 1])
    listm[[paste0('m',m_iter[im])]] = rpseudo(l = m_iter[im])
    # Calculo para la probabilidad alpha
    val = prodA(thetaym = listm[[paste0('m',m_iter[im - 1])]],
                thetaymp = listm[[paste0('m',m_iter[im])]])
    # Evaluacion de el criterio
    alpham = min(1,as.numeric(val))
    if (alpham >= stats::runif(1)) {
      m_iter[im] = m_iter[im]
      acepm = acepm + 1
    }else{
      m_iter[im] = m_iter[im - 1]
    }
    pm_im[,im - 1] = table(factor(m_iter,levels = 1:l0))/im
    utils::setTxtProgressBar(pb,im)
  }
  close(pb)
  message('Saving results ... \n')
  # exits
  m_iter = m_iter[-c(1:burn_m)]
  vecm = sort(table(m_iter)/length(m_iter)*100,decreasing = TRUE)
  namest = c('first freq','second freq','third freq','fourth freq')
  ls = as.numeric(names(vecm))
  names(vecm) = paste(namest[1:length(vecm)],paste0(round(vecm,2),'%'))
  vecm[1:length(vecm)] = ls[1:length(vecm)]
  if (chain_m & list_m) {
    results = list(tsregim = ini_obj$tsregime_obj,
                   list_m = listm ,m_chain = m_iter,
                   prop = pm_im,
                   estimates = vecm,final_m = vecm[[1]])
  }else if (chain_m & !list_m) {
    results = list(tsregim = ini_obj$tsregime_obj,
                   m_chain = m_iter,
                   prop = pm_im,
                   estimates = vecm,final_m = vecm[1])
  }else if (!chain_m & list_m) {
    results = list(tsregim = ini_obj$tsregime_obj,
                   list_m = listm,
                   prop = pm_im,
                   estimates = vecm,final_m = vecm[1])
  }else if (!chain_m & !list_m) {
    results = list(tsregim = ini_obj$tsregime_obj,
                   prop = pm_im,
                   estimates = vecm,
                   final_m = vecm[1])
  }
  for (o in l0_min:l0) {
    results$NAIC[[paste0('m',o)]] = mtarNAIC(listm[[paste0('m',o)]]$par)
  }
  compiler::enableJIT(0)
  class(results) = 'regime_number'
  return(results)
}
