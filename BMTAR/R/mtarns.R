#==================================================================================================#
# Date: 07/04/2019
# Description:
#-> When r_init is NULL default is the proportion that separate the observations in l equal parts
#-> When r is unknown, a metropolis-hasting algorithm is used for its posteriori with an uniform proposal
# Function:
#==================================================================================================#
mtarns = function(ini_obj, level = 0.95, burn = NULL, niter = 1000, chain = FALSE, r_init = NULL){
  if (!is.logical(chain)) {stop('chain must be a logical object')}
  # checking
  if (!inherits(ini_obj, 'regime_inipars')) {
    stop('ini_obj must be a regime_inipars object')
  }
  # data
  Yt = ini_obj$tsregime_obj$Yt
  Ut = cbind(ini_obj$tsregime_obj$Zt,ini_obj$tsregime_obj$Xt)
  k = ini_obj$tsregime_obj$k
  N = ini_obj$tsregime_obj$N
  nu = ini_obj$tsregime_obj$nu
  if (is.null(nu)) {nu = 0}
  # parameters
  r = ini_obj$pars$r
  l = ini_obj$pars$l
  if (is.null(ini_obj$pars$orders)) {
    stop('orders must be known and enter in list_model$pars$orders to use mtarns')
  }
  orders = ini_obj$pars$orders
  # code
  burn = ifelse(is.null(burn),round(0.3*niter),burn)
  other = 100
  pj = orders$pj
  qj = orders$qj
  dj = orders$dj
  Yt = t(Yt)
  if (l == 1) {
    if (is.null(Ut)) {
      Ut = matrix(0, ncol = N,nrow = 1)
    }else{
      Ut = rbind(matrix(0, ncol = N,nrow = 1),t(Ut)) # only for co-variable
    }
  }else{Ut = t(Ut)}
  Zt = Ut[1,]
  if (nu == 0) {
    Xt = matrix(0,ncol = N,nrow = 1)
    qj = rep(0,l)
  }else{
    Xt = t(ini_obj$tsregime_obj$Xt)
  }
  eta = 1 + pj*k + qj*nu + dj
  # functions and values for r
  dmunif = function(r,a,b){
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
  # initials values for r
  rini = ini_obj$init$r
  if (is.null(r)) {
    a = ifelse(is.null(rini$za),min(Zt),stats::quantile(Zt,probs = rini$za))
    b = ifelse(is.null(rini$zb),max(Zt),stats::quantile(Zt,probs = rini$zb))
  }
  lists = function(r,...){
    rj = matrix(nrow = 2,ncol = l)
    if (l == 1) {
      rj[,1] = c(-Inf,Inf)
    }else{
      rj[,1] = c(-Inf,r[1])
      rj[,l] = c(rev(r)[1],Inf)
    }
    if (l > 2) {for (i2 in 2:{l - 1}) {rj[,i2] = c(r[i2 - 1],r[i2])}}
    # indicator variable for the regime
    Ind = vector(mode = 'numeric',length = N)
    for (j in 1:l) {
      Ind[Zt > rj[1,j] & Zt <= rj[2,j]] = j
    }
    Nrg = vector(mode = 'numeric')
    listaWj = listaYj = vector('list', l)
    Inj_W = function(ti,Yt,Zt,Xt,p,q,d){
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
    Inj_W = Vectorize(Inj_W,vectorize.args = "ti")
    for (lj in 1:l) {
      p = pj[lj]
      q = qj[lj]
      d = dj[lj]
      maxj = max(p,q,d)
      Inj = which(Ind == lj)
      Inj = Inj[Inj > maxj]
      Nrg[lj] = length(Inj)
      Yj = matrix(Yt[,Inj],nrow = k,ncol = Nrg[lj])
      # matrix Wj =(1,lagY,lagX,lagZ)
      if (identical(Inj,integer(0))) {
        Wj = matrix(nrow = eta[lj],ncol = 0)
      }else{
        Wj = sapply(Inj,Inj_W,Yt = Yt,Zt = Zt,Xt = Xt,p = p,q = q,d = d)
      }
      listaWj[[lj]] = Wj
      listaYj[[lj]] = Yj
    }
    return(list(Nrg = Nrg,listaW = listaWj,listaY = listaYj,Ind = Ind))
  }
  fycond = function(i2,listr,...){
    acum = 0
    Nrg = listr$Nrg
    for (lj in 1:l) {
      yj = c(listr$listaY[[lj]])
      Wj = listr$listaW[[lj]]
      if (is.null(Sigma)) {
        acum = acum + t(yj - {t(Wj) %x% diag(k)} %*% theta_iter[[lj]][,i2]) %*% {
          diag(Nrg[lj]) %x% solve(sigma_iter[[lj]][[i2]])} %*% (yj - {t(Wj) %x% diag(k)} %*% theta_iter[[lj]][,i2])
      }else{
        acum = acum + t(yj - {t(Wj) %x% diag(k)} %*% theta_iter[[lj]][,i2]) %*% {
          diag(Nrg[lj]) %x% solve(sigma[[lj]])} %*% (yj - {t(Wj) %x% diag(k)} %*% theta_iter[[lj]][,i2])
      }
    }
    if (is.null(Sigma)) {
      sigmareg = lapply(sigma_iter,function(x){x[[i2]]})
      val = prodB(Brobdingnag::as.brob(sapply(sigmareg,function(x){
        return(c(determinant(x,logarithm = FALSE)$modulus))}))^{-Nrg/2})*exp(-1/2*Brobdingnag::as.brob(acum))
    }else{
      val = prodB(Brobdingnag::as.brob(sapply(sigma,function(x){
        return(c(determinant(x,logarithm = FALSE)$modulus))}))^{-Nrg/2})*exp(-1/2*Brobdingnag::as.brob(acum))
    }
    return(val)
  }
  # objects for each regimen and iterations
  Sigma = ini_obj$pars$Sigma
  theta_iter = itheta0j = icov0j = vector('list')
  if (is.null(Sigma)) {
    sigma_iter = iS0j = inu0j = vector('list')
  }else{
    sigma = vector('list')
  }
  # set initial values for each regime in each chain
  thetaini = ini_obj$init$Theta
  for (lj in 1:l) {
    theta_iter[[lj]] = matrix(ncol = niter + burn + other,nrow = k*eta[lj])
    itheta0j[[lj]] = thetaini[[paste0('R',lj)]]$theta0j
    icov0j[[lj]] = thetaini[[paste0('R',lj)]]$cov0j
    theta_iter[[lj]][,1] = mvtnorm::rmvnorm(1,mean = itheta0j[[lj]],sigma = icov0j[[lj]])
    if (is.null(Sigma)) {
      sigma_iter[[lj]] = vector('list',length = niter + burn + other)
      sigmaini = ini_obj$init$Sigma
      iS0j[[lj]] = sigmaini[[paste0('R',lj)]]$S0j
      inu0j[[lj]] = sigmaini[[paste0('R',lj)]]$nu0j
      sigma_iter[[lj]][[1]] = MCMCpack::riwish(v = inu0j[[lj]],S = iS0j[[lj]])
    }else{
      sigma[[lj]] = Sigma[[paste0('R',lj)]]
    }
  }
  # last check
  if (is.null(r) & l == 1) {r = 0
  }else if (is.null(r) & l != 1) {
    r_iter = matrix(ncol = niter + burn + other,nrow = l - 1)
    if (!is.null(r_init)) {
      if (is.numeric(r_init) & length(r_init) == {l - 1}) {
        if (dmunif(r_init,a,b) == 0) {
          stop('r_init must be in Zt range and for l >= 2, r[i] < r[i+1]')
        }
      }else{stop('r_init must be vector of length l - 1')}
    }
    if (l != 1) {
      if (is.null(r_init)) {
        r_iter[,1] = c(stats::quantile(Zt, probs = 1/l*(1:{l - 1})))
      }else{
        r_iter[,1] = r_init
      }
    }
  }
  # iterations gibbs and metropolis for r unknown
  if (is.null(r)) {
    message('Estimating non-structural parameters and threshold(s) ...','\n')
    pb = utils::txtProgressBar(min = 2, max = niter + burn + other, style = 3)
    acep = 0
    for (i in 2:{niter + burn + other}) {
      listj = lists(r_iter[,i - 1])
      for (lj in 1:l) {
        Wj = listj$listaW[[lj]]
        Yj = listj$listaY[[lj]]
        Nj = listj$Nrg[lj]
        yj = c(Yj)
        theta0j = itheta0j[[lj]]
        sigma0j = icov0j[[lj]]
        if (!is.null(Sigma)) {
          Vj = solve(Wj %*% t(Wj) %x% solve(sigma[[lj]] %*% sigma[[lj]]) + solve(sigma0j))
          thetaj = Vj %*% {(Wj %x% solve(sigma[[lj]] %*% sigma[[lj]])) %*% yj + solve(sigma0j) %*% theta0j}
          theta_iter[[lj]][,i] = mvtnorm::rmvnorm(1,mean = thetaj,sigma = Vj)
        }else{
          S0j = iS0j[[lj]]
          nu0j = inu0j[[lj]]
          Vj = solve(Wj %*% t(Wj) %x% solve(sigma_iter[[lj]][[i - 1]]) + solve(sigma0j))
          thetaj = Vj %*% {(Wj %x% solve(sigma_iter[[lj]][[i - 1]])) %*% yj + solve(sigma0j) %*% theta0j}
          theta_iter[[lj]][,i] = mvtnorm::rmvnorm(1,mean = thetaj,sigma = Vj)
          Hj = ks::invvec(theta_iter[[lj]][,i],nrow = k,ncol = eta[lj])
          Sj = (Yj - Hj %*% Wj) %*% t(Yj - Hj %*% Wj)
          sigma_iter[[lj]][[i]] = MCMCpack::riwish(v = Nj + nu0j,S = Sj + S0j)
        }
      }
      # use of metropolis with random walk
      if (i <= other) {
        ek = mvtnorm::rmvnorm(1,mean = rep(0,l - 1),sigma = 0.5*diag(l - 1))
      }else{
        ek = stats::runif(l - 1,-abs(rini$val_rmh),abs(rini$val_rmh))
      }
      rk = r_iter[,i - 1] + ek
      listrk = lists(rk)
      pr = dmunif(rk,a,b)*fycond(i,listrk)
      px = dmunif(r_iter[,i - 1],a,b)*fycond(i,listj)
      alpha = min(1,as.numeric(pr/px))
      if (alpha >= stats::runif(1)) {
        r_iter[,i] = rk
        acep = acep + 1
      }else{
        r_iter[,i] = r_iter[,i - 1]
        acep = acep
      }
      utils::setTxtProgressBar(pb,i)
    }
    close(pb)
    message('\n')
  }else{#r known
    listj = lists(r)
    for (lj in 1:l) {
      Wj = listj$listaW[[lj]]
      Yj = listj$listaY[[lj]]
      Nj = listj$Nrg[lj]
      yj = c(Yj)
      theta0j = itheta0j[[lj]]
      sigma0j = icov0j[[lj]]
      message('Estimating non-structural parameters with threshold(s) known ...',paste0('Reg_',lj),'\n')
      pb = utils::txtProgressBar(min = 2, max = niter + burn + other, style = 3)
      for (i in 2:{niter + burn + other}) {
        if (!is.null(Sigma)) {
          Vj = solve(Wj %*% t(Wj) %x% solve(sigma[[lj]] %*% sigma[[lj]]) + solve(sigma0j))
          thetaj = Vj %*% {(Wj %x% solve(sigma[[lj]] %*% sigma[[lj]])) %*% yj + solve(sigma0j) %*% theta0j}
          theta_iter[[lj]][,i] = mvtnorm::rmvnorm(1,mean = thetaj,sigma = Vj)
        }else{
          S0j = iS0j[[lj]]
          nu0j = inu0j[[lj]]
          Vj = solve(Wj %*% t(Wj) %x% solve(sigma_iter[[lj]][[i - 1]]) + solve(sigma0j))
          thetaj = Vj %*% {(Wj %x% solve(sigma_iter[[lj]][[i - 1]])) %*% yj + solve(sigma0j) %*% theta0j}
          theta_iter[[lj]][,i] = mvtnorm::rmvnorm(1,mean = thetaj,sigma = Vj)
          Hj = ks::invvec(theta_iter[[lj]][,i],nrow = k,ncol = eta[lj])
          Sj = (Yj - Hj %*% Wj) %*% t(Yj - Hj %*% Wj)
          sigma_iter[[lj]][[i]] = MCMCpack::riwish(v = Nj + nu0j,S = Sj + S0j)
        }
        utils::setTxtProgressBar(pb,i)
      }
      message('\n')
    }
    close(pb)
  }
  message('Saving results ... \n')
  # objects for chains and info in each regime
  Rest = thetaest = thetachain = vector('list', l)
  names(Rest) = names(thetaest) = names(thetachain) = paste0('R',1:l)
  if (is.null(Sigma)) {
    sigmaest = sigmachain = vector('list', l)
    names(sigmaest) = names(sigmachain) = paste0('R',1:l)
  }
  # save chains and creation of the 'regime' type object
  if (is.null(r)) {
    if (l > 2) {
      r_iter = r_iter[,-c(1:{other + burn})]
    }else{
      r_iter = r_iter[-c(1:{other + burn})]
    }
    rest = matrix(nrow = l - 1,ncol = 3)
    colnames(rest) = colnames(rest) =
      c(paste('lower limit ',(1 - level)/2*100,'%',sep = ''),'mean',paste('upper limit ',(1 + level)/2*100,'%',sep = ''))
    rchain = matrix(r_iter,ncol = niter,nrow = l - 1)
    rest[,1] = apply(rchain,1,stats::quantile,probs = (1 - level)/2)
    rest[,3] = apply(rchain,1,stats::quantile,probs = (1 + level)/2)
    rest[,2] = apply(rchain,1,mean)
    rvec = c(rest[,2],'prop %' = acep/niter*100)
  }else{
    rvec = c('mean' = r)
  }
  # logLik
  listj = lists(rvec[1])
  logLikj = vector(mode = "numeric")
  for (lj in 1:l) {
    theta_iter[[lj]] = theta_iter[[lj]][,-c(1:other)]
    # save chains of theta
    thetachain[[lj]] = theta_iter[[lj]][,-c(1:burn)]
    # credibility intervals for theta
    vectheta = matrix(nrow = k*eta[lj],ncol = 3)
    colnames(vectheta) = c(paste0('lower limit ',(1 - level)/2*100,'%'),'mean',paste0('upper limit ',(1 + level)/2*100,'%'))
    vectheta[,1] = apply(thetachain[[lj]],1,stats::quantile,probs = (1 - level)/2)
    vectheta[,3] = apply(thetachain[[lj]],1,stats::quantile,probs = (1 + level)/2)
    vectheta[,2] = apply(thetachain[[lj]],1,mean)
    thetaest[[lj]] = vectheta
    if (nu != 0 & qj[lj] != 0 & dj[lj] != 0) {
      rownames(vectheta) =
        rep(c('phi0',rep(paste0('phi',1:pj[lj]),each = k),rep(paste0('beta',1:qj[lj]),each = nu),paste0('delta',1:dj[lj])),k)
    }else if (nu != 0 & qj[lj] != 0 & dj[lj] == 0) {
      rownames(vectheta) =
        rep(c('phi0',rep(paste0('phi',1:pj[lj]),each = k),rep(paste0('beta',1:qj[lj]),each = nu)),k)
    }else if (qj[lj] == 0 & dj[lj] != 0) {
      rownames(vectheta) =
        rep(c('phi0',rep(paste0('phi',1:pj[lj]),each = k),paste0('delta',1:dj[lj])),k)
    }else if (qj[lj] == 0 & dj[lj] == 0) {
      rownames(vectheta) =
        rep(c('phi0',rep(paste0('phi',1:pj[lj]),each = k)),k)
    }
    if (is.null(Sigma)) {
      sigma_iter[[lj]] = sigma_iter[[lj]][-c(1:other)]
      SigmaPrep = function(x){return(c(expm::sqrtm(matrix(x,k,k))))}
      # save chains of sigma
      sigmachain[[lj]] = sapply(sigma_iter[[lj]][-c(1:burn)], ks::vec)
      # credibility intervals for sigma^1/2
      vecsigma = matrix(nrow = k*k,ncol = 3)
      colnames(vecsigma) = c(paste0('lower limit ',(1 - level)/2*100,'%'),'mean',paste0('upper limit ',(1 + level)/2*100,'%'))
      rownames(vecsigma) = c(sapply(1:k, function(x){paste0(1:k,x)}))
      if (k == 1) {
        vecsigma[,1] = sqrt(stats::quantile(sigmachain[[lj]],probs = (1 - level)/2))
        vecsigma[,3] = sqrt(stats::quantile(sigmachain[[lj]],probs = (1 + level)/2))
        vecsigma[,2] = sqrt(mean(sigmachain[[lj]]))
      }else{
        vecsigma[,1] = SigmaPrep(apply(sigmachain[[lj]],1,stats::quantile,probs = (1 - level)/2))
        vecsigma[,3] = SigmaPrep(apply(sigmachain[[lj]],1,stats::quantile,probs = (1 + level)/2))
        vecsigma[,2] = SigmaPrep(apply(sigmachain[[lj]],1,mean))
      }
      sigmaest[[lj]] = vecsigma
    }
    # creation of the 'regime' type object
    p = pj[lj]
    q = qj[lj]
    d = dj[lj]
    if (q == 0 & d == 0) {
      thetaind = c(0,(10 + (1:p)) %x% rep(1,k))
    }else if (q != 0 & d == 0) {
      thetaind = c(0,(10 + (1:p)) %x% rep(1,k),(20 + (1:q)) %x% rep(1,nu))
    }else if (q == 0 & d != 0) {
      thetaind = c(0,(10 + (1:p)) %x% rep(1,k),30 + (1:d))
    }else{
      thetaind = c(0,(10 + (1:p)) %x% rep(1,k),(20 + (1:q)) %x% rep(1,nu),30 + (1:d))
    }
    Thetaj = ks::invvec(thetaest[[lj]][,2],ncol = eta[lj],nrow = k)
    Ri = vector('list')
    Ri$cs = matrix(Thetaj[,thetaind == 0],nrow = k,ncol = 1)
    Ri$phi = vector('list', p)
    names(Ri$phi) = paste0('phi',1:p)
    for (j in 1:p) {
      Ri$phi[[j]] = matrix(Thetaj[,thetaind == (10 + j)],nrow = k,ncol = k)
    }
    if (q != 0) {
      Ri$beta = vector('list', q)
      names(Ri$beta) = paste0('beta',1:q)
      for (j in 1:q) {Ri$beta[[j]] = matrix(Thetaj[,thetaind == (20 + j)],nrow = k,ncol = nu)}
    }
    if (d != 0) {
      Ri$delta = vector('list', d)
      names(Ri$delta) = paste0('delta',1:d)
      for (j in 1:d) {Ri$delta[[j]] = matrix(Thetaj[,thetaind == (30 + j)],nrow = k,ncol = 1)}
    }
    if (is.null(Sigma)) {
      Ri$sigma = ks::invvec(sigmaest[[lj]][,2],ncol = k,nrow = k)
    }else{
      Ri$sigma = sigma[[lj]]
    }
    Rest[[lj]] = mtaregime(orders = list(p = p,q = q,d = d),cs = Ri$cs,
                          Phi = Ri$phi,Beta = Ri$beta,Delta = Ri$delta,
                          Sigma = Ri$sigma)
    Wj = listj$listaW[[lj]]
    Yj = listj$listaY[[lj]]
    Nj = listj$Nrg[lj]
    yj = c(Yj)
    Hj = ks::invvec(thetaest[[lj]][,2],nrow = k,ncol = eta[lj])
    Sj = (Yj - Hj %*% Wj) %*% t(Yj - Hj %*% Wj)
    logLikj[lj] = log(det(Sj/Nj))
  }
  # exits
  ## chain
  if (chain) {
    Chain = vector('list')
    Chain$Theta = thetachain
    if (is.null(r) & l != 1) {Chain$r = rchain}
    if (is.null(Sigma)) {Chain$Sigma = sigmachain}
  }
  ## estimates and credibility interval
  estimates = vector('list')
  estimates$Theta = thetaest
  if (is.null(r) & l != 1) {estimates$r = rest}
  if (is.null(Sigma)) {estimates$Sigma = sigmaest}
  data = ini_obj$tsregime_obj
  # fitted.values and residuals
  Yt_fit = Yt_res = matrix(ncol = N,nrow = k)
  for (t in 1:N) {
    lj = listj$Ind[t]
    p = pj[lj]
    q = qj[lj]
    d = dj[lj]
    Wj = matrix(0,nrow = eta[lj],ncol = 1)
    yti = vector(mode = "numeric")
    for (w in 1:p) {
      if (t - w > 0) {yti = c(yti,Yt[,t - w])
      }else{yti = c(yti,rep(0,k))}}
    if (identical(yti,numeric(0))) {yti = rep(0,k*p)}
    xti = vector(mode = "numeric")
    for (w in 1:q) {
      if (t - w > 0) {xti = c(xti,Xt[,t - w])
      }else{xti = c(xti,rep(0,nu))}}
    if (identical(xti,numeric(0))) {xti = rep(0,nu*q)}
    zti = vector(mode = "numeric")
    for (w in 1:d) {
      if (t - w > 0) {zti = c(zti,Zt[t - w])
      }else{zti = c(zti,0)}}
    if (identical(zti,numeric(0))) {zti = rep(0,d)}
    if (q == 0 & d != 0) {
      wtj = c(1,yti,zti)
    }else if (d == 0 & q != 0) {
      wtj = c(1,yti,xti)
    }else if (d == 0 & q == 0) {
      wtj = c(1,yti)
    }else{
      wtj = c(1,yti,xti,zti)}
    Wj[,1] = wtj
    Hj = ks::invvec(thetaest[[lj]][,2],nrow = k,ncol = eta[lj])
    Sig = as.matrix(Rest[[lj]]$sigma)
    Yt_fit[,t] = Hj %*% Wj
    Yt_res[,t] = solve(Sig) %*% (Yt[,t] - Yt_fit[,t])
  }
  if (chain) {
    results = list(Nj = listj$Nrg,estimates = estimates,regime = Rest,Chain = Chain,
                   residuals = t(Yt_res), fitted.values = t(Yt_fit),
                   logLikj = logLikj,data = data,r = rvec,orders = list(pj = pj,qj = qj,dj = dj))
  }else{
    results = list(Nj = listj$Nrg,estimates = estimates,regime = Rest,
                   residuals = t(Yt_res), fitted.values = t(Yt_fit),
                   logLikj = logLikj,data = data,r = rvec,orders = list(pj = pj,qj = qj,dj = dj))
  }
  class(results) = 'regime_model'
  return(results)
}
