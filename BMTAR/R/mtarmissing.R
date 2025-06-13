#==================================================================================================#
# Date: 14/04/2020
# Description:
# Function:
#==================================================================================================#
mtarmissing = function(ini_obj,niter = 1000, chain = FALSE, level = 0.95, burn = NULL, cU = 0.5, b = NULL) {
  #checking
  compiler::enableJIT(3)
  if (!is.logical(chain)) {stop('chain must be a logical object')}
  if (!inherits(ini_obj, 'regime_inipars')) {
    stop('ini_obj must be a regime_inipars object')
  }
  #code
  symm = function(x) {
    x = (x + t(x)) / 2
    return(x)
  }
  Yt = ini_obj$tsregime_obj$Yt
  Ut = cbind(ini_obj$tsregime_obj$Zt,ini_obj$tsregime_obj$Xt)
  if (!is.na(sum(Ut))) {
    stop('ini_obj$tsregime_obj contains no missing data')
  }
  k = ini_obj$tsregime_obj$k
  N = ini_obj$tsregime_obj$N
  nu = ini_obj$tsregime_obj$nu
  if (is.null(nu)) {nu = 0}
  l = ini_obj$pars$l
  r = ini_obj$pars$r
  pj = ini_obj$pars$orders$pj
  qj = ini_obj$pars$orders$qj
  dj = ini_obj$pars$orders$dj
  burn = ifelse(is.null(burn),round(0.1*niter),burn)
  pmax = max(pj)
  qmax = max(qj)
  dmax = max(dj)
  pmax = ifelse(pmax == 1,2,pmax)
  qmax = ifelse(qmax == 0,1,qmax)
  dmax = ifelse(dmax == 0,1,dmax)
  # first entries
  Yt = t(Yt)
  Ut = t(Ut)
  b = ifelse(is.null(b),1,b)
  Zt = Ut[1,]
  if (nu == 0) {
    Xt = matrix(0,ncol = N,nrow = 1)
    qj = rep(0,l)
  }else{
    Xt = t(ini_obj$tsregime_obj$Xt)
  }
  etaj = 1 + k*pj + nu*qj + dj
  PosNAMat = PosNAvec = PosNAvecT = vector(mode = 'list',2)
  PosNAMat[[1]] = apply(Yt,2,is.na)
  PosNAvec[[1]] = c(1:ncol(Yt))[apply(PosNAMat[[1]],2,any)]
  PosNAvecT[[1]] = matrix(rep(c(1:N),k),nrow = k,ncol = N,byrow = T)[PosNAMat[[1]]]
  if (nu == 0) {
    PosNAMat[[2]] = t(as.matrix(apply(Ut,2,is.na)))
    PosNAvec[[2]] = c(1:ncol(Ut))[PosNAMat[[2]]]
  }else{
    PosNAMat[[2]] = apply(Ut,2,is.na)
    PosNAvec[[2]] = c(1:ncol(Ut))[apply(PosNAMat[[2]],2,any)]
  }
  PosNAvecT[[2]] = matrix(rep(c(1:N),nu + 1),nrow = nu + 1,ncol = N,byrow = T)[PosNAMat[[2]]]
  #Completamos Ut faltantes con promedios Ut
  if (length(PosNAvec[[2]]) != 0) {
    meanU = apply(Ut,1,mean,na.rm = TRUE)
    for (i in 1:nrow(Ut)) {
      Ut[i,PosNAMat[[2]][i,]] = meanU[i]
    }
  }
  #Completar datos faltantes con o en Yt y permutar en Yt y Kt(OJO)
  initialU = mtarinipars(tsregime(t(Ut)),list_model = list(pars = list(l = 1,orders = list(pj = b,qj = 0,dj = 0))))
  message('Estimating model (Zt,Xt) \n')
  modelU = mtarns(ini_obj = initialU,niter = 1000,chain = FALSE,burn = 1000)
  modelU = modelU$regime$R1
  #functions
  lists = function(r, Yt, Ut,...){
    Zt = Ut[1,]
    if (nu == 0) {
      Xt = matrix(0,ncol = N,nrow = 1)
    }else{
      Xt = matrix(Ut[-1,],nrow = nu,ncol = N,byrow = TRUE)
    }
    rj = matrix(nrow = 2,ncol = l)
    if (l == 1) {
      rj[,1] = c(-Inf,Inf)
    }else{
      rj[,1] = c(-Inf,r[1])
      rj[,l] = c(rev(r)[1],Inf)
    }
    if (l > 2) {for (i2 in 2:{l - 1}) {rj[,i2] = c(r[i2 - 1],r[i2])}}
    # indimessageor variable for the regime
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
        Wj = matrix(nrow = etaj[lj],ncol = 0)
      }else{
        Wj = sapply(Inj,Inj_W,Yt = Yt,Zt = Zt,Xt = Xt,p = p,q = q,d = d)
      }
      listaWj[[lj]] = Wj
      listaYj[[lj]] = Yj
    }
    return(list(Nrg = Nrg,listaW = listaWj,listaY = listaYj,Ind = Ind))
  }
  lists = compiler::cmpfun(lists)
  ker = function(t, Ut, ...){
    cs = modelU$cs
    At = as.matrix(as.data.frame(modelU$phi))
    Sig = as.matrix(modelU$sigma)
    EU = solve(diag(nu + 1) - At) %*% cs
    vecVU = solve(diag(2*(nu + 1)) - At %x% At) %*% c(Sig %*% Sig)
    VU = ks::invvec(vecVU,ncol = nu + 1, nrow = nu + 1)
    val = dmnormB(Ut[,t], EU, VU)
    return(c(val))
  }
  transker = function(t, Ut, ...){
    p = length(modelU$phi)
    ## create matrix
    cs = modelU$cs
    At = as.matrix(as.data.frame(modelU$phi))
    Sig = as.matrix(modelU$sigma)
    ## make lags and calculate
    uti = c()
    for (w in 1:p) {uti = c(uti,Ut[,t - w])}
    val = dmnormB(Ut[,t], cs + At %*% uti, Sig %*% Sig)
    return(c(val))
  }
  kernU = compiler::cmpfun(Vectorize(ker,vectorize.args = 't'))
  transkernU = compiler::cmpfun(Vectorize(transker,vectorize.args = 't'))
  state_space = function(reg, iSS, theta, sigma, ...) {
    p = pj[reg]
    q = qj[reg]
    d = dj[reg]
    Aj = ks::invvec(theta[[reg]][,iSS],nrow = k, ncol = etaj[reg])
    Ajf = Aj[,-1]
    if (p >= 1) {phis = Ajf[,1:(k*p)]}else{phis = NULL}
    if (q > 0) {
      betas = Ajf[,(k*p + 1):(k*p + nu*q)]
    }else{betas = NULL}
    if (d > 0) {
      deltas = Ajf[,(k*p + nu*q + 1):(k*p + nu*q + d)]
    }else{deltas = NULL}
    Aj = cbind(Aj[,1],phis, matrix(0,nrow = k, ncol = k*(pmax - p)),
               betas,matrix(0,nrow = k, ncol = nu*(qmax - q)),
               deltas,matrix(0,nrow = k,ncol = dmax - d))
    R_zt = t(cbind(expm::sqrtm(sigma[[reg]][[iSS]]),matrix(0,k,(pmax - 1)*k + nu*qmax + dmax)))
    L_zt = c(Aj[,1],rep(0,(pmax - 1)*k + nu*qmax  + dmax))
    hphi = cbind(diag(k*(pmax - 1)),matrix(0,nrow = k*(pmax - 1), ncol = k + qmax*nu + dmax))
    hbeta = cbind(matrix(0,nrow = nu*(qmax - 1), ncol = k*pmax),diag(nu*(qmax - 1)),matrix(0,nrow = nu*(qmax - 1),ncol = nu + dmax))
    hdelta = cbind(matrix(0,nrow = dmax - 1, ncol = k*pmax + qmax*nu),diag(dmax - 1),matrix(0,ncol = 1,nrow = dmax - 1))
    H_zt = rbind(Aj[,-1],
                 hphi,
                 matrix(0,nrow = nu, ncol = ncol(Aj) - 1),
                 hbeta,
                 matrix(0,nrow = 1, ncol = ncol(Aj) - 1),
                 hdelta)
    K_zt = cbind(diag(k),matrix(0,k,(pmax - 1)*k + nu*qmax + dmax))
    M_zt = rbind(matrix(0,nrow = k*pmax, ncol = nu + 1),
                 cbind(matrix(0,nrow = nu, ncol = 1),diag(nu)),
                 matrix(0,nrow = nu*(qmax - 1), ncol = nu + 1),
                 c(1,rep(0,nu)),
                 matrix(0,nrow = (dmax - 1), ncol = nu + 1))
    rownames(M_zt) = rownames(K_zt) = rownames(L_zt) = rownames(H_zt) = rownames(R_zt) = NULL
    colnames(M_zt) = colnames(K_zt) = colnames(L_zt) = colnames(H_zt) = colnames(R_zt) = NULL
    return(list(K = K_zt, L = L_zt, H = H_zt, M = M_zt, R = R_zt))
  }
  state_space = compiler::cmpfun(state_space)
  alphacond = function(t, iA, Ut, Yt, theta, sigma, ...) {
    Zt = Ut[1,]
    if (nu == 0) {
      Xt = matrix(0,ncol = N,nrow = 1)
    }else{
      Xt = matrix(Ut[-1,],nrow = nu,ncol = N,byrow = TRUE)
    }
    rj = matrix(nrow = 2,ncol = l)
    if (l == 1) {
      rj[,1] = c(-Inf,Inf)
    }else{
      rj[,1] = c(-Inf,r[1])
      rj[,l] = c(rev(r)[1],Inf)
    }
    if (l > 2) {for (i2 in 2:{l - 1}) {rj[,i2] = c(r[i2 - 1],r[i2])}}
    # indimessageor variable for the regime
    Ind = vector(mode = 'numeric',length = N)
    for (j in 1:l) {
      Ind[Zt > rj[1,j] & Zt <= rj[2,j]] = j
    }
    lj = Ind[t]
    p = pj[lj]
    q = qj[lj]
    d = dj[lj]
    Wj = matrix(0,nrow = etaj[lj],ncol = 1)
    yti = c()
    for (w in 1:p) {yti = c(yti,Yt[,t - w])}
    xti = c()
    for (w in 1:q) {xti = c(xti,Xt[,t - w])}
    zti = c()
    for (w in 1:d) {zti = c(zti,Zt[t - w])}
    if (q == 0 & d != 0) {
      wtj = c(1,yti,zti)
    }else if (d == 0 & q != 0) {
      wtj = c(1,yti,xti)
    }else if (d == 0 & q == 0) {
      wtj = c(1,yti)
    }else{
      wtj = c(1,yti,xti,zti)}
    Wj[,1] = wtj
    Hj = ks::invvec(theta[[lj]][,iA],nrow = k,ncol = etaj[lj])
    val = dmnormB(Yt[,t], {Hj %*% Wj}, sigma[[lj]][[iA]])
    return(val)
  }
  alphacond = compiler::cmpfun(Vectorize(alphacond,vectorize.args = 't'))
  #objects for each regimen and iterations
  theta_iter = sigma_iter = vector('list', l)
  itheta0j = isigma0j = vector('list', l)
  iS0j = inu0j = vector('list', l)
  Yt_iter = matrix(ncol = niter + burn,nrow = sum(ks::vec(PosNAMat[[1]])))
  Ut_iter = matrix(ncol = niter + burn,nrow = sum(ks::vec(PosNAMat[[2]])))
  Ytr = Yt #Yt que vamos a cambiar en el proceso
  Utr = Ut #Ut que vamos a cambiar en el proceso
  #Ytr[PosNAMat[[1]]] = 0
  Yt_iter[,1] = Ytr[PosNAMat[[1]]]
  Ut_iter[,1] = Utr[PosNAMat[[2]]]
  #set initial values for each regime in each chain
  #creacion de cadenas para sigma y theta
  for (lj in 1:l) {
    theta_iter[[lj]] = matrix(ncol = niter + burn,nrow = k*etaj[lj])
    itheta0j[[lj]] = ini_obj$init$Theta[[lj]]$theta0j
    isigma0j[[lj]] = ini_obj$init$Theta[[lj]]$cov0j
    theta_iter[[lj]][,1] = mvtnorm::rmvnorm(1,mean = itheta0j[[lj]],sigma = isigma0j[[lj]])
    sigma_iter[[lj]] = vector('list',niter + burn)
    iS0j[[lj]] = ini_obj$init$Sigma[[lj]]$S0j
    inu0j[[lj]] = ini_obj$init$Sigma[[lj]]$nu0j
    sigma_iter[[lj]][[1]] = MCMCpack::riwish(v = inu0j[[lj]],S = iS0j[[lj]])
  }
  #state-space model
  K_zti = K_zt = vector('list')
  R_zt = vector('list')
  L_zt = vector('list')
  H_zt = vector('list')
  M_zt = vector('list')
  #Primera permutaciones
  for (lj in 1:l) {
    listmatrix = state_space(lj, 1, theta_iter,sigma_iter)
    R_zt[[lj]] = listmatrix$R
    L_zt[[lj]] = listmatrix$L
    H_zt[[lj]] = listmatrix$H
    K_zt[[lj]] = listmatrix$K
    M_zt[[lj]] = listmatrix$M
  }
  listj = lists(r, Ytr, Ut)
  for (ij in 1:N) {
    K_zti[[ij]] = K_zt[[listj$Ind[ij]]]
  }
  #permutaciones para cadena Yt:
  for (ij in PosNAvec[[1]]) {
    posNAi = PosNAMat[[1]][,ij]
    if (!all(posNAi)) {
      K_zti[[ij]] = K_zti[[ij]][order(posNAi),]
      Ytr[,ij] = Ytr[,ij][order(posNAi)]
    }
    K_zti[[ij]][is.na(Ytr[,ij]),] = K_zti[[ij]][is.na(Ytr[,ij]),]*0
    Ytr[,ij][is.na(Ytr[,ij])] = 0
  }
  sersalY = Ytr
  # Sampling
  message('Estimating missing data ...\n')
  pb = utils::txtProgressBar(min = 2, max = niter + burn, style = 3)
  for (i in 2:{niter + burn}) {
    #State space model
    PtC = AlphatC = vector('list',N)
    QtC = ytC = vector('list',N)
    Pt = Alphat = vector('list',N + 1)
    Alphat[[1]] = matrix(0,nrow = k*pmax + nu*qmax + dmax,ncol = 1)
    Pt[[1]] = 10*diag(k*pmax + nu*qmax + dmax)
    # iteraciones
    Indi = listj$Ind
    for (i1 in 1:{N}) {
      #Prediction Equations mt|t-1
      AlphatC[[i1]] = H_zt[[Indi[i1]]] %*% Alphat[[i1]] + L_zt[[Indi[i1]]] + M_zt[[Indi[i1]]] %*% Ut[,i1]
      R2 = R_zt[[Indi[i1]]] %*% diag(k) %*% t(R_zt[[Indi[i1]]])
      PtC[[i1]] = H_zt[[Indi[i1]]] %*% Pt[[i1]] %*% t(H_zt[[Indi[i1]]]) + R2
      ytC[[i1]] = K_zti[[i1]] %*% AlphatC[[i1]]
      QtC[[i1]] = K_zti[[i1]] %*% PtC[[i1]] %*% t(K_zti[[i1]])
      #Updating Equations mt
      St = PtC[[i1]] %*% t(K_zti[[i1]]) %*% MASS::ginv(QtC[[i1]])
      Alphat[[i1 + 1]] = AlphatC[[i1]] + St %*% {sersalY[,i1] - ytC[[i1]]}
      Pt[[i1 + 1]] = PtC[[i1]] - St %*% K_zti[[i1]] %*% PtC[[i1]]
    }
    #sampling for state vector (pg37)
    PT = AlphaT = vector('list',N + 1)
    AlphaT[[N + 1]] = Alphat[[N + 1]]
    PT[[N + 1]] = Pt[[N + 1]]
    for (i1 in rev(1:{N})) {
      Eig = eigen(Pt[[i1 + 1]])$values
      Eig = any(Mod(Eig) > exp(-6))
      if (Eig) {
        estUp = MASS::mvrnorm(1,AlphaT[[i1 + 1]],PT[[i1 + 1]])
      }else{
        estUp = AlphaT[[i1 + 1]]
      }
      R2 = R_zt[[Indi[i1]]][1:k,] %*% diag(k) %*% t(R_zt[[Indi[i1]]][1:k,])
      Qt = MASS::ginv(H_zt[[Indi[i1]]][1:k,] %*%  Pt[[i1]] %*% t(H_zt[[Indi[i1]]][1:k,]) + R2)
      Bt = Pt[[i1]] %*% t(H_zt[[Indi[i1]]][1:k,]) %*% Qt
      if (nu == 0) {
        Gt = estUp[1:k] - M_zt[[Indi[i1]]][1:k,]*Ut[,i1] - L_zt[[Indi[i1]]][1:k] - H_zt[[Indi[i1]]][1:k,] %*% Alphat[[i1]]
      }else{
        Gt = estUp[1:k] - M_zt[[Indi[i1]]][1:k,] %*% Ut[,i1] - L_zt[[Indi[i1]]][1:k] - H_zt[[Indi[i1]]][1:k,] %*% Alphat[[i1]]
      }
      AlphaT[[i1]] = Alphat[[i1]] + Bt %*% Gt
      PT[[i1]] = Pt[[i1]] - Bt %*% H_zt[[Indi[i1]]][1:k,] %*% Pt[[i1]]
      PT[[i1]] = symm(PT[[i1]])
    }
    # Simulaion de datos faltantes en Yt
    for (i1 in PosNAvec[[1]]) {
      Ysim = as.matrix(MASS::mvrnorm(1,AlphaT[[i1 + 1]],PT[[i1 + 1]]))
      Yt_iter[,i][PosNAvecT[[1]] == i1] = Ysim[1:k][PosNAMat[[1]][,i1]]
      Ytr[,i1] = Ysim[1:k]
      AlphaT[[i1 + 1]] = Ysim
    }
    #random walk U
    for (i1 in PosNAvec[[2]]) {
      ek = mvtnorm::rmvnorm(1,mean = rep(0,nu + 1), sigma = cU*diag(nu + 1))
      # Simulacion de la propues
      Usim = Utr
      Usim[,i1] = Utr[,i1] + c(ek)
      Usim[!PosNAMat[[2]][,i1],i1] =  Utr[!PosNAMat[[2]][,i1],i1]
      # Calculo de las probabilidades segun sea el caso
      # Numerador
      if (i1 <= b) {
        prod1N = Reduce('*',kernU(1:b,Usim))
        prod2N = Reduce('*',alphacond(1:b,i - 1,Usim,Ytr,theta_iter,sigma_iter))
        prod3N = Reduce('*',transkernU({b + 1}:{2*b},Usim))
        prod1D = Reduce('*',kernU(1:b,Utr))
        prod2D = Reduce('*',alphacond(1:b,i - 1,Utr,Ytr,theta_iter,sigma_iter))
        prod3D = Reduce('*',transkernU({b + 1}:{2*b},Utr))
        val = (prod1N*prod2N*prod3N)/(prod1D*prod2D*prod3D)
      }else{
        prod1N = alphacond(i1,i - 1,Usim,Ytr,theta_iter,sigma_iter)[[1]]
        prod2N = Reduce('*',transkernU(i1:{i1 + b},Usim))
        prod1D = alphacond(i1,i - 1,Utr,Ytr,theta_iter,sigma_iter)[[1]]
        prod2D = Reduce('*',transkernU(i1:{i1 + b},Utr))
        val = (prod1N*prod2N)/(prod1D*prod2D)
      }
      if (val >= stats::runif(1)) {
        Utr = Usim
        Ut_iter[,i][PosNAvecT[[2]] == i1] = Usim[PosNAMat[[2]][,i1],i1]
      }else{
        Utr = Utr
        Ut_iter[,i][PosNAvecT[[2]] == i1] = Utr[,i1][PosNAMat[[2]][,i1]]
      }
    }
    listj = lists(r, Ytr, Utr)
    for (lj in 1:l) {
      Wj = listj$listaW[[lj]]
      Yj = listj$listaY[[lj]]
      Nj = listj$Nrg[lj]
      yj = c(Yj)
      theta0j = itheta0j[[lj]]
      sigma0j = isigma0j[[lj]]
      S0j = iS0j[[lj]]
      nu0j = inu0j[[lj]]
      Vj = solve(Wj %*% t(Wj) %x% solve(sigma_iter[[lj]][[i - 1]]) + solve(sigma0j))
      thetaj = Vj %*% {(Wj %x% solve(sigma_iter[[lj]][[i - 1]])) %*% yj + solve(sigma0j) %*% theta0j}
      theta_iter[[lj]][,i] = mvtnorm::rmvnorm(1,mean = thetaj,sigma = Vj)
      Hj = ks::invvec(theta_iter[[lj]][,i],nrow = k,ncol = etaj[lj])
      Sj = (Yj - Hj %*% Wj) %*% t(Yj - Hj %*% Wj)
      sigma_iter[[lj]][[i]] = MCMCpack::riwish(v = Nj + nu0j,S = Sj + S0j)
    }
    #Actualizacion de las matrices espacio y estado
    for (lj in 1:l) {
      listmatrix = state_space(lj, i, theta_iter,sigma_iter)
      R_zt[[lj]] = listmatrix$R
      L_zt[[lj]] = listmatrix$L
      H_zt[[lj]] = listmatrix$H
      K_zt[[lj]] = listmatrix$K
      M_zt[[lj]] = listmatrix$M
    }
    utils::setTxtProgressBar(pb,i)
  }
  close(pb)
  message('Saving results ... \n')
  # exits
  # names
  Names_Yt = paste0("(",1:N,",",1,")")
  if (k > 1) {
    for (i in 2:k) {
      Names_Yt = rbind(Names_Yt,paste0("(",1:N,",",k,")"))
    }
  }
  Names_Zt = paste0("(",1:N,",",1,")")
  if (nu != 0) {
    Names_Xt = paste0("(",1:N,",",1,")")
    if (nu > 1) {
      for (i in 2:nu) {
        Names_Xt = rbind(Names_Xt,paste0("(",1:N,",",k,")"))
      }
    }
  }else{Names_Xt = NULL}
  Names_Ut = rbind(Names_Zt,Names_Xt)
  # Table of estimations
  Yt_chains = Yt_iter[,-c(1:burn)]
  if (nu == 0) {Ut_chains = as.matrix(Ut_iter[,-c(1:burn)])
  }else{Ut_chains = Ut_iter[,-c(1:burn)]}
  Test_Yt = matrix(nrow = nrow(Yt_iter),ncol = 3)
  Test_Ut = matrix(nrow = nrow(Ut_iter),ncol = 3)
  colnames(Test_Yt) = colnames(Test_Ut) =  c(paste('lower limit ',(1 - level)/2*100,'%',sep = ''),'mean',paste('upper limit ',(1 + level)/2*100,'%',sep = ''))
  Test_Yt[,1] = apply(Yt_chains,1,stats::quantile,probs = (1 - level)/2)
  Test_Yt[,3] = apply(Yt_chains,1,stats::quantile,probs = (1 + level)/2)
  est_Yt = apply(Yt_chains,1,mean)
  Test_Yt[,2] = est_Yt
  rownames(Test_Yt) = Names_Yt[PosNAMat[[1]]]

  Test_Ut[,1] = apply(Ut_chains,1,stats::quantile,probs = (1 - level)/2)
  Test_Ut[,3] = apply(Ut_chains,1,stats::quantile,probs = (1 + level)/2)
  est_Ut = apply(Ut_chains,1,mean)
  Test_Ut[,2] = est_Ut
  rownames(Test_Ut) = Names_Ut[PosNAMat[[2]]]

  if (nu == 0) {
    Test_Zt = matrix(nrow = length(Names_Zt[PosNAMat[[2]]]),ncol = 3)
  }else{
    Test_Zt = matrix(nrow = length(Names_Zt[PosNAMat[[2]][1,]]),ncol = 3)
  }
  colnames(Test_Zt) =  c(paste('lower limit ',(1 - level)/2*100,'%',sep = ''),'mean',paste('upper limit ',(1 + level)/2*100,'%',sep = ''))
  if (nu == 0) {
    tab_name_Zt = Names_Zt[PosNAMat[[2]]]
  }else{
    tab_name_Zt = Names_Zt[PosNAMat[[2]][1,]]
  }
  Test_Zt[,1] = Test_Ut[tab_name_Zt,1]
  Test_Zt[,2] = Test_Ut[tab_name_Zt,2]
  Test_Zt[,3] = Test_Ut[tab_name_Zt,3]
  rownames(Test_Zt) = tab_name_Zt
  if (nu != 0) {
    Test_Xt = matrix(nrow = length(Names_Xt[PosNAMat[[2]][-1,]]),ncol = 3)
    colnames(Test_Xt) =  c(paste('lower limit ',(1 - level)/2*100,'%',sep = ''),'mean',paste('upper limit ',(1 + level)/2*100,'%',sep = ''))
    tab_name_Xt = Names_Xt[PosNAMat[[2]][-1,]]
    Test_Xt[,1] = Test_Ut[tab_name_Xt,1]
    Test_Xt[,2] = Test_Ut[tab_name_Xt,1]
    Test_Xt[,3] = Test_Ut[tab_name_Xt,1]
    rownames(Test_Xt) = tab_name_Xt
  }
  ini_obj$tsregime_obj$Yt[PosNAvec[[1]],] = matrix(Test_Yt[,2],ncol = k,byrow = T)
  ini_obj$tsregime_obj$Zt[PosNAvec[[2]],] = matrix(Test_Ut[,2],ncol = nu + 1,byrow = T)[,1]
  ini_obj$tsregime_obj$Xt[PosNAvec[[2]],] = matrix(Test_Ut[,2],ncol = nu + 1,byrow = T)[,-1]
  if (chain) {Chains = vector('list')}
  if (any(is.na(Yt)) & any(is.na(Zt)) & any(is.na(Xt))) {
    estimates = list(Yt = Test_Yt, Zt = Test_Zt, Xt = Test_Xt)
    if (chain) {
      Chains$Yt = Yt_chains
      Chains$Zt = Ut_chains[1:sum(PosNAMat[[2]][1,]),]
      Chains$Xt = Ut_chains[-c(1:sum(PosNAMat[[2]][1,])),]
    }
  }else if (any(is.na(Yt)) & any(is.na(Zt)) & !any(is.na(Xt))) {
    estimates = list(Yt = Test_Yt, Zt = Test_Zt)
    if (chain) {
      Chains$Yt = Yt_chains
      Chains$Zt = Ut_chains
    }
  }else if (any(is.na(Yt)) & !any(is.na(Zt)) & any(is.na(Xt))) {
    estimates = list(Yt = Test_Yt, Xt = Test_Xt)
    if (chain) {
      Chains$Yt = Yt_chains
      Chains$Xt = Ut_chains
    }
  }else if (!any(is.na(Yt)) & any(is.na(Zt)) & !any(is.na(Xt))) {
    estimates = list(Zt = Test_Zt, Xt = Test_Xt)
    if (chain) {
      Chains$Zt = Ut_chains[1:sum(PosNAMat[[2]][1,]),]
      Chains$Xt = Ut_chains[-c(1:sum(PosNAMat[[2]][1,])),]
    }
  }else if (any(is.na(Yt)) & !any(is.na(Zt)) & !any(is.na(Xt))) {
    estimates = list(Yt = Test_Yt)
    if (chain) {
      Chains$Yt = Yt_chains
    }
  }else if (!any(is.na(Yt)) & any(is.na(Zt)) & !any(is.na(Xt))) {
    estimates = list(Zt = Test_Zt)
    if (chain) {
      Chains$Zt = Ut_chains
    }
  }else if (!any(is.na(Yt)) & !any(is.na(Zt)) & any(is.na(Xt))) {
    estimates = list(Xt = Test_Xt)
    if (chain) {
      Chains$Xt = Ut_chains
    }
  }
  compiler::enableJIT(0)
  if (chain) {
    result = list(tsregime = ini_obj$tsregime_obj, estimates = estimates, Chains = Chains)
  }else{
    result = list(tsregime = ini_obj$tsregime_obj, estimates = estimates)
  }
  class(result) = 'regime_missing'
  return(result)
}
