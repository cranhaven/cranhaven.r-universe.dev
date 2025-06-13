#==================================================================================================#
# Date:
# Description:
# Coments:
#-> dependent function of mvtnorm library.
#-> In the simulation the first values of Ut are considered zeros and the first values
#-  of Yt as normal noises. Additionally a burn of 100.
#-> hacer comentario respecto a los casos donde explota y aclarar que no se evalua 'estabilidad'
# No evaluamos que Ut cumpla las propiedades de ser proceso markov
# Function:
#==================================================================================================#
mtarsim = function(N, Rg, r = NULL, Xt = NULL, Zt = NULL, seed = NULL){
  burn = 1000
  if (!{round(N) == N & N > 1}) {stop('N must be an integer greater than 1')}
  if (!is.null(Zt)) {
    if (!is.numeric(Zt)) {stop('Zt must be a real matrix of dimension Nx1')}
    if (!is.matrix(Zt)) {Zt = as.matrix(Zt)}
    if (nrow(Zt) != N) {stop('Zt and Yt number of rows must match')}
    Zt = t(Zt)
  }
  if (!is.null(Xt)) {
    if (!is.numeric(Xt)) {stop('Xt must be a real matrix of dimension Nx(nu+1)')}
    if (!is.matrix(Xt)) {Xt = as.matrix(Xt)}
    if (nrow(Xt) != N) {stop('Xt and Yt number of rows must match')}
    Xt = t(Xt)
  }
  Ut = rbind(Zt,Xt)
  if (is.null(Ut)) {nu = 0}else{nu = nrow(Ut) - 1}
  k = nrow(Rg[[1]]$sigma)
  l = length(Rg)
  if (l == 1) {
    rj = matrix(c(-Inf,Inf),nrow = 2,ncol = l)
    if (is.null(Ut)) {
      Ut = matrix(0, ncol = N,nrow = 1)
    }else{
      Ut = rbind(matrix(0, ncol = N,nrow = 1),Xt) # only for covariable
    }
  }
  # Validations
  if (!is.list(Rg)) {stop('Rg must be a list type object with objects of class regime')}
  if (ncol(Ut) != N | !is.numeric(Ut) | !is.matrix(Ut)) {
    stop(paste0('Ut must be a matrix of dimension ',N,'x',nu + 1))}
  for (i in 1:l) {
    if (class(Rg[[i]]) != 'regime') {stop('Rg must be a list of objects of class regime')}
  }
  if (l >= 2) {
    if (length(r) < 1 | length(r) != (l - 1) | !is.numeric(r) | is.null(r)) {
      stop(paste('r must be a numeric vector of length',length(Rg) - 1))}else{
      if (l > 2) {for (i in 1:{l - 2}) {
        if (r[i] >= r[i + 1]) {stop('r[i] must be smaller than r[i+1]')}}
      }
    }
  }
  # values by regime
  pj = qj = dj = vector('numeric')
  for (i in 1:l) {
    pj[i] = length(Rg[[i]]$phi)
    qj[i] = length(Rg[[i]]$beta)
    dj[i] = length(Rg[[i]]$delta)
  }
  # create intervals
  if (l != 1) {
    rj = matrix(nrow = 2,ncol = l)
    rj[,1] = c(-Inf,r[1])
    rj[,l] = c(rev(r)[1],Inf)
    if (l > 2) {
      for (i in 2:{l - 1}) {rj[,i] = c(r[i - 1],r[i])}
    }
  }
  # initial Vectors
  maxj = max(pj,qj,dj)
  Yt = matrix(0,nrow = k,ncol = N + maxj + burn)
  if (!is.null(seed)) {set.seed(seed)}
  et = t(mvtnorm::rmvnorm(N + maxj + burn,mean = rep(0,k),sigma = diag(k)))
  Yt[,1:(maxj + burn)] = et[,1:(maxj + burn)]
  Zt = c(rep(0,maxj + burn),Ut[1,])
  if (nu == 0) {
    Xt = matrix(0,ncol = N + maxj + burn,nrow = 1)
  }else{
    Xt = cbind(rep(0,nu) %x% matrix(1,ncol = maxj + burn),matrix(Ut[-1,],nrow = nu))
  }
  # iterations of the simulation
  for (i in (maxj + burn + 1):(N + maxj + burn)) {
    ## evaluate regime
    for (w in 1:l) {
      if (Zt[i] > rj[1,w] & Zt[i] <= rj[2,w]) {Ri = Rg[[w]]}
    }
    ## calculate from according regime selected
    p = length(Ri$phi)
    q = length(Ri$beta)
    d = length(Ri$delta)
    ## create matrices
    cs = Ri$cs
    At = as.matrix(as.data.frame(Ri$phi))
    if (q != 0) {
      Bt = as.matrix(as.data.frame(Ri$beta))
    }else{Bt = matrix(0,nrow = k,ncol = 1)}
    if (d != 0) {
      Dt = as.matrix(as.data.frame(Ri$delta))
    }else{Dt = matrix(0,nrow = k,ncol = 1)}
    Sig = as.matrix(Ri$sigma)
    ## make lags and calculate
    yti = c()
    for (w in 1:p) {yti = c(yti,Yt[,i - w])}
    xti = c()
    if (l == 1 & nrow(Ut) != 1) {
      xti = c(xti,Xt[,i])
    }else{
      for (w in 1:ifelse(q == 0,1,q)) {xti = c(xti,Xt[,i - w])}
    }
    zti = c()
    for (w in 1:ifelse(d == 0,1,d)) {zti = c(zti,Zt[i - w])}
    Yt[,i] = cs + At %*% yti + Bt %*% xti + Dt %*% zti + Sig %*% et[,i]
  }
  # delete burn
  if (k == 1) {
    Yt = as.matrix(Yt[-(1:{maxj + burn})])
  }else{
    Yt = t(Yt[,-(1:{maxj + burn})])
  }
  Zt = Zt[-c(1:{maxj + burn})]
  if (nu == 1) {
    Xt = as.matrix(Xt[,-c(1:{maxj + burn})])
  }else{
    Xt = t(Xt[,-c(1:{maxj + burn})])
  }
  if (sum(Xt) != 0 & sum(Zt) != 0) {
    sim = tsregime(Yt = Yt,Xt = Xt,Zt = Zt,r = r)
  }else if (sum(Xt) == 0 & sum(Zt) != 0) {
    sim = tsregime(Yt = Yt,Zt = Zt,r = r)
  }else if (sum(Zt) == 0 & sum(Xt) != 0) {
    sim = tsregime(Yt = Yt,Xt = Xt)
  }else if (sum(Zt) == 0 & sum(Xt) == 0) {
    sim = tsregime(Yt = Yt)
  }
  List_RS = list(Sim = sim, Reg = Rg,pj = pj,qj = qj,dj = dj)
  class(List_RS) = 'mtarsim'
  return(List_RS)
}
