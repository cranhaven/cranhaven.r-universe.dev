delta.est = function(S.hat.A, Omega.hat0, lam1){
  # delta.est: the function estimating divergence matrix (Delta)
  pm = dim(S.hat.A)[1]
  B.hat = Omega.hat0 %*% S.hat.A - diag(pm)
  Z = array(rep(0, 2*pm^2), dim=c(pm,pm,2))
  Z[,,2] = abs(B.hat) - lam1
  delta.hat = sign(B.hat) * apply(Z, 1:2, max)
  return(delta.hat)
}

Initial_GGM = function(t.data=NULL, A.data=NULL, precision.method="glasso",
                       correlation=F, preselect.aux=0, sel.type="L1",
                       input.A.cov=F, A.cov=NULL, nA.vec=NULL, t.Theta.hat0=NULL, t.n=NULL){
  if(is.null(t.Theta.hat0)){
    p = dim(t.data)[2]
    n = dim(t.data)[1]
    if(precision.method=="glasso"){
      Theta.hat0 = huge::huge(t.data, lambda = 0.5*sqrt(log(max(p,n))/n), method = "glasso", verbose = FALSE)$icov[[1]]
    }
    if(precision.method=="CLIME"){
      Theta.hat0 = clime::clime(t.data,lambda=sqrt(log(max(p,n))/n))$Omegalist[[1]]
      # Theta.hat0 = huge::huge(t.data, lambda = 0.5*sqrt(log(max(p,n))/n), method = "glasso", verbose = FALSE)$icov[[1]]
      # warning("Note: CLIME cannot be used for initialization due to the removal of R package 'fastclime' from the CRAN repository. Initialization is still based on glasso.")
    }
  }
  if(!is.null(t.Theta.hat0)){
    p = dim(t.Theta.hat0)[1]
    n = t.n
    Theta.hat0 = t.Theta.hat0
  }


  if(!input.A.cov){
    K = length(A.data)
    nA.vec = rep(0, K)
  } else {
    K = length(A.cov)
    if(is.null(nA.vec)){warning("The auxiliary sample size is missing!")}
  }

  if(!correlation){
    A.S.hat.list0 = list()
    for (k in 1:K) {
      if(!input.A.cov){A.S.hat.list0[[k]] = cov(A.data[[k]]); nA.vec[k] = dim(A.data[[k]])[1]}
      if(input.A.cov){A.S.hat.list0[[k]] = A.cov[[k]]; nA.vec = nA.vec }
    }
    A.S.hat.list00 = A.S.hat.list0
    nA.vec00 = nA.vec

    ## pre-select informative auxiliary domains, if preselect.aux > 0
    delta.vec = rep(0,K)
    if(preselect.aux > 0){
      s = max(apply(Theta.hat0, 2, function(x) sum(x!=0) ))
      lam.preaux = preselect.aux * s * sqrt(log(p)/n)
      if(sel.type=="L2"){
        s = sum(Theta.hat0!=0)
        lam.preaux = preselect.aux * s * log(p)/n
      }
      A.S.hat.list0.select = list()
      nA.vec.select = c()
      A.data.select = list()
      A.cov.select = list()
      vk = 1
      for (k in 1:K) {
        delta.k.l1 = max(apply(A.S.hat.list0[[k]] %*% Theta.hat0 - diag(p), 2, function(x) sum(abs(x))))
        if(sel.type=="L2"){
          delta.k.l1 = sum((A.S.hat.list0[[k]] %*% Theta.hat0 - diag(p))^2)
        }
        delta.vec[k] = delta.k.l1
        if(delta.k.l1 < lam.preaux){
          A.S.hat.list0.select[[vk]] = A.S.hat.list0[[k]]
          nA.vec.select[vk] = nA.vec[k]
          if(!input.A.cov){ A.data.select[[vk]] = A.data[[k]]}
          if(input.A.cov){ A.cov.select[[vk]] = A.cov[[k]] }
          vk = vk+1
        }
      }

      A.S.hat.list0 = A.S.hat.list0.select
      nA.vec = nA.vec.select
      K = length(A.S.hat.list0)
      noninfor = F

      if(vk == 1){
        warning("Warning: There is no informative auxiliary domains at the current set threshold!
                  The current output result is based on the target domain only.
                  You may consider to raise the threshold 'preselect.aux'.")
        A.data.select = NULL
        A.cov.select = NULL
        A.S.hat.list0 = A.S.hat.list00
        nA.vec = nA.vec00
        K = length(A.S.hat.list0)
        noninfor = T
      }


    } else {A.data.select = A.data; A.cov.select = A.cov;
            noninfor = F; lam.preaux=NULL}

    # covariance matrix weighted by the sample sizes
    S.hat.A.M.size = diag(p) - diag(p)
    alpha.k = nA.vec/sum(nA.vec)
    for (k in 1:K) {
      S.hat.A.M.size = S.hat.A.M.size + A.S.hat.list0[[k]] * alpha.k[k]
    }

    # covariance matrix weighted by the differences
    S.hat.A.M = diag(p) - diag(p)
    mode.set = c(1)
    S.hat.A.M.diff0 = S.hat.A.M
    weight.KM0 = matrix(0, ncol = K, nrow = length(mode.set))
    for (k in 1:K) {
      for (m in mode.set) {
        mi = match(m, mode.set)
        weight.KM0[mi,k] = 1/sum((A.S.hat.list0[[k]] %*% Theta.hat0 - diag(p))^2)
      }
    }

    wed0 = t(t(weight.KM0)*nA.vec)
    alpha.k.diff0 = wed0 / apply(wed0, 1, sum)
    for (k in 1:K) {
      for (m in mode.set) {
        mi = match(m, mode.set)
        S.hat.A.M.diff0 = S.hat.A.M.diff0 + A.S.hat.list0[[k]] * alpha.k.diff0[mi,k]
      }
    }
    S.hat.A.weight = S.hat.A.M.diff0

    # covariance matrix selected by the differences
    k.check = which.max(weight.KM0)
    S.hat.A.opt = A.S.hat.list0[[k.check]]

  } else {
    S.hat0 = cor(t.data)
    A.S.hat.list0 = list()
    for (k in 1:K) {
      if(!input.A.cov){A.S.hat.list0[[k]] = cor(A.data[[k]]); nA.vec[k] = dim(A.data[[k]])[1]}
      if(input.A.cov){A.S.hat.list0[[k]] = sqrt(diag(diag(A.cov[[k]])^(-1))) %*% A.cov[[k]] %*% sqrt(diag(diag(A.cov[[k]])^(-1)))}
    }

    ## pre-select informative auxiliary domains, if preselect.aux > 0
    delta.vec = rep(0,K)
    if(preselect.aux > 0){
      s = max(apply(Theta.hat0, 2, function(x) sum(x!=0) ))
      lam.preaux = preselect.aux * s * sqrt(log(p)/n)
      if(sel.type=="L2"){
        s = sum(Theta.hat0!=0)
        lam.preaux = preselect.aux * s * log(p)/n
      }
      A.S.hat.list0.select = list()
      nA.vec.select = c()
      A.data.select = A.data
      A.cov.select=A.cov
      vk = 1
      for (k in 1:K) {
        delta.k.l1 = max(apply(A.S.hat.list0[[k]] - S.hat0, 2, function(x) sum(abs(x))))
        if(sel.type=="L2"){
          delta.k.l1 = sum((A.S.hat.list0[[k]] %*% Theta.hat0 - diag(p))^2)
        }
        if(delta.k.l1 < lam.preaux){
          A.S.hat.list0.select[[vk]] = A.S.hat.list0[[k]]
          nA.vec.select[vk] = nA.vec[k]
          if(!input.A.cov){ A.data.select[[vk]] = A.data[[k]]}
          if(input.A.cov){ A.cov.select[[vk]] = A.cov[[k]] }
          vk = vk+1
        }
      }

      A.S.hat.list0 = A.S.hat.list0.select
      K = length(A.S.hat.list0)
      nA.vec = nA.vec.select
      noninfor = F

      if(vk == 1){
        warning("Warning: There is no informative auxiliary domains at the current set threshold!
                  The current output result is based on the target domain only.
                  You may consider to raise the threshold 'preselect.aux'.")
        A.data.select = NULL
        A.cov.select = NULL
        A.S.hat.list0 = A.data
        nA.vec = nA.vec
        K = length(A.S.hat.list0)
        noninfor = T
      }

    } else {A.data.select = A.data; A.cov.select = A.cov;
            noninfor = F; lam.preaux=NULL}

    # covariance matrix weighted by the sample sizes
    S.hat.A.M.size = diag(p) - diag(p)
    alpha.k = nA.vec/sum(nA.vec)
    for (k in 1:K) {
        S.hat.A.M.size = S.hat.A.M.size + A.S.hat.list0[[k]] * alpha.k[k]
    }

    # covariance matrix weighted by the differences
    S.hat.A.M = diag(p) - diag(p)
    mode.set = c(1)
    S.hat.A.M.diff0 = S.hat.A.M
    weight.KM0 = matrix(0, ncol = K, nrow = length(mode.set))
    for (k in 1:K) {
      for (m in mode.set) {
        mi = match(m, mode.set)
        weight.KM0[mi,k] = 1/sum(( A.S.hat.list0[[k]] - S.hat0 )^2)
      }
    }

    wed0 = t(t(weight.KM0)*nA.vec)
    alpha.k.diff0 = wed0 / apply(wed0, 1, sum)
    for (k in 1:K) {
      for (m in mode.set) {
        mi = match(m, mode.set)
        S.hat.A.M.diff0 = S.hat.A.M.diff0 + A.S.hat.list0[[k]] * alpha.k.diff0[mi,k]
      }
    }
    S.hat.A.weight = S.hat.A.M.diff0

    # covariance matrix selected by the differences
    k.check = which.max(weight.KM0)
    S.hat.A.opt = A.S.hat.list0[[k.check]]

  }

  infor.num = c(1:K)[delta.vec < lam.preaux]
  res = list(Theta.hat0 = Theta.hat0, A.data.select=A.data.select, A.cov.select=A.cov.select,
             S.hat.A.size = S.hat.A.M.size,
             S.hat.A.weight = S.hat.A.weight,
             S.hat.A.opt = S.hat.A.opt, k.check = k.check,
             n = n, p = p, N = min(nA.vec), nA.vec=nA.vec, noninfor=noninfor,
             delta.vec=delta.vec, lam.preaux=lam.preaux, infor.num = infor.num)
  return(res)


}

Thetaest.cd = function(S.hat.A, deltaI, lam2, Omega.hat0, max_iter=10, eps=0.001){
  # Thetaest.cd: the function estimating transfer learning-based estimator of
  #              precision matrix via coordinate descent algorithm.
  p = dim(S.hat.A)[1]
  Theta_hat = Omega.hat0
  for (j in 1:p){
    thetaj = Omega.hat0[,j]
    iter = 0
    diff = 10
    while(iter < max_iter && diff > eps){
      thetaj0 = thetaj
      for (i in 1:p){
        Sj = S.hat.A[i,]
        thetaji = deltaI[i,j] - Sj %*% thetaj + Sj[i] * thetaj[i]
        if(i == j){
          thetaj[i] = S_soft(thetaji, 0) / Sj[i]
        }else{
          thetaj[i] = S_soft(thetaji, lam2) / Sj[i]
        }
      }
      diff = sqrt( sum((thetaj - thetaj0)^2) / p )
      iter = iter + 1
    }
    Theta_hat[,j] = thetaj
  }
  return(Theta_hat)
}

Thetaest.admm = function(S.hat.A, deltaI, lam2, Omega.hat0,
                         max_iter=10, eps=1e-3, kappa = 1){
  # Thetaest.cd: the function estimating transfer learning-based estimator of
  #              precision matrix via ADMM algorithm.
  p = dim(S.hat.A)[1]
  SI = S.hat.A+kappa*diag(p)
  Theta_hat = Omega.hat0

  for (j in 1:p){
    thetaj = Omega.hat0[,j]
    v = thetaj
    ej = rep(1,p)
    ej[j] = 0
    gamma = rep(0,p)
    iter = 0
    diff = 10
    while(iter < max_iter && diff > eps){
      thetaj0 = thetaj
      thetaj = as.numeric(solve(SI) %*% ( deltaI[,j] + gamma + kappa*v ))
      v = S_soft.vec(thetaj - gamma/kappa, lam2, ej)
      gamma = gamma + kappa * ( v - thetaj )

      diff = sqrt( sum((thetaj - thetaj0)^2) / p )
      iter = iter + 1
    }
    Theta_hat[,j] = v
  }
  return(Theta_hat)
}

symmetric.mat = function(Omega){
  pm = dim(Omega)[1]
  Z = array(rep(0, 2*pm^2), dim=c(pm,pm,2))
  Z[,,1] = Omega
  Z[,,2] = t(Omega)
  Omega.sym = apply(Z, 1:2, function(x) x[which.min(abs(x))])
  return(Omega.sym)
}

S_soft = function(z,lambda){
  # S_soft: single lasso shrinkage estimate
  norm.z = sqrt(sum(z^2))
  if(norm.z!=0){
    n.x = 1 - lambda/norm.z
    rho = n.x*(n.x > 0)*z
  } else{
    rho = z
  }
  return(rho)
}

S_soft.vec = function(z,lambda,ej=rep(1,length(z))){
  # S_soft.vec: single lasso shrinkage estimate for a vector
  n.z = abs(z) - lambda*ej
  return(sign(z) * (n.z > 0) * n.z)
}

BIC_value = function(S.hat.A, delta.hat, Theta.hat, n=100, adjust=F){
  pm = dim(S.hat.A)[1]
  deltaI = delta.hat + diag(pm)
  fitness = 0.5*sum(diag(t(Theta.hat) %*% S.hat.A %*% Theta.hat)) - sum(diag( t(deltaI) %*%  Theta.hat))
  degree = sum(Theta.hat != 0) - pm
  if(adjust){Cn = log(n*pm)} else {Cn = 1}
  BIC.penalty = Cn*degree*log(n)  / n
  BICvalue = fitness + BIC.penalty

  return(list(BIC=BICvalue, fitness=fitness, BIC.penalty=BIC.penalty,degree=degree))
}
