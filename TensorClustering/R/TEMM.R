#' @export

TEMM = function(Xn, u, K, initial="kmeans", iter.max=500, stop=1e-3, trueY=NULL, print=FALSE){
  #Xn, array type, the tensor data for clustering, dimension of last mode is sample size
  #u is a vector of envelope dimensions
  #K is number of clusters
  #trueY is the true label
  
  dimen = dim(Xn)[-length(dim(Xn))]   #vector of dimensions
  M = length(dimen)
  p = prod(dimen)
  n = dim(Xn)[M+1]   #sample size
  
  ## center data ##
  Xbar = rowMeans(Xn,dims=M)  #sample mean
  Xnl = asplit(Xn,M+1)  #a list of tensor observations
  X = lapply(Xnl,function(X,d){X-d},Xbar)  #centered tensor data, a list of length n
  Xm = sapply(X,as.vector)  #vectorized centered tensor observations
  
  ## initialize cluster labels ##
  if(is.null(initial)){
    id_init = sample(K, n, replace=TRUE)
  }
  else if(initial=="true"){
    id_init = trueY
  }
  else if(initial=="kmeans"){
    init_kmeans = stats::kmeans(t(Xm), centers=K, nstart=20)
    id_init = init_kmeans$cluster
  }
  
  
  ## initialize cluster mean, covariance, B and pi for EM ##
  pi.est = rep(0,K)
  mu.est_old = matrix(0,p,K)
  Xm_mu = matrix(0,p,n)
  B_old = list()
  for(k in 1:K){
    isk = which(id_init==k)
    pi.est[k] = sum(id_init==k)/n
    mu.est_old[,k] = rowMeans(Xm[,isk])
    Xm_mu[,isk] = Xm[,isk] - mu.est_old[,k]  #every data substract it's cluster mean
  }
  Mu_temp = array(mu.est_old,c(dimen,K))
  Mu.est_old = asplit(Mu_temp,M+1)  #a list of length K, each element is a tensor cluster mean
  
  X_subclus = array(Xm_mu,c(dimen,n))
  
  SIG_temp = TRES::kroncov(rTensor::as.tensor(X_subclus))
  SIG.est = SIG_temp$S
  SIG.est[[1]] = SIG.est[[1]]*SIG_temp$lambda
  SIGinv.est = lapply(SIG.est,MASS::ginv)
  SIGinvhalf = list()
  for (m in 1:M) {
    SIGinvhalf[[m]] = pracma::sqrtm(SIG.est[[m]])$Binv
  }
  
  for(k in 2:K){
    Mu_dif = Mu.est_old[[k]]-Mu.est_old[[1]]
    B_old[[k-1]] = rTensor::ttl(rTensor::as.tensor(Mu_dif), SIGinv.est, ms=c(1:M))@data
  }
  
  
  ## Env-EM ##
  eta.est = matrix(0,n,K)
  rp = matrix(1,n,K)  #the ratio of prabability Xi belong to k and 1
  #results = NULL
  PGamma_old = list()  #in order to compare subspace
  for(m in 1:M){
    PGamma_old[[m]] = diag(dimen[m])
  }
  
  for(iter in 1:iter.max){
    t0 = Sys.time()
    
    ## E-step: calculate B_k and eta.est ##
    for(i in 1:n){
      for(k in 2:K){
        X_temp = X[[i]] - (Mu.est_old[[k]]+Mu.est_old[[1]])/2
        logrp = log(pi.est[k]/pi.est[1]) +
          rTensor::innerProd(rTensor::as.tensor(B_old[[k-1]]),
                             rTensor::as.tensor(X_temp))
        rp[i,k] = exp(logrp)
      }
      eta.est[i,1] = 1/sum(rp[i,])
      for(k in 2:K){
        eta.est[i,k] = eta.est[i,1]*rp[i,k]
      }
    }
    #eta.est[which(is.na(eta.est))] = 1
    
    
    ## M-step ##
    pi.est = colMeans(eta.est)
    mutil_temp = Xm %*% eta.est
    mutil = t(t(mutil_temp)/colSums(eta.est))   # vectorized \tilde{\mu} in notes
    Mutil_temp = array(mutil,c(dimen,K))
    Mutil = asplit(Mutil_temp,M+1)  # \tilde{\mu} in notes, a list of length K
    
    
    sub.est = estGamma(X=abind::abind(X,along=M+1), Mutil=Mutil_temp, PGamma=PGamma_old,
                       SIG=SIG.est, SIGinvhalf=SIGinvhalf, eta=eta.est, u=u)
    
    Gamma.est = sub.est$Gamma_est
    PGamma_new = sub.est$PGamma_est
    SIG.est = sub.est$SIG_est
    SIGinv.est = sub.est$SIGinv_est
    SIGinvhalf = sub.est$SIGinvhalf
    
    Mu.est_new = B_new = list()
    for(k in 1:K) {
      Mu.est_new[[k]] = rTensor::ttl(rTensor::as.tensor(Mutil[[k]]), PGamma_new, ms=c(1:M))@data
      if(k>1){
        Mu_dif = Mu.est_new[[k]] - Mu.est_new[[1]]
        B_new[[k-1]] = rTensor::ttl(rTensor::as.tensor(Mu_dif), SIGinv.est, ms=c(1:M))@data
      }
    }
    
    
    Mu_err = tensr::fnorm(abind::abind(Mu.est_new)-abind::abind(Mu.est_old))/
      tensr::fnorm(abind::abind(Mu.est_old))
    #B_err = tensr::fnorm(abind(B_new)-abind(B_old))/tensr::fnorm(abind(B_old))
    id_env = apply(eta.est, 1, which.max)
    
    #subspace distance
    #Gamma_err = norm(mkronecker(PGamma_new)-mkronecker(PGamma_old),type='F')/sqrt(2*prod(u))
    #compute log-likelihood function
    #loglk = logMixTenGau(Xm, pi.est, eta.est, Mu.est_new, SIG.est)
    #results = rbind(results,c(iter,Mu_err,B_err,Gamma_err,loglk,id_err))
    #colnames(results) = c("iter","mean_err","B_err","Gamma_err","loglike","id_err")
    
    t_iter = difftime(Sys.time(), t0, units="secs")
    
    if (print) {
      if(is.null(trueY)){
        cat(paste0("iter",iter),Mu_err,"\n",sep=" ")
      } else {
        id_err = cluster_err(K,trueY,id_env)$cluster_err
        cat(paste0("iter",iter),Mu_err,id_err,"\n",sep=" ")
      }
    }

    
    if(Mu_err<stop){
      break
    } else {
      Mu.est_old = Mu.est_new
      B_old = B_new
      PGamma_old = PGamma_new
    }
  }
  
  Mu.est = list()
  for (k in 1:K) {
    Mu.est[[k]] = Mu.est_new[[k]] + Xbar
  }
  
  #estimate cluster lables
  #id_env = apply(eta.est, 1, which.max)
  return(list(id=id_env, pi=pi.est, eta=eta.est,
              Mu.est=Mu.est, SIG.est=SIG.est,
              Mm=sub.est$Mm, Nm=sub.est$Nm,
              Gamma.est=Gamma.est, PGamma.est=PGamma_new))
}






















