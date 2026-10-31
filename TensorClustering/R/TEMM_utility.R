tinner = function(A,B){
  s = sum(as.vector(A)*as.vector(B))
  return(s)
}


cluster_err = function(K, idx, id_est) {
  # K is number of clusters
  # id_est is the estimate label
  # idx is the true label
  # return error rate

  perK = combinat::permn(K)

  n = length(idx)

  K_pred = perK[[1]]
  id_pred = K_pred[id_est]
  for(t in 2:length(perK)) {
    K_now = perK[[t]]
    id_now = K_now[id_est]
    if(sum(id_now!=idx)<sum(id_pred!=idx)){
      K_pred = K_now
      id_pred = id_now
    }
  }

  id_err = sum(id_pred!=idx)/n*100

  return(list(cluster_err=id_err, K_pred=K_pred, id_pred=id_pred))
}



mu_err = function(Mu,mu_est,K,idx,id_est){
  #Mu is a list of tensor centroids
  #mu_est is a matrix of estimated centroids, every row is a centroid
  #idx is true label for each data point
  #id_est is the estimate lable
  mu_err = 0
  mu = t(sapply(Mu,as.vector))
  K_pred = cluster_err(K,idx,id_est)$K_pred
  for(k in 1:K){
    err_temp = as.matrix(mu[K_pred[k],] - mu_est[k,])
    mu_err = mu_err + norm(err_temp,type="F")
  }
  #mu_err1 = norm(mu[K_pred,]-mu_est, type="F")

  return(list(mu_err=mu_err))
}



mkronecker = function(X){
  #X is a list of matrices for kronecker product

  M = length(X)
  KronX = X[[M]]

  if(M!=1){
    for(m in (M-1):1){
      KronX = kronecker(KronX,X[[m]])
    }
  }
  return(KronX)
}



logMixTenGau = function(Xm, pi, eta, Mu, SIG){
  #calculate observed loglikelihood
  #Each column of Xm is a vectorized observed tensor data
  #pi is estimate weight
  #eta is estimate probability of Xi belong to class k
  #Mu is a list of estimate cluster mean, of length K
  #SIG is a list of estimate covariance matrices, of length M

  n = ncol(Xm)
  K = length(Mu)
  sigma = mkronecker(SIG)

  loglk1 = mvtnorm::dmvnorm(t(Xm), mean=as.vector(Mu[[1]]), sigma=sigma, log=TRUE)

  loglk = -sum(log(eta[,1])) + n*log(pi[1]) + sum(loglk1)
  return(loglk)
}





