#' @import stats

########################################################
########   Median bootstrap to standard error   ########
########################################################
med_se = function(x,B){
  B_median = rep(0,B)
  n = length(x)
  for (i in 1:B) {
    id = sample(1:n,n,replace=T)
    B_median[i] = median(x[id])
  }
  return(sd(B_median)/sqrt(B))
}


tnorm <- function(x){
  s=sum(as.vector(x)*as.vector(x))
  return(s)
}


tinner = function(A,B){
  s = sum(as.vector(A)*as.vector(B))
  return(s)
}


# cluster_err = function(K, idx, id_est) {
#   # K is number of clusters
#   # id_est is the estimate label
#   # idx is the true label
#   # return error rate
# 
#   perK = combinat::permn(K)
# 
#   n = length(idx)
# 
#   K_pred = perK[[1]]
#   id_pred = K_pred[id_est]
#   for(t in 2:length(perK)) {
#     K_now = perK[[t]]
#     id_now = K_now[id_est]
#     if(sum(id_now!=idx)<sum(id_pred!=idx)){
#       K_pred = K_now
#       id_pred = id_now
#     }
#   }
# 
#   id_err = sum(id_pred!=idx)/n*100
# 
#   return(list(cluster_err=id_err, K_pred=K_pred, id_pred=id_pred))
# }
# 
# 
# mkronecker = function(X){
#   #X is a list of matrix to do kronecker product
# 
#   M = length(X)
#   KronX = X[[M]]
# 
#   if(M!=1){
#     for(m in (M-1):1){
#       KronX = kronecker(KronX,X[[m]])
#     }
#   }
#   return(KronX)
# }


krondet = function(X,log=TRUE){
  #X is a list of matrices
  #Calculate the determinant of Kronecker product of X

  M = length(X)
  dimen = sapply(X, ncol)
  p = prod(dimen)

  logdet = log(sapply(X, det))
  mydet = p*sum(logdet/dimen)

  if(log){
    return(mydet)
  }
  else{
    return(exp(mydet))
  }
}



tensrloglk = function(X, espi, Mu, SIG){
  #calculate observed loglikelihood
  #X is a list of observed tensor data
  #espi is estimate weight
  #Mu is a list of estimate cluster mean, of length K
  #SIG is a list of estimate covariance matrices, of length M

  n = length(X)
  dimen = dim(X[[1]])
  M = length(dimen)
  p = prod(dimen)
  K = length(Mu)

  SIGinv = lapply(SIG, MASS::ginv)
  Siginv = mkronecker(SIGinv)
  logSIGdet = krondet(SIG,log=TRUE)

  B = array(list(),K-1)
  for (k in 2:K) {
    B[[k-1]] = tensr::atrans(Mu[[k]]-Mu[[1]], SIGinv)
  }

  loglk = 0
  for (i in 1:n){
    x_mu1 = matrix(X[[i]]-Mu[[1]],ncol=1)
    dis_mu1 =  t(x_mu1) %*% Siginv %*% x_mu1

    logf1 = -p*log(2*pi)/2 - logSIGdet/2 - dis_mu1/2

    for (k in 2:K){
      temp = espi[1]
      logfkoverf1 = tinner(B[[k-1]], X[[i]]-(Mu[[k]]+Mu[[1]])/2)
      fkoverf1 = exp(logfkoverf1)
      temp = temp + espi[k]*fkoverf1
    }
    loglk = loglk+log(temp)+logf1
  }

  return(loglk)
}


distortion <- function(x, y, K){
  n=length(y)
  muall=array(0,dim=dim(x[[1]]))
  for (i in 1:n){
    muall=muall+x[[i]]
  }
  muall=muall/n

  mu=array(list(),K)
  n.fit=rep(0,K)
  for (i in 1:K){
    mu[[i]]=array(0,dim=dim(x[[1]]))
  }
  SSb=0
  for (i in 1:n){
    mu[[y[i]]]=mu[[y[i]]]+x[[i]]
    n.fit[y[i]]=n.fit[y[i]]+1
    SSb=SSb+tnorm(x[[i]]-muall)
  }
  for (i in 1:K){
    mu[[i]]=mu[[i]]/n.fit[i]
  }
  SSw=0
  for (i in 1:n){
    SSw=SSw+tnorm(x[[i]]-mu[[y[i]]])
  }
  SSb=SSb-SSw
  dist=SSw/SSb

}
