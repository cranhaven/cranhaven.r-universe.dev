#' @export
#' @import MASS abind tensr TRES
#' @useDynLib TensorClustering, .registration=TRUE

DEEM = function(X, nclass, niter = 100, lambda = NULL, dfmax = n, pmax = nvars, pf = rep(1, nvars),
                eps = 1e-04, maxit = 1e+05, sml = 1e-06, verbose = FALSE, ceps = 0.1,
                initial = TRUE, vec_x = NULL){
  
  if (is.null(lambda)){
    stop("Parameter lambda")
  }
  
  n = length(X)
  dimen = dim(X[[1]])
  M = length(dimen)
  nvars = prod(dimen)
  
  X_T = abind(X,along=M+1)
  
  if (is.null(vec_x)){
    vec_x = sapply(X,as.vector)
    vec_x = t(vec_x)
  }
  
  cov_temp = list(array(),M)
  for (m in 1:M) {
    dim_m = (1:M)[-m]
    cov_temp[[m]] = ttt(rTensor::as.tensor(X_T), rTensor::as.tensor(X_T), c(dim_m,M+1))@data
  }
  
  
  
  #initialize
  if (initial==TRUE) {
    obj = kmeans(vec_x,nclass)
    y = obj$cluster
    mu = array(list(),nclass)
    for (i in 1:nclass) {
      mu[[i]] = array(0,dim = dimen)
    }
    for (i in 1:n) {
      mu[[y[i]]] = mu[[y[i]]] + X[[i]]
    }
    for (i in 1:nclass) {
      mu[[i]] = mu[[i]]/sum(y==i)
    }
    
  }else{
    #initialize mean
    mu = array(list(),nclass)
    idx = sample(n,nclass,replace = FALSE)
    
    for (i in 1:nclass){
      mu[[i]] = X[[idx[i]]]
    }
    
    #calculate distance and assign class
    dist = matrix(0,n,nclass)
    for (i in 1:n) {
      for (j in 1:nclass) {
        dist[i,j] = tnorm(X[[i]] - mu[[j]])
      }
    }
    y = apply(dist,1,which.min)
  }
  
  espi = rep(0,nclass)
  for (i in 1:nclass){
    espi[i] = sum(y==i)/n
  }
  
  
  
  #initialize covariance
  es_sigma = array(list(),M)
  invsigma = array(list(),M)
  for (i in 1:M){
    es_sigma[[i]] = diag(dimen[i])
    invsigma[[i]] = ginv(es_sigma[[i]])
  }
  
  for (icov in 1:1){
    old_invsigma = invsigma
    es_sigma = array(list(),M)
    for (m in 1:M){
      invsigmam = array(list(), M)
      es_sigma[[m]] = matrix(0, dimen[m], dimen[m])
      for (i in 1:M){
        if (i!=m){
          invsigmam[[i]] = old_invsigma[[i]]
        }else{
          invsigmam[[i]] = diag(dimen[i])
        }
      }
      varscale = 0
      for (i in 1:n){
        standx = X[[i]] - mu[[y[i]]]
        w = rTensor::ttl(rTensor::as.tensor(standx), invsigmam, ms=c(1:M))@data
        es_sigma[[m]] = es_sigma[[m]] + mat(w, m)%*%t(mat(standx, m))
        varscale = varscale + (standx[1])^2
      }
      varscale = varscale/n
      es_sigma[[m]] = es_sigma[[m]]/(n*nvars/dimen[m])
      #		if (m! = 1){
      #			es_sigma[[m]] = es_sigma[[m]]*dimen[m]/sum(diag(es_sigma[[m]]))
      #		}
      if (m!=1){
        es_sigma[[m]] = es_sigma[[m]]/es_sigma[[m]][1,1]
      }else{
        es_sigma[[1]] = es_sigma[[1]]/es_sigma[[1]][1,1]*varscale
      }
      invsigma[[m]] = ginv(es_sigma[[m]])
    }
  }
  
  
  
  #pre for Fortran
  nk = as.integer(nclass-1)
  dfmax = n
  pmax = nvars
  nvars = as.integer(nvars)
  pf = rep(1,nvars)
  eps = 1e-04
  maxit = 1e+05
  sml = 1e-06
  ulam = lambda
  nlam = as.integer(1)
  pf =  as.double(pf)
  if (length(pf)!=nvars)
    stop("The size of penalty factor must be same as the number of input variables")
  dfmax = as.integer(dfmax*nk)
  pmax =  as.integer(pmax)
  flmin =  as.double(1)
  eps =  as.double(eps)
  maxit =  as.integer(maxit)
  verbose =  as.integer(FALSE)
  sml =  as.double(sml)
  ldim = as.integer(M)
  maxd = max(dimen)
  nzero = 0
  cov_t = 0
  
  #loop
  for (iter in 1:niter){
    muold = mu
    yold = y
    
    ##############
    ### E-step ###
    ##############
    
    #estimate beta
    delta = matrix(0,nclass-1,nvars)
    for (k in 1:(nclass-1)){
      delta[k,] = matrix((mu[[k+1]]-mu[[1]]), nrow=1)
    }
    sigma = matrix(0,M,maxd*maxd)
    for (i in 1:M){
      tmp= matrix(0, maxd, maxd)
      tmp[1:dimen[i], 1:dimen[i]] = es_sigma[[i]]
      sigma[i,] = as.vector(tmp)
    }
    
    fit =  .Fortran("clustertensor", obj = double(nlam), nk, nvars, ldim, dimen,maxd,as.double(sigma), as.double(delta),
                    as.double(pf), dfmax, pmax, nlam, flmin, ulam, eps, maxit, sml, verbose, nalam = integer(1),
                    theta = double(pmax * nk * nlam), itheta = integer(pmax), ntheta = integer(nlam),
                    alam = double(nlam), npass = integer(1), jerr = integer(1))
    beta = matrix(fit$theta, pmax, nk, byrow=TRUE)
    
    nzero = length(which(beta[,1]!=0)/(nclass-1)) + nzero
    
    
    gamma = matrix(0, n, nclass)
    vec_mu = sapply(mu,as.vector)
    vec_mu = t(vec_mu)
    
    inexpterm = matrix(0,n,nclass)
    for (k in 2:nclass){
      tmp = 0.5*(vec_mu[k,] + vec_mu[1,])
      for (i in 1:n){
        inexpterm[i,k] = (matrix(vec_x[i,],nrow=1)-matrix(tmp,nrow=1))%*%matrix(beta[,k-1],ncol=1)
      }
    }
    expterm = exp(inexpterm)
    
    for (i in 1:n){
      den = espi[1] + sum(expterm[i,2:nclass]*espi[2:nclass])
      gamma[i,1] = espi[1]/den
      for (k in 2:nclass){
        gamma[i,k] = espi[k]*expterm[i,k]/den
      }
    }
    
    #update pi
    espi = apply(gamma,2,sum)/n
    
    ##############
    ### M-step ###
    ##############
    
    #Mean update
    mu = array(list(),nclass)
    for (k in 1:nclass){
      mu[[k]] = array(0, dim=dimen)
      for (i in 1:n){
        mu[[k]] = mu[[k]] + gamma[i,k]*X[[i]]
      }
      mu[[k]] = mu[[k]]/apply(gamma,2,sum)[k]
    }
    
    
    
    #Covariance update
    t1 = Sys.time()
    for (m in 1:M){
      es_sigma[[m]] = matrix(0, dimen[m], dimen[m])
      for (k in 1:nclass){
        es_sigma[[m]] = es_sigma[[m]] + n*espi[k]*mat(mu[[k]],m)%*%t(mat(mu[[k]],m))
      }
      es_sigma[[m]] = cov_temp[[m]] - es_sigma[[m]]
      es_sigma[[m]] = es_sigma[[m]]/(n*prod(dimen)/dimen[m])
    }
    
    scale1 = 0
    for (i in 1:n){
      for (k in 1:nclass){
        scale1 = scale1 + gamma[i,k]*(X[[i]][1]-mu[[k]][1])^2
      }
    }
    scale1 = scale1/(n*es_sigma[[1]][1])
    es_sigma[[1]] = scale1*es_sigma[[1]]
    for (m in 2:M){
      scale = es_sigma[[m]][1]
      es_sigma[[m]] = es_sigma[[m]]/scale
    }
    cov_t = cov_t + difftime(Sys.time(), t1, units = "secs")
    
    
    #check convergence
    s = 0
    for(i in 1:nclass){
      s = s + tnorm(mu[[i]]-muold[[i]])
    }
    
    if (s<ceps) break
  }
  
  
  nzero = nzero/iter
  #output
  y = apply(gamma,1,which.max)
  outlist = list(pi=espi, mu=mu, sigma=es_sigma, gamma=gamma,
                 y=y, iter=iter, df=nzero, beta=beta)
}

