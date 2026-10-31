#' @export

tune_u_joint = function(u_candi, K, X, iter.max=500, stop=1e-3, trueY=NULL){
  ## u_candi is a list of candidate evelope dimension
  dimen = dim(X)[-length(dim(X))]
  dim_u = sapply(u_candi, length)
  
  p = prod(dimen)
  n = dim(X)[length(dim(X))]
  M = length(dim(X))-1

  Xnl = asplit(X,M+1)
  Xm = sapply(Xnl,as.vector)
  
  opt.bic = 1e9
  opt.u = rep(0,M)
  # bic = matrix(0,dim_u[1],dim_u[2])
  # err = matrix(0,dim_u[1],dim_u[2])
  
  # if(M==2){
  #   opt.bic = 1e9
  #   opt.u = rep(0,M)
  #   # bic = matrix(0,dim_u[1],dim_u[2])
  #   # err = matrix(0,dim_u[1],dim_u[2])
  #   
  #   for(i in 1:dim_u[1]) {
  #     for (j in 1:dim_u[2]) {
  #       u_now = c(u_candi[[1]][i],u_candi[[2]][j])
  #       Ku = (K-1)*prod(u_now) + sum(dimen*(dimen+1))/2
  #       env = TEMM(Xn=X, u=u_now, K=K, initial="kmeans", iter.max=iter.max, trueY=trueY)
  #       loglk = logMixTenGau(Xm, env$pi, env$eta, env$Mu.est, env$SIG.est)
  #       
  #       # err[i,j] = cluster_err(K,Y,env$id)$cluster_err
  #       # bic[i,j] = -2*loglk + log(n)*Ku
  #       bic_now = -2*loglk + log(n)*Ku
  #       
  #       if(bic_now<opt.bic){
  #         opt.bic = bic_now
  #         opt.u[1] = u_candi[[1]][i]
  #         opt.u[2] = u_candi[[2]][j]
  #         opt.id = env$id
  #         opt.Mu = env$Mu.est
  #       }
  #     }
  #   }
  # }
  
  
  
  for(i in 1:prod(dim_u)) {
    
    u_ind = as.vector(arrayInd(i, dim_u))
    u_now = rep(0,M)
    for (m in 1:M) {
      u_now[m] = u_candi[[m]][u_ind[m]]
    }
    
    Ku = (K-1)*prod(u_now) + sum(dimen*(dimen+1))/2
    env = TEMM(Xn=X, u=u_now, K=K, initial="kmeans", iter.max=iter.max, stop=stop, trueY=trueY)
    loglk = logMixTenGau(Xm, env$pi, env$eta, env$Mu.est, env$SIG.est)
    
    # err[i,j] = cluster_err(K,Y,env$id)$cluster_err
    # bic[i,j] = -2*loglk + log(n)*Ku
    bic_now = -2*loglk + log(n)*Ku
    
    if(bic_now<opt.bic){
      opt.bic = bic_now
      opt.u = u_now
      opt.id = env$id
      opt.Mu = env$Mu.est
    }
    
  }
  
  
  # ind = as.vector(arrayInd(which.min(bic), dim_u))
  # opt.u = rep(0,M)
  # for (m in 1:M) {
  #   opt.u[m] = u_candi[[m]][ind[m]]
  # }
  # opt.err = err[ind]
  
  return(list(opt.u=opt.u, opt.id=opt.id, opt.Mu=opt.Mu, bic=opt.bic))
}

