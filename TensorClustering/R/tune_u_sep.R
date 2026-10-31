#' @export

tune_u_sep = function(m, u_candi, K, X, C=1, oneD=TRUE, iter.max=500, stop=1e-3, trueY=NULL){
  
  dimen = dim(X)[-length(dim(X))]
  n = dim(X)[length(dim(X))]
  M = length(dim(X))-1
  u = dimen
  #dimen_m = dimenrod(dimen)/dimen[m]
  
  if(oneD){
    env_ini = TEMM(Xn=X, u=dimen, K=K, initial="kmeans", iter.max=iter.max, stop=stop, trueY=trueY)
    matM = env_ini$Mm[[m]]
    matU = env_ini$Nm[[m]] - env_ini$Mm[[m]]
    tune_1d = TRES::oneD_bic(matM, matU, n, C=C, maxdim=dimen[m])
    bic = tune_1d$bicval
    opt.u = tune_1d$u
  } else {
    Gm = rep(0,length(u_candi))
    
    for(i in 1:length(u_candi)) {
      u[m] = u_candi[i] 
      #df[i] = (K-1)*dimenrod(u) + sum((dimen-u)*u + u*(u+1)/2 + (dimen-u)*(dimen-u+1)/2)
      env = TEMM(Xn=X, u=u, K=K, initial="kmeans", iter.max=iter.max, stop=stop, trueY=trueY)
      Gamma = env$Gamma.est
      Mm = env$Mm
      Nm = env$Nm
      
      Gm[i] = log(det(t(Gamma[[m]])%*%Mm[[m]]%*%Gamma[[m]]))+
        log(det(t(Gamma[[m]])%*%solve(Nm[[m]])%*%Gamma[[m]]))
    }
    bic = Gm + C*u_candi*log(n)/n
    opt.u = u_candi[which.min(bic)]
  }
  
  
  return(list(opt.u=opt.u, bic=bic))
}