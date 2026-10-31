
# upadate Gamma in every iteration

estGamma = function(X, Mutil, PGamma, SIG, SIGinvhalf, eta, u){


  #X is tensor data observations, last mode is sample size
  #Mutil is \tilde{\mu} in paper, last mode is number of clusters
  #SIGinvhalf is a list of inverse square root of covariance matrices
  #u is a vector of envelope dimensions

  dimen = dim(X)[-length(dim(X))]   #vector of dimensions
  M = length(dimen)
  p = prod(dimen)
  n = dim(X)[M+1]
  K = dim(Mutil)[M+1]

  Xm = tensr::collapse_mode(X,m=1:M)

  SIG_old = SIG
  SIG_new = Gamma = list()
  Mm = Nm = list()
  iter.max = 1
  for(iter in 1:iter.max){
    #SIG_err = rep(0,M)
    for (m in 1:M) {
      dim_m = (1:M)[-m]
      PGamma_m = PGamma
      PGamma_m[[m]] = NULL
      Mutil_p = rTensor::ttl(rTensor::as.tensor(Mutil), PGamma_m, ms=dim_m)@data  #projected \tilde{\mu}
      mutil_p = tensr::collapse_mode(Mutil_p,m=1:M)
      Xm_mu = matrix(0,p,n*K)
      for(k in 1:K){
        Xm_mu[,((k-1)*n+1):(k*n)] = t(t(Xm-mutil_p[,k])*sqrt(eta[,k]))
      }
      X_Mu = array(Xm_mu,c(dimen,n*K))

      SIGinvhalf_m = SIGinvhalf
      SIGinvhalf_m[[m]] = NULL

      Mhalf = rTensor::ttl(rTensor::as.tensor(X_Mu), SIGinvhalf_m, ms=dim_m)
      Nhalf = rTensor::ttl(rTensor::as.tensor(X), SIGinvhalf_m, ms=dim_m)
      # Mm_temp = mat(Mhalf@data,m)
      # N_temp = mat(Nhalf@data,m)
      # Mm = Mm_temp %*% t(Mm_temp) / (n*p/dimen[m])
      # N = N_temp %*% t(N_temp) / (n*p/dimen[m])
      # faster:
      Mm[[m]] = TRES::ttt(Mhalf, Mhalf, c(dim_m,M+1))@data / (n*p/dimen[m])
      Nm[[m]] = TRES::ttt(Nhalf, Nhalf, c(dim_m,M+1))@data / (n*p/dimen[m])

      if(u[m]==dimen[m]){
        Gamma[[m]] = diag(dimen[m])
      }
      else{
        Gamma[[m]] = TRES::ECD(Mm[[m]], Nm[[m]]-Mm[[m]], u[m])
      }
      PGamma[[m]] = Gamma[[m]] %*% t(Gamma[[m]])
      SIG_new[[m]] = PGamma[[m]] %*% Mm[[m]] %*% PGamma[[m]]+
        (diag(dimen[m])-PGamma[[m]]) %*% Nm[[m]] %*% (diag(dimen[m])-PGamma[[m]])
      SIGinvhalf[[m]] = pracma::sqrtm(SIG_new[[m]])$Binv
      #SIG_err[m] = tensr::fnorm(SIG_new[[m]]-SIG_old[[m]])/tensr::fnorm(SIG_old[[m]])
    }
    #SIG_err = norm(as.matrix(SIG_err),type="F")
    #print(SIG_err)
    # if(SIG_err<1e-6){
    #   break
    # }
    # else{
    #   SIG_old = SIG_new
    # }
  }

  SIGinv = lapply(SIG,MASS::ginv)
  return(list(Gamma_est=Gamma, PGamma_est=PGamma, Mm=Mm, Nm=Nm,
              SIG_est=SIG_new, SIGinv_est=SIGinv, SIGinvhalf=SIGinvhalf))
}




