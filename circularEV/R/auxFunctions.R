

# Random generation for a generalized Pareto distribution
rgpd <- function (n, loc = 0, scale = 1, shape = 0) {
  if(scale <= 0){
    stop("scale must be positive")
  }
  if(shape == 0){
    return(loc + scale * stats::rexp(n))
  }else{
    return(loc + scale * (stats::runif(n)^(-shape) - 1)/shape)
  }
}




AutoChoice_k_circ <- function(df_local, EVIestimator, ss, useKernel=F, b,
                              concent=NULL){

  n <- nrow(df_local)
  kmin <- 2
  kmax <- n
  kvec <- kmin:(kmax-1)

  xiVec <- rep(NA, length(kvec))
  aVec <- rep(NA, length(kvec))
  xisdVec <- rep(NA, length(kvec))
  Svec <- rep(NA, length(kvec))

  xisd_ML <- NA
  nloc <- nrow(df_local)
  for(ki in seq_along(kvec)){

    k <- kvec[ki]

    #### local estimation
    if(!useKernel){
      X <- sort(df_local$X)
      switch (EVIestimator,
              Mom = {
                mom <- MomEst(X=X, k=k, asymptotic=F);
                xi <- mom$xi;
                a <- mom$scale
              },
              ML = {
                ml <- MLEst(X=X, k=k, asymptotic=F);
                xi <- ml$xi;
                a <- ml$scale;
                xisd_ML <- ml$xisd
              }
      )
    }else{
      #### local estimation using kernel

      u <- sort(df_local$X)[nloc-k]
      whichExcesses <- (df_local$X - u)>0
      excesses <- df_local$X[whichExcesses] - u

      Q1 <- (log(df_local$X) - log(u))[whichExcesses]
      Q2 <- ((log(df_local$X) - log(u))^2)[whichExcesses]

      ss_i <- df_local$theta_loc[whichExcesses]
      L <- myKernel(x = ss_i, x0 = ss, concent = concent)[1,]

      H <- sum(L*Q1)
      M2 <- sum(L*Q2)

      switch (EVIestimator,
              Mom = {
                mom <- MomEstKernel(H=H, M2=M2, k=k, n=nloc, localThr=u, asymptotic=F);
                xi <- mom$xi;
                a <- mom$scale;
              },
              ML = {
                ml <- MLEstKernel(excesses=excesses, L=L, k=k, n=nloc, localThr=u, asymptotic=F);
                xi <- ml$xi;
                a <- ml$scale;
                xisd_ML <- ml$xisd;
              },
      )
    }

    xiVec[ki] <- xi
    aVec[ki] <- a
    xisdVec[ki] <- xisd_ML
    Svec[ki] <- mean(((1:ki)^b)*abs(xiVec[1:ki] - stats::median(xiVec[1:ki], na.rm=T)))

  }

  kminprov <- 40
  k_opt <- kvec[which.min(Svec[-c(1:kminprov)])+kminprov]
  xi_opt <- xiVec[which.min(Svec[-c(1:kminprov)])+kminprov]

  return(list(xi_opt=xi_opt, k_opt=k_opt,
              xiVec=xiVec, kvec=kvec, xisdVec=xisdVec))
}
