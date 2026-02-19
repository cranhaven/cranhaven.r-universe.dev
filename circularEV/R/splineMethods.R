
negloglik <- function(hp, ydata){
  n <- length(ydata)
  xi <- hp[1]
  beta <- exp(hp[2])

  cond1 <- beta <= 0
  cond2 <- (xi <= 0) && (max(ydata) > (-beta/xi))
  OnePlusXiYBeta <- 1 + (xi * ydata)/beta
  cond3 <- OnePlusXiYBeta <= 0

  if (cond1 | cond2 | (sum(cond3)>0) ) {
    f <- 1e+06
  } else {
    y <- logb(OnePlusXiYBeta)
    y <- y/xi
    f <- n * logb(beta) + (1 + xi) * sum(y)
  }
  return(f)
}



negloglik_per_iFUN <- function(i, xi_all, beta_all, y, ang){
  xi <- xi_all[ang[i]+1L]
  beta <- beta_all[ang[i]+1L]
  ydata <- y[i]

  cond1 <- beta <= 0
  cond2 <- (xi <= 0) && (ydata > (-beta/xi))
  OnePlusXiYBeta <- 1 + (xi * ydata)/beta
  cond3 <- OnePlusXiYBeta <= 0

  if (cond1 | cond2 | cond3) {
    f <- Inf
  } else {
    f <- log(beta) + (1 + 1/xi) * log(OnePlusXiYBeta)
  }
  return(f)
}


GPDcond_per_i_iFUN <- function(j, xi_all, beta_all, yfull, angfull){
  xi <- xi_all[angfull[j]+1L]
  beta <- beta_all[angfull[j]+1L]
  ydata <- yfull[j]

  cond1 <- beta <= 0
  OnePlusXiYBeta <- 1 + (xi * ydata)/beta
  cond3 <- OnePlusXiYBeta <= 0

  if (cond1 | cond3) {
    f <- Inf
  } else {
    f <- log(beta) + (1 + 1/xi) * log(OnePlusXiYBeta)
  }
  return(f)
}



negloglik_VaryingParams <- function(hp, y, angle, yfull, angfull, lambda, kappa,
                                    bspl, where_xi, where_sig) {

  xi_all <- c(bspl%*%hp[where_xi])

  beta_all <- c(exp(bspl%*%hp[where_sig]))

  nBasis <- length(hp[where_xi])
  P <- diff(diag(nBasis), differences = 1)
  P <- rbind(P, c(1, rep(0, nBasis-2), -1))
  PP <- t(P)%*%P

  n <- length(yfull)
  GPDcond_per_i <- sapply(1:n, function(j)
    GPDcond_per_i_iFUN(j=j, xi_all=xi_all, beta_all=beta_all, yfull=yfull, angfull=angfull))

  if(max(GPDcond_per_i)==Inf){
    negloglik_per_i <- Inf
    output <- Inf
  }else{
    n <- length(y)
    negloglik_per_i <- sapply(1:n, function(i)
      negloglik_per_iFUN(i=i, xi_all=xi_all, beta_all=beta_all, y=y, ang=angle))
    coeffsXi <- as.matrix(hp[where_xi])
    coeffsSig <- as.matrix(hp[where_sig])

    penalty <- lambda*t(coeffsXi)%*%PP%*%coeffsXi + kappa*t(coeffsSig)%*%PP%*%coeffsSig
    output <- sum(negloglik_per_i) + penalty
  }

  return(output)
}

negloglik_TestSet <- function(hp, ydata, angle_exceed, bspl, where_xi, where_sig) {

  xi_all <- c(bspl%*%hp[where_xi])

  beta_all <- exp(bspl%*%hp[where_sig])

  ydata_all <- ydata

  n <- length(ydata)
  negloglik_per_i <- sapply(1:n, function(i)
    negloglik_per_iFUN(i, xi_all, beta_all, ydata_all, angle_exceed))

  output <- mean(negloglik_per_i)

  return(output)
}


Dnegloglik_VaryingParams <- function(hp, y, angle, lambda, kappa, yfull=NULL,
                                     angfull=NULL, bspl, where_xi, where_sig) {

  ydata <- y
  angle_exceed <- angle
  xi_all <- c(bspl%*%hp[where_xi])

  beta_all <- exp(bspl%*%hp[where_sig])

  ydata_all <- ydata

  nBasis <- length(hp[where_xi])
  P <- diff(diag(nBasis), differences = 1)
  P <- rbind(P, c(1, rep(0, nBasis-2), -1))
  PP <- t(P)%*%P

  n <- length(ydata)

  DPenalty_bj <- rep(NA, 2*nBasis)
  Dneglik_bj_i <- matrix(NA, n, 2*nBasis)
  for(j in 1:(2*nBasis)){
    for(i in 1:n){
      xi <- xi_all[angle_exceed[i]+1L]
      beta <- beta_all[angle_exceed[i]+1L]
      ydata <- ydata_all[i]

      if(j%in%where_xi){
        Bij <- bspl[angle_exceed[i]+1L,j]
        Dneglik_bj_i[i, j] <- (-1/xi^2)*Bij*log(1+xi*ydata/beta) +
          (1+1/xi)*ydata*Bij/((1+xi*ydata/beta)*beta)
      }else{
        Bij <- bspl[angle_exceed[i]+1L,j-nBasis]
        Dneglik_bj_i[i, j] <- Bij + (1+1/xi)*xi*ydata*(-1/beta^2)*beta*Bij/(1+xi*ydata/beta)
      }
    }

    if(j%in%where_xi){

      hp_j <- hp[j]
      if(!(j%in%c(1,nBasis))){
        hp_j_left <- hp[j-1]
        hp_j_right <- hp[j+1]
      }
      if(j==1){
        hp_j_left <- hp[nBasis]
        hp_j_right <- hp[j+1]
      }
      if(j==nBasis){
        hp_j_left <- hp[j-1]
        hp_j_right <- hp[1]
      }
      DPenalty_bj[j] <- lambda*(2*(hp_j_right - hp_j)*(-1) + 2*(hp_j - hp_j_left))
    }else{

      hp_j <- hp[j]
      if(!(j%in%c(1+nBasis,nBasis+nBasis))){
        hp_j_left <- hp[j-1]
        hp_j_right <- hp[j+1]
      }
      if(j==(1+nBasis)){
        hp_j_left <- hp[nBasis+nBasis]
        hp_j_right <- hp[j+1]
      }
      if(j==(nBasis+nBasis)){
        hp_j_left <- hp[j-1]
        hp_j_right <- hp[1+nBasis]
      }
      DPenalty_bj[j] <- kappa*(2*(hp_j_right - hp_j)*(-1) + 2*(hp_j - hp_j_left))

    }

  }

  output <- apply(Dneglik_bj_i, 2, sum) + DPenalty_bj

  return(output)
}


ret_level_gpd <- function(sigma,xi,tau,thresh,period){
  ret_level <- thresh + (sigma/xi)*(((period*(1-tau))^(xi))-1)
  return(ret_level)
}


Calc_taufun <- function(Data, drc, h, thetaGrid=1:360, thr){

  n <- length(Data)

  thetaGrid <- c(0, thetaGrid)
  tauLocalEstim <- rep(NA, length(thetaGrid))
  for(theta_i in seq_along(thetaGrid)){

    whichNeighb <- rep(NA, n)
    for(i in 1:n){
      whichNeighb[i] <- (  min(abs(theta_i - drc[i]), 360 - abs(theta_i - drc[i])) < h)
    }

    countExceed <- (Data[whichNeighb] > thr[drc[whichNeighb]])
    propExceed <- mean(countExceed)
    tauLocalEstim[theta_i] <- 1-propExceed

  }
  tauLocalEstim <- tauLocalEstim + 1e-4

  logit_tauLocalEstim <- log(tauLocalEstim/(1-tauLocalEstim))

  dftaufun <- data.frame(thetaGrid, logit_tauLocalEstim)
  fit <- mgcv::gam(logit_tauLocalEstim ~ s(thetaGrid,bs="cc"), data = dftaufun)

  taufun <- exp(fit$fitted.values)/(1+exp(fit$fitted.values))

  return(taufun)
}
