
# x: theta_i
# x0: theta (centre)
# concent: concentration parameter (similar job as bandwidth)

myKernel <- function (x, x0, concent) {

  xDegrees <- circular::circular(x, units = "degrees")
  xRadians <- circular::conversion.circular(xDegrees, units = "radians", zero = 0,
                                             rotation = "counter", modulo = "2pi")

  x0Degrees <- circular::circular(x0, units = "degrees")
  x0Radians <- circular::conversion.circular(x0Degrees, units = "radians", zero = 0,
                                              rotation = "counter", modulo = "2pi")

  x_x0 <- outer(x0Radians, xRadians, "-")
  B <- exp(concent * cos(x_x0))
  L <- B/apply(B, 1, sum)

  return(L)
}


MomEstKernel <- function(H, M2, k, n, localThr=NULL,
                         TTs=NULL, timeRange=NULL, asymptotic=TRUE, xi=NULL, a=NULL){

  if(is.null(xi)){
    # deHaan and Ferreira eq 3.5.9
    xi <- H+1-0.5/(1-(H*H)/M2)
  }
  if(xi < 0){
    varStdxi <- ((1-xi)^2*(1-2*xi)*(1-xi+6*xi^2))/((1-3*xi)*(1-4*xi))
  }else{
    varStdxi <- xi^2 + 1
  }
  xisd <- sqrt(1/k)*sqrt(varStdxi)

  if(is.null(a)){
    # scale estimate for a(n/k)
    a <- 0.5*(1 - H^2/M2)^(-1)*localThr*H
  }

  if(!is.null(TTs)){

    RLvec <- rep(NA, length(TTs))
    RLsdvec <- rep(NA, length(TTs))
    for(iTT in seq_along(TTs)){
      TT <- TTs[iTT]
      TTadj <- Tadj(TT=TT, n=n, timeRange=timeRange)

      # T-year levels
      RLvec[iTT] <- localThr + a*((TTadj*(k/n))^xi - 1)/xi

      # calculate standard deviation of RLs
      if(asymptotic){
        # q_xi (at infinity) in remark 4.3.3 p. 135 deHandF
        if(xi<0){
          q_xi <- 1/xi^2
        }else{
          q_xi <- (TTadj*(k/n))^xi*log(TTadj*(k/n))/xi
          if(xi==0){
            q_xi <- 0.5*(log((TTadj*(k/n))))^2
          }
        }
        # variance is given by eq. 4.3.12 p. 141 deHandF
        if(xi<0){
          varStdRL <- ((1-xi)^2*(1-3*xi+4*xi^2))/((1-2*xi)*(1-3*xi)*(1-4*xi))
        }else{
          varStdRL <- xi^2+1
        }
        RLsd <- sqrt(1/k)*a*q_xi*sqrt(varStdRL)


        RLsdvec[iTT] <- RLsd
      }else{
        RLsdvec <- NULL
      }

    }
  }else{
    RLvec <- NULL
    RLsdvec <- NULL
  }

  return(list(xi=xi, xisd=xisd, scale=a, RL=RLvec, RLsd=RLsdvec, H=H))
}




MLEstKernel <- function(excesses=excesses, L=L, k=k, n, localThr=NULL,
                        TTs=NULL, timeRange=NULL, asymptotic=TRUE,
                        xi=NULL, a=NULL, xisd=NULL, restrict=FALSE){

  if(is.null(xi) | is.null(a) | is.null(xisd)){
    parInit <- c(-0.1, log(2))
    fit <- stats::nlminb(start=parInit, objective=kernelnegloglik, ydata=excesses,
                         L=L, restrict=restrict)
    xi <- fit$par[1]
    xisd <- abs(1+xi)/sqrt(k)
    a <- exp(fit$par[2])
  }

  if(!is.null(TTs)){

    RLvec <- rep(NA, length(TTs))
    RLsdvec <- rep(NA, length(TTs))

    for(iTT in seq_along(TTs)){
      TT <- TTs[iTT]
      TTadj <- Tadj(TT=TT, n=n, timeRange=timeRange)

      # T-year levels
      if(!is.na(xi)){
        RLvec[iTT] <- localThr + a*((TTadj*(k/n))^xi - 1)/xi
      }
      # calculate standard deviation of RLs
      if(asymptotic & !is.na(xi)){
        # q_xi (at infinity) in remark 4.3.3 p. 135 deHandF
        if(xi<0){
          q_xi <- 1/xi^2
        }else{
          q_xi <- (TTadj*(k/n))^xi*log(TTadj*(k/n))/xi
          if(xi==0){
            q_xi <- 0.5*(log((TTadj*(k/n))))^2
          }
        }
        if(xi<0){
          varStdRL <- 1+4*xi+5*xi^2+2*xi^3+2*xi^4
        }else{
          varStdRL <- (1+xi)^2
        }
        RLsd <- sqrt(1/k)*a*q_xi*sqrt(varStdRL)


        RLsdvec[iTT] <- RLsd
      }else{
        RLsdvec <- NULL
      }

    }
  }else{
    RLvec <- NULL
    RLsdvec <- NULL
  }

  return(list(xi=xi, xisd=xisd, scale=a, RL=RLvec, RLsd=RLsdvec))
}



MomEstKernelMovThr <- function(H, M2, k, n=NULL, localThr=NULL,
                               TTs=NULL, timeRange=NULL, asymptotic=TRUE,
                               xi=NULL, a=NULL){

  if(is.null(xi)){
    # deHaan and Ferreira eq 3.5.9
    xi <- H+1-0.5/(1-(H*H)/M2)
  }
  if(xi < 0){
    varStdxi <- ((1-xi)^2*(1-2*xi)*(1-xi+6*xi^2))/((1-3*xi)*(1-4*xi))
  }else{
    varStdxi <- xi^2 + 1
  }
  xisd <- sqrt(1/k)*sqrt(varStdxi)

  if(is.null(a)){
    # scale estimate for a(n/k)
    a <- 0.5*(1 - H^2/M2)^(-1)*localThr*H
  }

  if(!is.null(TTs)){

    RLvec <- rep(NA, length(TTs))
    RLsdvec <- rep(NA, length(TTs))
    for(iTT in seq_along(TTs)){
      TT <- TTs[iTT]
      TTadj <- Tadj(TT=TT, n=n, timeRange=timeRange)

      # T-year levels
      RLvec[iTT] <- localThr + a*((TTadj*(k/n))^xi - 1)/xi

      # calculate standard deviation of RLs
      if(asymptotic){
        # q_xi (at infinity) in remark 4.3.3 p. 135 deHandF
        if(xi<0){
          q_xi <- 1/xi^2
        }else{
          q_xi <- (TTadj*(k/n))^xi*log(TTadj*(k/n))/xi
          if(xi==0){
            q_xi <- 0.5*(log((TTadj*(k/n))))^2
          }
        }
        # variance is given by eq. 4.3.12 p. 141 deHandF
        if(xi<0){
          varStdRL <- ((1-xi)^2*(1-3*xi+4*xi^2))/((1-2*xi)*(1-3*xi)*(1-4*xi))
        }else{
          varStdRL <- xi^2+1
        }
        RLsd <- sqrt(1/k)*a*q_xi*sqrt(varStdRL)


        RLsdvec[iTT] <- RLsd
      }else{
        RLsdvec <- NULL
      }

    }
  }else{
    RLvec <- NULL
    RLsdvec <- NULL
  }
  return(list(xi=xi, xisd=xisd, scale=a, RL=RLvec, RLsd=RLsdvec, H=H))
}






MLEstKernelMovThr <- function(excesses, L,
                              k, n=NULL, localThr=NULL,
                              TTs=NULL, timeRange=NULL, asymptotic=TRUE,
                              xi=NULL, a=NULL, xisd=NULL, restrict=FALSE){


  if(is.null(xi) | is.null(a) | is.null(xisd)){

    parInit <- c(log(0.4), log(2))   # initial values
    if(restrict){
      fit <- stats::nlminb(start=parInit, objective=kernelnegloglik, ydata=excesses, L=L,
                   lower=c(log(0.01), log(0.1)),
                   upper=c(log(0.5-1e-10), log(10)))
    }else{
      fit <- stats::nlminb(start=parInit, objective=kernelnegloglik, ydata=excesses, L=L,
                    lower=c(log(0.01), log(0.1)),
                    upper=c(log(0.5+0.01), log(10)))
    }





    xi <- -0.5+exp(fit$par[1])
    xisd <- abs(1+xi)/sqrt(k)
    a <- exp(fit$par[2])
  }

  if(!is.null(TTs)){

    RLvec <- rep(NA, length(TTs))
    RLsdvec <- rep(NA, length(TTs))

    for(iTT in seq_along(TTs)){
      TT <- TTs[iTT]
      TTadj <- Tadj(TT=TT, n=n, timeRange=timeRange)

      # T-year levels
      if(!is.na(xi)){
        RLvec[iTT] <- localThr + a*((TTadj*(k/n))^xi - 1)/xi
      }
      # calculate standard deviation of RLs
      if(asymptotic & !is.na(xi)){
        # q_xi (at infinity) in remark 4.3.3 p. 135 deHandF
        if(xi<0){
          q_xi <- 1/xi^2
        }else{
          q_xi <- (TTadj*(k/n))^xi*log(TTadj*(k/n))/xi
          if(xi==0){
            q_xi <- 0.5*(log((TTadj*(k/n))))^2
          }
        }
        if(xi<0){
          varStdRL <- 1+4*xi+5*xi^2+2*xi^3+2*xi^4
        }else{
          varStdRL <- (1+xi)^2
        }
        RLsd <- sqrt(1/k)*a*q_xi*sqrt(varStdRL)


        RLsdvec[iTT] <- RLsd
      }else{
        RLsdvec <- NULL
      }

    }
  }else{
    RLvec <- NULL
    RLsdvec <- NULL
  }
  return(list(xi=xi, xisd=xisd, scale=a, RL=RLvec, RLsd=RLsdvec))
}



kernelnegloglik <- function(hp, ydata, L, restrict=TRUE){
  n <- length(ydata)

  if(restrict){
    xi <- -0.5+exp(hp[1])
  }else{
    xi <- hp[1]
  }

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
    f <- n * logb(beta) + (1 + xi) * n * sum(y*L)
  }
  return(f)
}




## Quantile estimation
# Let x_p := U(1/p) be the quantile we want to estimate
# TT := 1/p                            --- notation for T-(year) level
# n                                    --- number of observations
# k                                    --- is the number of top order statistics used in POT
# timeRange                           --- period of data collection
# n_avg_per_time_unit := n/timeRange  --- average number of observations per unit of time
# Tadj := TT*n_avg_per_time_unit      --- T adjusted (accordingly to the sample size per unit of time)
# Tadj*(k/n)                          --- term that appears on high quantile calculation

Tadj <- function(TT, n, timeRange){
  n_avg_per_time_unit <- n/timeRange
  Tadj <- TT*n_avg_per_time_unit
  return(Tadj)
}






MomEst <- function(X, k, TTs=NULL, timeRange=NULL, asymptotic=TRUE, xi=NULL, a=NULL){

  X <- sort(X)
  n <- length(X)

  # deHaan and Ferreira eq 3.5.2:
  m2log <- (1/k)*sum((log(X[n:(n-k+1)])-log(X[n-k]))^2)
  H <- (1/k)*sum(log(X[n:(n-k+1)])) - log(X[n-k])

  if(is.null(xi)){
    # deHaan and Ferreira eq 3.5.9
    xi <- H+1-0.5/(1-(H*H)/m2log)
  }
  if(xi < 0){
    varStdxi <- ((1-xi)^2*(1-2*xi)*(1-xi+6*xi^2))/((1-3*xi)*(1-4*xi))
  }else{
    varStdxi <- xi^2 + 1
  }
  xisd <- sqrt(1/k)*sqrt(varStdxi)

  if(is.null(a)){
    a <- 0.5*(1 - H^2/m2log)^(-1)*X[n-k]*H
  }

  if(!is.null(TTs)){

    RLvec <- rep(NA, length(TTs))
    RLsdvec <- rep(NA, length(TTs))
    for(iTT in seq_along(TTs)){
      TT <- TTs[iTT]
      TTadj <- Tadj(TT=TT, n=n, timeRange=timeRange)

      # T-year levels
      RLvec[iTT] <- X[n-k] + a*((TTadj*(k/n))^xi - 1)/xi

      # calculate standard deviation of RLs
      if(asymptotic){
        # q_xi (at infinity) in remark 4.3.3 p. 135 deHandF
        if(xi<0){
          q_xi <- 1/xi^2
        }else{
          q_xi <- (TTadj*(k/n))^xi*log(TTadj*(k/n))/xi
          if(xi==0){
            q_xi <- 0.5*(log((TTadj*(k/n))))^2
          }
        }
        # variance is given by eq. 4.3.12 p. 141 deHandF
        if(xi<0){
          varStdRL <- ((1-xi)^2*(1-3*xi+4*xi^2))/((1-2*xi)*(1-3*xi)*(1-4*xi))
        }else{
          varStdRL <- xi^2+1
        }
        RLsd <- sqrt(1/k)*a*q_xi*sqrt(varStdRL)


        RLsdvec[iTT] <- RLsd
      }else{
        RLsdvec <- NULL
      }

    }
  }else{
    RLvec <- NULL
    RLsdvec <- NULL
  }
  return(list(xi=xi, xisd=xisd, scale=a, RL=RLvec, RLsd=RLsdvec, H=H))
}





MLEst <- function(X, k, TTs=NULL, timeRange=NULL, asymptotic=TRUE, xi=NULL, a=NULL, xisd=NULL){

  X <- sort(X)
  n <- length(X)

  if(is.null(xi) | is.null(a) | is.null(xisd)){
    parInit <- c(-0.1, log(2))   # initial values
    excesses <- (X-X[n-k])[(n-k+1):n]
    fit <- stats::nlminb(start = parInit, objective = negloglik, ydata = excesses)
    xi <- fit$par[1]
    xisd <- abs(1+xi)/sqrt(k)
    a <- exp(fit$par[2])
  }

  if(!is.null(TTs)){

    RLvec <- rep(NA, length(TTs))
    RLsdvec <- rep(NA, length(TTs))

    for(iTT in seq_along(TTs)){
      TT <- TTs[iTT]
      TTadj <- Tadj(TT=TT, n=n, timeRange=timeRange)

      # T-year levels
      if(!is.na(xi)){
        RLvec[iTT] <- X[n-k] + a*((TTadj*(k/n))^xi - 1)/xi
      }
      # calculate standard deviation of RLs
      if(asymptotic & !is.na(xi)){
        # q_xi (at infinity) in remark 4.3.3 p. 135 deHandF
        if(xi<0){
          q_xi <- 1/xi^2
        }else{
          q_xi <- (TTadj*(k/n))^xi*log(TTadj*(k/n))/xi
          if(xi==0){
            q_xi <- 0.5*(log((TTadj*(k/n))))^2
          }
        }
        if(xi<0){
          varStdRL <- 1+4*xi+5*xi^2+2*xi^3+2*xi^4
        }else{
          varStdRL <- (1+xi)^2
        }
        RLsd <- sqrt(1/k)*a*q_xi*sqrt(varStdRL)


        RLsdvec[iTT] <- RLsd
      }else{
        RLsdvec <- NULL
      }

    }
  }else{
    RLvec <- NULL
    RLsdvec <- NULL
  }

  return(list(xi=xi, xisd=xisd, scale=a, RL=RLvec, RLsd=RLsdvec))
}


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




MomEstMovThr <- function(H, m2log, k, n=NULL, localThr=NULL, TTs=NULL,
                         timeRange=NULL, asymptotic=TRUE, xi=NULL, a=NULL, xisd=NULL){

  if(is.null(xi)){
    # deHaan and Ferreira eq 3.5.9
    xi <- H+1-0.5/(1-(H*H)/m2log)
  }
  if(xi < 0){
    varStdxi <- ((1-xi)^2*(1-2*xi)*(1-xi+6*xi^2))/((1-3*xi)*(1-4*xi))
  }else{
    varStdxi <- xi^2 + 1
  }
  xisd <- sqrt(1/k)*sqrt(varStdxi)

  if(is.null(a)){
    a <- 0.5*(1 - H^2/m2log)^(-1)*localThr*H
  }

  if(!is.null(TTs)){

    RLvec <- rep(NA, length(TTs))
    RLsdvec <- rep(NA, length(TTs))
    for(iTT in seq_along(TTs)){
      TT <- TTs[iTT]
      TTadj <- Tadj(TT=TT, n=n, timeRange=timeRange)

      # T-year levels
      RLvec[iTT] <- localThr + a*((TTadj*(k/n))^xi - 1)/xi

      # calculate standard deviation of RLs
      if(asymptotic){
        # q_xi (at infinity) in remark 4.3.3 p. 135 deHandF
        if(xi<0){
          q_xi <- 1/xi^2
        }else{
          q_xi <- (TTadj*(k/n))^xi*log(TTadj*(k/n))/xi
          if(xi==0){
            q_xi <- 0.5*(log((TTadj*(k/n))))^2
          }
        }
        # variance is given by eq. 4.3.12 p. 141 deHandF
        if(xi<0){
          varStdRL <- ((1-xi)^2*(1-3*xi+4*xi^2))/((1-2*xi)*(1-3*xi)*(1-4*xi))
        }else{
          varStdRL <- xi^2+1
        }
        RLsd <- sqrt(1/k)*a*q_xi*sqrt(varStdRL)


        RLsdvec[iTT] <- RLsd
      }else{
        RLsdvec <- NULL
      }

    }
  }else{
    RLvec <- NULL
    RLsdvec <- NULL
  }

  return(list(xi=xi, xisd=xisd, scale=a, RL=RLvec, RLsd=RLsdvec, H=H))
}



MLEstMovThr <- function(excesses, n=NULL, localThr=NULL, TTs=NULL,
                        timeRange=NULL, asymptotic=TRUE, xi=NULL, a=NULL, xisd=NULL){

  k <- length(excesses)

  if(is.null(xi) | is.null(a) | is.null(xisd)){
    parInit <- c(-0.1, log(2))   # initial values
    fit <- stats::nlminb(start = parInit, objective = negloglik, ydata = excesses)
    xi <- fit$par[1]
    xisd <- abs(1+xi)/sqrt(k)
    a <- exp(fit$par[2])
  }

  if(!is.null(TTs)){

    RLvec <- rep(NA, length(TTs))
    RLsdvec <- rep(NA, length(TTs))

    for(iTT in seq_along(TTs)){
      TT <- TTs[iTT]
      TTadj <- Tadj(TT=TT, n=n, timeRange=timeRange)

      # T-year levels
      if(!is.na(xi)){
        RLvec[iTT] <- localThr + a*((TTadj*(k/n))^xi - 1)/xi
      }
      # calculate standard deviation of RLs
      if(asymptotic & !is.na(xi)){
        # q_xi (at infinity) in remark 4.3.3 p. 135 deHandF
        if(xi<0){
          q_xi <- 1/xi^2
        }else{
          q_xi <- (TTadj*(k/n))^xi*log(TTadj*(k/n))/xi
          if(xi==0){
            q_xi <- 0.5*(log((TTadj*(k/n))))^2
          }
        }
        if(xi<0){
          varStdRL <- 1+4*xi+5*xi^2+2*xi^3+2*xi^4
        }else{
          varStdRL <- (1+xi)^2
        }
        RLsd <- sqrt(1/k)*a*q_xi*sqrt(varStdRL)


        RLsdvec[iTT] <- RLsd
      }else{
        RLsdvec <- NULL
      }

    }
  }else{
    RLvec <- NULL
    RLsdvec <- NULL
  }

  return(list(xi=xi, xisd=xisd, scale=a, RL=RLvec, RLsd=RLsdvec))
}
