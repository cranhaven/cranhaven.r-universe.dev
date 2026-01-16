#' Automatic Modeling of a Scalar Non-Seasonal Time Series
#'
#' Select an ARIMA model for a non-seasonal scalar time series. It uses augmented Dickey-Fuller
#' (ADF) test to check for unit roots.
#' The maximum degree of differencing is 2.
#'
#' @param zt T by 1 vector of an observed scalar time series without missing values.
#' @param maxorder Maximum order of \eqn{(p,d,q)} where \eqn{p} is the AR order, \eqn{d} the degree of differencing,
#' and \eqn{q} the MA order. Default value is (5,1,4).
#' @param criterion Information criterion used for model selection. Either AIC or BIC. Default is "bic".
#' @param output If TRUE it returns the differencing order, the selected order and the minimum
#' value of the criterion. Default is TRUE.
#' @param method Estimation method. See the arima command in R. Possible values are "CSS-ML", "ML", and "CSS".
#' Default is "CSS-ML".
#' @param pv P-value for unit-root test. Default is 0.01.
#'
#' @details
#' Find the AR order by checking a pure AR model for the differenced series.
#' The maximum AR order tried is min(default AR order and the order of pure AR model).
#' Check the MA order by checking pure MA model using rank-based Ljung-Box statistics.
#' The maximum MA order tried is the min(default MA order and the order of pure MA model).
#' Finally, sequentially decreasing the AR order and increasing the MA order to obtain
#' best models using the specified criterion function.

#' @return A list containing:
#' \itemize{
#'    \item order - Regular ARIMA order.
#'    \item crit - Minimum criterion.
#'    \item include.mean - Switch about including mean in the model.
#' }
#'
#' @examples
#' data(TaiwanAirBox032017)
#' fit <- arimaSpec(as.matrix(TaiwanAirBox032017[,1]))
#'
#' @import stats
#'
#' @export
"arimaSpec" <- function(zt, maxorder = c(5,1,4), criterion="bic",
                        output = FALSE, method = "CSS-ML", pv = 0.01){

  d <- 0
  p <- maxorder[1]+maxorder[2]
  ### The next line is added on January 31, 2021. RST
  if(is.matrix(zt)){
    zt <- zt[,1]
    message("This command works on scalar time series only. First column is used.","\n")
  }

  pv <- stats::Box.test(rank(zt),lag=10,type="Ljung")$p.value
  if(pv < 0.1){
    p1 <- ar(zt,order.max=p)$order
  }else{p1 <- p}

  utcri <- -2.86
  if(pv < 0.03)utcri <- -3.12
  if(pv < 0.011)utcri <- -3.43
  tst1 <- adftest(zt,lags=p1)

  if(tst1 >= utcri)d <- 1

  if(d==1){
    dzt <- diff(zt)
    if(p1 > 1){
      tst2 <- adftest(dzt,lags=(p1-1))
      if(tst2 >= utcri) d <- 2
    }
  }

  dzt <- zt
  if(d==1)dzt <- diff(zt)
  if(d==2) dzt <- diff(diff(zt))

  tstmean <- t.test(dzt)
  include.mean <- TRUE
  if(tstmean$p.value > 0.05)include.mean=FALSE

  step1 <- ar(dzt,include.mean=include.mean,order.max=sum(maxorder))
  pmax <- step1$order

  qmax <- pureMA(dzt,maxq=12)

  choice <- c(0,d,0)
  if(pmax == 0)choice <- c(0,d,0)

  nT <- length(dzt)
  mincrit <- 99*nT
  mu <- mean(dzt)
  s1 <- sqrt(var(dzt))

  loglik <- sum(dnorm(dzt,mean=mu,sd=s1,log=TRUE))
  if(criterion=="aic"){
    crit = -loglik*2+2
  }else{ crit = -2*loglik+2*log(nT)
  }
  mincrit <- min(mincrit,crit)

  if(pmax > 0){
    pmax <- min(pmax,maxorder[1])

    for (i in 0:pmax){
      qmax1 <- min(maxorder[3],qmax)

      for (j in 0:qmax1){
        if((i+j) > 0){
          m1 <- try(suppressWarnings(arima(dzt,order=c(i,0,j),include.mean=include.mean,method=method)), silent = TRUE)

          if(as.character(class(m1)) == "try-error" & grepl("system is computationally singular", as.character(class(m1)))){
            warning("To avoid optimization problems the data has been scaled to zero mean and unit variance.", call. = FALSE)
            dzt <- scale(dzt)
            m1 <- suppressWarnings(arima(dzt,order=c(i,0,j),include.mean=include.mean,method=method))
          }

          if(as.character(class(m1)) == "try-error" & grepl("non-stationary AR part from CSS", as.character(class(m1)))){
            next
          } else {
            s1 <- sqrt(var(m1$residuals))
            loglik <- sum(dnorm(m1$residuals,mean=0,sd=s1,log=T))
            if(criterion=="aic"){
              crit <- (-loglik+2+i+j)*2
            }else{
              crit <- -loglik*2 +(2+i+j)*log(nT)
            }
            if(mincrit > crit){
              choice <- c(i,d,j)
              mincrit <- crit
            }
          }
        }
      }
    }
  }

  if(output){
    cat("include.mean: ",include.mean,"\n")
    cat("Selected order (p,d,q): ",choice,"\n")
    cat("minimum criterion: ",mincrit,"\n")
  }

  return(list(order = choice, crit = mincrit, include.mean = include.mean))
}

"adftest" <- function(x, lags = 2){

  if(lags < 1)lags <- 0
  x <- as.numeric(x)
  dx <- c(0,diff(x))
  nT <- length(x)
  effnob <- nT-lags-1
  ist <- lags+2
  y <- dx[ist:nT]
  X <- x[(ist-1):(nT-1)]
  if(lags > 0){
    for (i in 1:lags){
      X <- cbind(X,dx[(ist-i):(nT-i)])
    }
  }
  m1 <- lm(y~.,data=data.frame(X))
  tst <- summary(m1)$coefficients[2,3]

  tst
}

"pureMA" <- function(zt, maxq = 12){
  if(is.matrix(zt))zt <- zt[,1]
  rzt <- rank(zt)
  mm <- acf(rzt,lag.max=maxq+1,plot=FALSE)
  rhohat <- mm$acf[-1]
  nn <- length(rhohat)
  iend <- min(maxq,nn)
  nT <- length(zt)
  q <- iend
  Qm <- 0
  for (i in 1:nn){
    Qm <- Qm + rhohat[i]^2/(nT-i)
  }
  Qm <- Qm*nT*(nT+2)
  pv <- 1-pchisq(Qm,df=nn)
  if(pv > 0.1) q <- 0

  i = 1
  if (q >= iend){
    Qm <- Qm - nT*(nT+2)*rhohat[i]^2/(nT-i)
    pv <- 1-pchisq(Qm,df=(nn-i))
    if(pv > 0.1){q <- i
    }else{i <- i+1}
  }

  q
}
