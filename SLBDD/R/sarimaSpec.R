#' Automatic Modeling of a Scalar Seasonal Time Series
#'
#' Auto-model specification of a scalar seasonal time series. The period should be given.
#'
#' @param zt T by 1 vector of an observed scalar time series without missing values.
#' @param maxorder Maximum order of \eqn{(p,d,q)}. \eqn{p} is the AR order, \eqn{d} the degree of differencing,
#' and \eqn{q} The MA order. Default value is (2,1,3).
#' @param maxsea Maximum order of \eqn{(P,D,Q)}. \eqn{P} is the seasonal AR order, \eqn{D} the degree of seasonal differencing,
#' and \eqn{Q} the seasonal MA order. Default value is (1,1,1).
#' @param criterion Information criterion used for model selection. Either AIC or BIC.
#' Default is "bic".
#' @param period Seasonal period. The default is 12.
#' @param output If TRUE it returns the differencing order, the selected order and the minimum
#' value of the criterion. Default is TRUE.
#' @param method Estimation method. See the arima command in R. Possible values are "CSS-ML", "ML", and "CSS".
#' Default is "CSS-ML".
#' @param include.mean Should the model include a mean/intercept term? Default is TRUE.
#'
#'@details ADF unit-root test is used to assess seasonal and regular differencing.
#' For seasonal unit-root test, critical value associated with pv = 0.01 is used.
#'
#' @return A list containing:
#' \itemize{
#'    \item data - The time series. If any transformation is taken, "data" is the transformed series.
#'    \item order - Regular ARIMA order.
#'    \item sorder - Seasonal ARIMA order.
#'    \item period - Seasonal period.
#'    \item include.mean - Switch about including mean in the model.
#' }
#'
#' @examples
#' data(TaiwanAirBox032017)
#' output <- sarimaSpec(TaiwanAirBox032017[1:100,1])
#'
#' @import stats
#'
#' @export
"sarimaSpec" <- function(zt, maxorder = c(2,1,3), maxsea = c(1,1,1), criterion = "bic",
                         period = 12, output = FALSE,  method = "CSS-ML", include.mean = TRUE){

  if(is.matrix(zt))zt <- zt[,1]
  nn <- s.adftest(zt,period=period,lags=max(floor(period/2)-1,maxorder[1]))
  D <- d <- 0
  if(nn[1] >= -3.43)D <- 1
  if(nn[2] >= -3.43)d <- 1

  if((d==1) && (D==0)){
    tmp <- diff(zt)
    tst <- adftest(tmp,lags=(period+maxorder[1]-1))
    if(tst >= -3.43) d <- 2
  }

  choice <- c(0,d,0,0,D,0)
  dz <- zt
  if(d > 0){
    for (i in 1:d){
      dz <- diff(dz)
    }
  }
  if(D == 1)dz=diff(dz,period)
  mm <- t.test(dz)
  if(mm$p.value < 0.05){
    include.mean=TRUE
  }else{include.mean=FALSE}

  nT <- length(dz)
  mincrit <- 99*nT
  mu <- mean(dz)
  s1 <- sqrt(var(dz))

  loglik <- sum(dnorm(dz,mean=mu,sd=s1,log=TRUE))
  if(criterion=="aic"){
    crit = (-loglik+2)*2
  }else{ crit = -2*loglik+2*log(nT)
  }
  mincrit <- min(mincrit,crit)

  if(maxsea[1] > 2)maxsea[1]=2
  if(maxsea[3] > 1)maxsea[3]=1

  for (ii in 0:maxsea[1]){
    for (jj in 0:maxsea[3]){
      for (i in 0:maxorder[1]){
        for (j in 0:maxorder[3]){
          if((i+j+ii+jj) > 0){

            m1 <- try(suppressWarnings(arima(dz,order=c(i,0,j),seasonal=list(order=c(ii,0,jj),period=period),include.mean=include.mean,method=method)), silent = TRUE)

            if(as.character(class(m1)) == "try-error" & grepl("system is computationally singular", as.character(class(m1)))){
              warning("To avoid optimization problems the data has been scaled to zero mean and unit variance.")
              dzt <- scale(dz)
              m1 <- suppressWarnings(arima(dz,order=c(i,0,j),seasonal=list(order=c(ii,0,jj),period=period),include.mean=include.mean,method=method))
            }

            if(as.character(class(m1)) == "try-error" & grepl("non-stationary AR part from CSS", as.character(class(m1)))){
              next
            } else {

              if(criterion=="aic"){
                crit <- (-m1$loglik+2+i+j+ii+jj)*2
              }else{
                crit <- -m1$loglik*2 +(2+i+j+ii+jj)*log(nT)
              }
              if(mincrit > crit){
                choice <- c(i,d,j,ii,D,jj)
                mincrit <- crit
              }
            }
          }
        }
      }
    }
  }
  #
  if(output){
    cat("Selected order (p,d,q,P,D,Q): ",choice,"\n")
    cat("minimum criterion: ",mincrit,"\n")
  }

  return(list(order=choice, crit=mincrit, include.mean=include.mean))
}

"s.adftest" <- function(x,period=12,lags=1){

  if(lags < 1)lags <- 0
  if(period < 2)period=2
  x <- as.numeric(x)
  dx <- c(0,diff(x))
  sdx <- c(rep(0,period),diff(dx,period))

  nT <- length(x)
  effnob <- nT-lags-period
  ist <- lags+1+period+1
  y <- sdx[ist:nT]
  X <- dx[(ist-period):(nT-period)]
  X <- cbind(X,x[(ist-1):(nT-1)])
  if(lags > 0){
    for (i in 1:lags){
      X <- cbind(X,sdx[(ist-i):(nT-i)])
    }
  }
  m1 <- lm(y~.,data=data.frame(X))
  sm1 <- summary(m1)

  tsts <- sm1$coefficients[2,3]
  tstr <- sm1$coefficients[3,3]
  tst <- c(tsts,tstr)

  tst
}
