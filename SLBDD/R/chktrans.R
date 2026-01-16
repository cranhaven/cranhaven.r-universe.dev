#' Check for Possible Non-linear Transformations of a Multiple Time Series
#'
#' Check for possible non-linear transformations of a multiple time series, series by series.
#'
#' @param x T by k data matrix: T data points in rows with each row being data at a given time point,
#' and k time series in columns.
#' @param block  Number of blocks used in the linear regression. Default value is floor(sqrt(T)).
#' @param output If TRUE it returns the estimates, the code: log, sqrt and No-trans and
#' the numbers of non-linear transformations. Default is TRUE.
#' @param period Seasonal period.
#' @param pv P-value = pv/log(1 + k) is used to check the significance of the coefficients.
#' Default value is 0.05.
#'
#' @details
#' Each series is divided into a given number of consecutive blocks and in each of them
#' the mean absolute deviation (MAD) and the median are computed. A regression of
#' the log of the MAD with respect to the log of the median is run and the slope defines
#' the non-linear transformation.
#'
#' @return A list containing:
#' \itemize{
#'    \item lnTran - Column locations of series that require log-transformation.
#'    \item sqrtTran - Column locations of series that require square-root transformation.
#'    \item noTran - Column locations of series that require no transformation.
#'    \item tran - A vector indicating checking results, where 0 means no transformation,
#'    1 means log-transformation, 2 means square-root transformation.
#'    \item tranX Transformed series. This is only provided if the number of series
#'    requiring transformation is sufficiently large, i.e. greater than \eqn{2kpv}.
#'    \item Summary Number of time series that require log-transformation,
#'    square-root transformation and no transformation.
#' }
#'
#' @export
#'
#' @examples
#' data(TaiwanAirBox032017)
#' output <- chktrans(TaiwanAirBox032017[,1])
#'
"chktrans" <- function(x, block = 0, output = FALSE, period = 1, pv = 0.05){
  if(!is.matrix(x))x <- as.matrix(x)
  nT <- nrow(x)
  k <- ncol(x)
  Trans <- rep(0,k)

  if (block <= 0)block=floor(sqrt(nT))
  size <- floor(nT/block)
  if(size < period)size <- period
  if(size < 12){size = 12
  block <- floor(nT/size)
  }
  idxln <- idxsqrt <- idxNull <- NULL
  if (block > 3){
    minV <- apply(x,2,min)
    for (i in 1:k){
      if(minV[i] < 0.0)x[,i]=x[,i]+abs(minV[i])
    }
    Xm <- NULL
    Xmad <- NULL
    for (i in 1:block){
      ist <- (i-1)*size
      if(k > 1){
        xm <- apply(x[(ist+1):(ist+size),],2,median)
        xmad <- apply(x[(ist+1):(ist+size),],2,mad)
      }else{
        xm <- median(x[(ist+1):(ist+size),1])
        xmad <- mad(x[(ist+1):(ist+size),1])
      }
      Xm <- rbind(Xm,xm)
      Xmad <- rbind(Xmad,xmad)
    }
    n11 <- min(Xmad)
    n12 <- min(Xm)
    if(n11 < 0.0001)Xmad <- Xmad + 0.1
    if(n12 < 0.0001)Xm <- Xm + 0.1
    for (i in 1:k){
      m1 <- lm(log(Xmad[,i])~log(Xm[,i]))
      m1s <- summary(m1)
      coef <- m1s$coefficients[2,1]
      tstat <- m1s$coefficients[2,4]
      if(tstat < (pv/log(k+1))){
        if(coef > 0.7){Trans[i] <- 1
        }else{if(coef > 0.3)Trans[i] <- 2}
      }
      if(output){cat("Estimate: ",m1s$coefficients[2,],"\n")}
    }
    idxln <- c(1:k)[Trans==1]
    idxsqrt <- c(1:k)[Trans==2]
    idxNull <- c(1:k)[Trans==0]
    if(output){
      cat("code: (0,1,2): ","No-trans,log,sqrt","\n")
      print(Trans)
    }
  }else{
    message("Number of block is too small","\n")
  }
  if(output){
    cat("Numbers of (log,sqrt,no)-trans = ", c(length(idxln),length(idxsqrt),length(idxNull)),"\n")}

  ntr <- (length(idxln)+length(idxsqrt))/k
  tranX <- NULL
  if(ntr > 2*pv){
    tranX <- x
    for (i in 1:k){
      if(Trans[i]==1){
        tranX[,i] <- log(x[,i]+abs(min(x[,i])))
      }
      if(Trans[i]==2){
        tranX[,i] <- sqrt(x[,i]+abs(min(x[,i])))
      }
    }
    message("Transformed series are in the output. ","\n")
  }

  return(list(lnTran=idxln, sqrtTran=idxsqrt, noTran=idxNull, trans=Trans, tranX=tranX, Summary = c(length(idxln), length(idxsqrt), length(idxNull))))
}

