#' Check the Seasonality of Each Component of a Multiple Time Series
#'
#' Check the seasonality of each component of a multiple time series.
#'
#' @param x T by k data matrix: T data points in rows with each row being data at a given time point,
#' and k time series in columns.
#' @param period seasonal period. Default value is 12.
#' @param p Regular AR order. Default value is max(floor(log(T)),1).
#' @param alpha Type-I error for the t-ratio of seasonal coefficients. Default value is 0.05.
#' @param output If TRUE it returns if the series has seasonality. Default is TRUE.
#'
#' @details
#' Check the seasonality fitting a seasonal AR(1) model and a regular AR(p) model to a scalar
#' time series and testing if the estimated seasonal AR coefficient is significant.
#'
#' @return A list containing:
#' \itemize{
#'    \item Seasonal - TRUE or FALSE.
#'    \item period - Seasonal period.
#' }
#'
#' @examples
#' data(TaiwanAirBox032017)
#' output <- chksea(TaiwanAirBox032017[,1])
#' @import stats
#'
#' @export
"chksea" <- function(x, period = c(12), p = 0, alpha = 0.05, output = TRUE){
  #### Check the seasonality of each component of a  multiple time series
  ### checking for seasonality of each column
  ###
  if(!is.matrix(x))x <- as.matrix(x)
  k <- ncol(x)
  sea <- NULL
  Period <- NULL
  for (i in 1:k){
    m1 <- chksea1d(x[,i],period=period,p=p,alpha=alpha,output=FALSE)
    sea <- c(sea,m1$Seasonal)
    Period <- c(Period,m1$period)
  }
  if(output){cat("Seasonal series: ",sea,"\n")}

  return(list(Seasonal=sea,period=Period))
}

"chksea1d" <- function(x,period=c(12),p=0,alpha=0.01,output=TRUE){
  Seasonal <- FALSE; Per <- NULL
  if(length(period) > 0){
    if(is.matrix(x)) x <- as.numeric(x[,1])
    nT <- length(x)
    nfre <- length(period)
    if(p <= 0) p <- max(floor(log(nT)),1)
    pp <- min(period)-1
    if(p > pp)p <- pp
    ist <- max(period)+1
    y <- x[ist:nT]
    X <- NULL
    for (i in 1:nfre){
      X <- cbind(X,x[(ist-period[i]):(nT-period[i])])
    }

    for (i in 1:p){
      X <- cbind(X,x[(ist-i):(nT-i)])
    }
    colnames(X) <- c(paste("slag",period,sep=""),paste("lag",1:p,sep=""))
    m1 <- lm(y~.,data=data.frame(X))
    m1s <- summary(m1)
    Result <- NULL
    for (i in 1:nfre){
      pv <- m1s$coefficients[i+1,4]

      if(pv < alpha){Result <- c(Result,TRUE)
      }else{Result <- c(Result,FALSE)}
    }

    idx <- c(1:nfre)[Result==TRUE]
    Per <- NULL
    if(length(idx) > 0){ Seasonal <- TRUE
    Per <- max(period[idx])
    }

    if(output){
      cat("seasonal series: ",Seasonal,"\n")
      if(Seasonal){cat("period: ",Per,"\n")}
    }
  }

  return <- list(Seasonal=Seasonal,period=Per)
}
