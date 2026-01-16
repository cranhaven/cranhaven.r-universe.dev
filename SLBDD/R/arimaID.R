#' Automatic Modeling of a Scalar Time Series
#'
#' Automatic selection and estimation of a regular or possibly seasonal ARIMA model for a given time series.
#'
#' @param zt T by 1 vector of an observed scalar time series without any missing values.
#' @param maxorder Maximum order of \eqn{(p,d,q)} where \eqn{p} is the AR order, \eqn{d} the degree of differencing,
#' and \eqn{q} the MA order. Default value is (5,1,4).
#' @param criterion Information criterion used for model selection. Either AIC or BIC.
#' Default is "bic".
#' @param period Seasonal period. Default value is 12.
#' @param output If TRUE it returns the differencing order, the selected order and the minimum
#' value of the criterion. Default is TRUE.
#' @param method Estimation method. See the arima command in R. Possible values are "CSS-ML", "ML", and "CSS".
#' Default is "CSS-ML".
#' @param pv P-value for unit-root test. Default value is 0.01.
#' @param spv P-value for detecting seasonality. Default value is 0.01.
#' @param transpv P-value for checking non-linear transformation. Default value is 0.05.
#' @param nblock Number of blocks used in checking non-linear transformations. Default value is floor(sqrt(T)).
#'
#' @details
#' The program follows the following steps:
#' \itemize{
#' \item Check for seasonality: fitting a multiplicative ARIMA(p,0,0)(1,0,0)_s model to a scalar time
#' series and testing if the estimated seasonal AR coefficient is significant.
#' \item Check for non-linear transformation: the series is divided into a given number of consecutive blocks and in each
#' of them the Mean Absolute Deviation (MAD) and the median is computed. A regression of the log of the
#' MAD with respect to the log of the median is run and the slope defines the non-linear transformation.
#' \item Select orders: maximum order of \eqn{(p,d,q)}.
#' }
#'
#' @return A list containing:
#' \itemize{
#'    \item data - The time series. If any non-linear transformation is taken, "data" is the transformed series.
#'    \item order - Regular ARIMA order.
#'    \item sorder - Seasonal ARIMA order.
#'    \item period - Seasonal period.
#'    \item include.mean - Switch concerning the inclusion of mean in the model.
#' }
#'
#' @examples
#' data(TaiwanAirBox032017)
#' fit <- arimaID(TaiwanAirBox032017[,1])
#'
#' @import stats
#'
#' @export
"arimaID" <- function(zt, maxorder = c(5,1,3), criterion = "bic", period = c(12), output = TRUE, method = "CSS-ML", pv = 0.01, spv = 0.01, transpv = 0.05, nblock = 0){

  if(is.matrix(zt)){
    zt <- c(zt[,1])
    message("This command only works for scalar series. The first column is used.", "\n")
  }
  m1 <- chksea(zt,period=period,output=FALSE,alpha=spv)
  if(m1$Seasonal){
    message("Seasonality found with period: ",m1$period,"\n")
    period=m1$period
    m2 <- chktrans(zt,period=period,output=FALSE,pv=transpv,block=nblock)
    if(m2$trans[1]==1){
      minzt <- min(zt)
      if(minzt > 0){zt <- log(zt)
      message("Take log transformation!","\n")
      }else{
        zt <- log(zt+abs(minzt)+1)
        message("Add abs(min)+1, then take log transformation!","\n")
      }
    }
    if(m2$trans[1]==2){
      minzt <- min(zt)
      if(minzt > 0){zt <- sqrt(zt)
      message("Take square-root transformation!","\n")
      }else{
        zt <- sqrt(zt+abs(minzt))
        message("Add abs(min), then take square-root transformation!","\n")
      }
    }
    m3 <- sarimaSpec(zt,period=period,output=FALSE,criterion=criterion,method=method)
    order=m3$order[1:3]
    sorder=m3$order[4:6]
    include.mean <- m3$include.mean
    if(output){
      message("Selected model: ","\n")
      message("Seasonal with period: ",period,"\n")
      message("Regular order(p,d,q): ", paste(order, collapse = " "),"\n")
      message("Seasonal order(P,D,Q): ", paste(sorder, collapse = " "),"\n")
    }
  }else{
    period=1
    m2 <- chktrans(zt,period=period,output=FALSE,pv=transpv,block=nblock)
    if(m2$trans[1]==1){
      minzt <- min(zt)
      if(minzt > 0){zt <- log(zt)
      message("Take log transformation!","\n")
      }else{
        zt <- log(zt+abs(minzt)+1)
        message("Add abs(min)+1, then take log transformation!","\n")
      }
    }
    if(m2$trans[1]==2){
      minzt <- min(zt)
      if(minzt > 0){zt <- sqrt(zt)
      message("Take square-root transformation!","\n")
      }else{
        zt <- sqrt(zt+abs(minzt))
        message("Add abs(min), then take square-root transformation!","\n")
      }
    }

    m3 <- arimaSpec(zt,maxorder=maxorder,criterion=criterion,output=FALSE,method=method,pv=pv)
    order=m3$order
    sorder <- NULL
    include.mean <- m3$include.mean
    if(output){
      cat("Selected ARIMA order(p,d,q): ", paste(order, collapse = " "),"\n")
      cat("Include.mean: ",include.mean,"\n")
    }
  }

  arimaID <- list(data=zt,order=order,sorder=sorder,period=period,include.mean=include.mean)
}
