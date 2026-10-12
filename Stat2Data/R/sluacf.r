#' Computes autocorrelations (ACF) for a time series
#'
#' @param series  a time series object
#' @param lags   a multiplier for the lag.  For example, use lag=12 for monthly data.
#' @param maxlag  maximum number of lags to compute
#' @param ndiff   number of regular differences to take before finding the ACF
#' @param sdiff   number of seasonal differences (with seasonal period specified by lags)
#'
#' @return An object of class "acf"
#'
#' @description
#' This function computes autocorrelations for various lags of a time series.
#'
#' @details
#' This is is a wrapper for the acf function which allows for specifying
#' regular (ndiff) and seasonal (sdiff) differences.
#' The lags parameter specifies the seasonal lag and adjusts the default lags in the returned acf object
#' to go 1, 2, ..., rather than showing fractional lags (for example, 1/12, 2/12, ... for monthly data).
#' The lag 0 autocorrelation is set to NA (rather than 1) so that it won;t show when acf is plotted.
#'
#'
#' @examples
#' data(SeaIce)
#' ExtentY=ts(SeaIce$Extent,start=1979)
#' sluacf(ExtentY)
#' sluacf(ExtentY, maxlag=8,ndiff=1)
#'
#' data(Inflation)
#' CPIts=ts(Inflation$CPI,start=c(2009,1),frequency=12)
#' CPIacf=sluacf(CPIts,maxlag=36,lags=12)
#' plot(CPIacf,lwd=2,ci.type="ma",xlim=c(1,36),xaxp=c(0,36,6),main="")
#'
#' @export

sluacf=function(series,lags=1,maxlag=NULL,ndiff=0,sdiff=0){
  if(ndiff>0){series=diff(series,differences=ndiff)}
  if(sdiff>0){series=diff(series,lag=lags,differences=sdiff)}
  seriesacf=acf(series,lag.max=maxlag,plot=F)
  seriesacf$acf[1]=NA
  if(lags>1){seriesacf$lag=seriesacf$lag*lags}
  return(seriesacf)
}
