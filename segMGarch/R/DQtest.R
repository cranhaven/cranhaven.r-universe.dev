#' A regression-based test to backtest VaR models proposed by Engle and Manganelli (2004)
#' @references Engle, Robert F., and Simone Manganelli. "CAViaR: Conditional autoregressive value at risk by regression quantiles." Journal of Business & Economic Statistics 22, no. 4 (2004): 367-381.
#' @rdname DQtest-methods
#' @description Typical VaR tests cannot control for the dependence of violations, i.e., violations may cluster while the overall (unconditional) average of violations is not significantly different from \eqn{\alpha = 1-VaR}. The conditional expectation should also be zero meaning that \eqn{Hit_t(\alpha)} is uncorrelated with its own past and other lagged variables (such as \eqn{r_t},
#'\eqn{r_t^2} or the one-step ahead forecast VaR). To test this assumption, the dynamic conditional quantile (DQ) test is used which involves the following statistic
#'\eqn{DQ = Hit^T X(X^T X)^{-1} X^T Hit/ \alpha(1-\alpha)}
#'where \eqn{X} is the matrix of explanatory variables (e.g., raw and squared past returns) and \eqn{Hit} the vector collecting \eqn{Hit_t(\alpha)}. Under the null hypothesis, Engle and Manganelli (2004) show that the proposed
#'statistic \eqn{DQ} follows a \eqn{\chi^2_q} where \eqn{q = rank(X)}.
#' @param y The time series to apply a VaR model (a single asset rerurn or portfolio return).
#' @param VaR The forecast VaR.
#' @param VaR_level The VaR level, typically 95\% or 99\%.
#' @param lag The chosen lag for y.Default is 1.
#' @param lag_hit The chosen lag for hit. Default is 1.
#' @param lag_var The chosen lag for VaR forecasts. Default is 1.
#' @examples
#' #VaR_level=0.95
#' #y=rnorm(1000,0,4)
#' #VaR=rep(quantile(y,1-VaR_level),length(y))
#' #y[c(17,18,19,20,100,101,102,103,104)]=-8
#' #lag=5
#' #DQtest(y,VaR,VaR_level,lag)
#' @import Rcpp foreach doParallel parallel iterators
#' @importFrom stats pbinom dbinom
#' @useDynLib segMGarch, .registration = TRUE
#' @export
#' @docType methods
#' @aliases DQtest DQtest-class DQtest-methods
setGeneric(name="DQtest",
           def=function(y, VaR,VaR_level,lag=1,lag_hit=1,lag_var=1)
           {
             standardGeneric("DQtest")
           }
)
#' @rdname DQtest-methods
setMethod(f="DQtest", definition = function(y, VaR,VaR_level,lag=1,lag_hit=1,lag_var=1) {
  y=tail(y,length(VaR))
  length_y=length(y)
  hit=numeric(length_y)
  VaR_level = 1 - VaR_level #change to lower quantile
  hit[y < VaR] = 1-VaR_level
  hit[y > VaR] = -VaR_level
  x0=rep(1,(length_y -lag))
  hit_ahead=hit[(lag_hit+1):length_y]
  VaR_ahead=VaR[(lag_var+1):length_y]
  hit_lag=matrix(0,length_y - lag_hit,lag_hit)
  y_lag=y[lag :(length_y-1)]^2
  for (i in 1:lag_hit) hit_lag[,i]=hit[i:(length_y-(lag_hit+1-i))]
  len_VaR_ahead=length(VaR_ahead)
  len_hit_lag=dim(hit_lag)[1]
  len_y_lag=length(y_lag)
  min_len=min(len_VaR_ahead,len_hit_lag,len_y_lag)
  x0=rep(1,min_len)
  VaR_ahead=tail(VaR_ahead,min_len)
  hit_lag=tail(hit_lag,min_len)
  y_lag=tail(y_lag,min_len)
  X=cbind(x0,VaR_ahead,hit_lag,y_lag)
  hit_ahead=tail(hit_ahead,min_len)
  DQOOS=(t(hit_ahead)%*%X%*%MASS::ginv(t(X)%*%X)%*%t(X)%*%(hit_ahead))/(VaR_level*(1-VaR_level))
  DQOOS_p=1-pchisq(DQOOS,ncol(X))
  out=list()
  out[[1]]=DQOOS
  out[[2]]=DQOOS_p
  return(out)
})