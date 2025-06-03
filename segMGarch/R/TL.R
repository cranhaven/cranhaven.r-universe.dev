#' Method to backtest VaR violation using the Traffic Light (TL) approach of Basel
#' @references Basle Committee on Banking Supervision (1996). "Supervisory Framework for the Use of ‘Backtesting’ in Conjunction with the Internal Models Approach to Market Risk Capital Requirements".
#' @rdname TL-methods
#' @description A method that performs backtest for VaR models using the TL approach. According to Basel, a VaR model is deemed valid if the cumulative probability of observing up to
#' \eqn{n_f} failures is less than 0.95 (green zone) under the binomial distribution with \eqn{n} (sample size) and Var level as
#' the parameters. If the cumulative probability is between 0.95 and 0.9999 a VaR model is in yellow zone. Otherwise (>0.9999) a VaR model is in red zone.
#' @param y The time series to apply a VaR model (a single asset rerurn or portfolio return).
#' @param n If \code{y} is not provided, then insert sample size. Default is NULL.
#' @param no_fail If \code{y} is not provided, then insert number of fails. Default is NULL.
#' @param VaR The forecast VaR.
#' @param VaR_level The VaR level, typically 95\% or 99\%.
#' @examples
#' pw.CCC.obj = new("simMGarch")
#' pw.CCC.obj@d = 10
#' pw.CCC.obj@n = 1000
#' pw.CCC.obj@changepoints = c(250,750)
#' pw.CCC.obj = pc_cccsim(pw.CCC.obj)
#' y_out_of_sample = t(pw.CCC.obj@y[,900:1000])
#' w=rep(1/pw.CCC.obj@d,pw.CCC.obj@d) #an equally weighted portfolio
#' #VaR = quantile(t(pw.CCC.obj@y[,1:899])%*%w,0.05)
#' #ts.plot(y_out_of_sample%*%w,ylab="portfolio return");abline(h=VaR,col="red")
#' #TL(y=y_out_of_sample%*%w,VaR=rep(VaR,100),VaR_level = 0.95)
#' @import Rcpp foreach doParallel parallel iterators
#' @importFrom mvtnorm rmvnorm
#' @importFrom stats pbinom dbinom
#' @useDynLib segMGarch, .registration = TRUE
#' @export
#' @docType methods
#' @aliases TL TL-class TL-methods
setGeneric(name="TL",
           def=function(y,n=NULL,no_fail=NULL,VaR,VaR_level)
           {
             standardGeneric("TL")
           }
)
#' @rdname TL-methods
setMethod(f="TL", definition = function(y,n=NULL,no_fail=NULL,VaR,VaR_level) {
  if (is.null(n)) {
    y=tail(y,length(VaR))
    n=length(y)
  }
  if (is.null(no_fail)){
    hit=numeric(n)
    hit[y < VaR] = 1
    no_fail=sum(hit)
  }
  prob = pbinom(q=no_fail,size = n,prob = 1-VaR_level)
  typeIprob=1-prob+dbinom(x=no_fail,size = n,prob = 1-VaR_level)
  if (prob <= 0.95) {
    TL_color="green"
  } else if (prob>0.95 && prob <= 0.9999){
    TL_color="yellow"
  } else TL_color="red"
  res = list()
  res$prob = prob
  res$typeI = typeIprob
  res$color = TL_color
  return(res)
})