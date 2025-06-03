#' Method to backtest VaR violation using the Kupiec statistics
#' @references Kupiec, P. "Techniques for Verifying the Accuracy of Risk Management Models." Journal of Derivatives. Vol. 3, 1995, pp. 73–84.
#' @rdname kupiec-methods
#' @description An S4 method that performs backtest for VaR models using the Kupiec statistics. For a sample of \eqn{n} observations, 
#' the Kupiec test statistics takes the form of likelihood ratio
#' 
#'\eqn{LR_{PoF}= -2 \log\left(\frac{(1-\alpha)^{T-n_f}\alpha^{n_f}}
#'                          {\left(1-\frac{n_f}{T}\right)^{T-n_f}\left(\frac{n_f}{T}\right)^{n_f}}\right)}
#'                          
#' \eqn{LR_{TFF}= -2 \log\left
#'  (\frac{\alpha(1-\alpha)^{t_f -1}} {\left ( \frac{1}{t_f}\right
#'  )\left ( 1- \frac{1}{t_f}\right )^{t_f-1}}\right),
#'}
#'
#' where \eqn{n_f} denotes the number of failures occurred and 
#' \eqn{t_f} the number of days until the first failure within the \eqn{n}
#'  observations. Under \eqn{H_0}, both \eqn{LR_{PoF}} and \eqn{LR_{TFF}} are asymptotically
#' \eqn{\chi^2_1}-distributed, and their exceedance of the critical value implies that the VaR model is inadequate.
#' @param y The time series to apply a VaR model (a single asset rerurn or portfolio return).
#' @param VaR The forecast VaR.
#' @param VaR_level The VaR level, typically 95\% or 99\%.
#' @param verbose If \code{TRUE} show the outcome. Default is \code{TRUE}.
#' @param test Choose between PoF or TFF. Default is \code{PoF}.
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
#' #kupiec(y_out_of_sample%*%w,rep(VaR,100),.95,verbose=TRUE,test="PoF")
#' @import Rcpp foreach doParallel parallel iterators
#' @importFrom mvtnorm rmvnorm
#' @importFrom stats pchisq qchisq
#' @useDynLib segMGarch, .registration = TRUE
#' @export
#' @docType methods
#' @aliases kupiec kupiec-class kupiec-methods
setGeneric(name="kupiec",
           def=function(y,VaR,VaR_level,verbose=TRUE,test="PoF")
           {
             standardGeneric("kupiec")
           }
)
#' @rdname kupiec-methods
setMethod(f="kupiec", definition = function(y,VaR,VaR_level,verbose=TRUE,test="PoF") {
    y=tail(y,length(VaR))
    N=length(y)
    hit=numeric(N)
    hit[y < VaR] = 1
    numOfFails=sum(hit)
    n=which(hit==1)[1]
    p=1-VaR_level
    if (test == "PoF"){
      lr=-2*log((((1-p)^(N-numOfFails))*(p^numOfFails))/((1-numOfFails/N)^(N-numOfFails)*(numOfFails/N)^numOfFails))  
    } else if (test == "TUFF"){
      lr=-2*log((p*(1-p)^(n-1))/((1/n)*(1-1/n)^(n-1)))  
    }
    if (lr>qchisq(VaR_level,1)){
      if (verbose) cat("VaR method  not accurate at:",p*100,"% level")
      return(round(c(1-pchisq(lr,df=1),lr,ifelse(test=="PoF",numOfFails,n)),3))
    }  else {
      if (verbose) cat("VaR method accurate at:",p*100,"% level")
      return(round(c(1-pchisq(lr,df=1),lr,ifelse(test=="PoF",numOfFails,n)),3))
    }
})