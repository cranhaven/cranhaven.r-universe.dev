#' Method to simulate correlated variables with change-points
#' @references
#' Cho, Haeran, and Karolos Korkas. "High-dimensional GARCH process segmentation with an application to Value-at-Risk." arXiv preprint arXiv:1706.01155 (2017).
#' @description An S4 method that takes a \code{simMGarch} object and outputs simulated correlated time series with a piecewise constant covariance matrix. 
#' The correlations are generated as \eqn{\sigma_{i, i'} =  \rho^{|i-i'|}} with \eqn{\rho} taking values from \eqn{(-1,1)}. The exact variables that will contain a change-point are
#' randomly selected and controlled by \code{r} in the \code{simMGarch} object.
#' @param object A \code{simMGarch} object.
#' @examples
#' cp=500
#' n=2000
#' pw.CCC.obj <- new("simMGarch")
#' pw.CCC.obj@changepoints=cp
#' pw.CCC.obj@n=n
#' pc_Sigma.obj <- pc_Sigma(pw.CCC.obj)
#' par(mfrow=c(1,2))
#' #requires corrplot library
#' #correlation matrix before the changepoint
#' #corrplot::corrplot.mixed(cor(pc_Sigma.obj@cor_errors[1:cp,]), order="hclust", tl.col="black")
#' #correlation matrix after the changepoint
#' #corrplot::corrplot.mixed(cor(pc_Sigma.obj@cor_errors[(cp+1):n,]), order="hclust", tl.col="black")
#' @import Rcpp foreach doParallel parallel iterators
#' @importFrom mvtnorm rmvnorm
#' @useDynLib segMGarch, .registration = TRUE
#' @export
#' @docType methods
#' @rdname pc_Sigma-methods
#' @aliases pc_Sigma pc_Sigma-class pc_Sigma-methods
setGeneric(name="pc_Sigma",
           def=function(object)
           {
             standardGeneric("pc_Sigma")
           }
)
#' @rdname pc_Sigma-methods
setMethod(f="pc_Sigma", signature= "simMGarch", definition = function(object) {
  tempSigma=diag(object@multp,object@d)
  #randomly select r*p processes with at least one change-point
  rpp = sample(1:object@d,floor(object@r*object@d), replace = FALSE)
  for(i in 1:floor(object@d/2)){ 
    for(j in 1:floor(object@d/2)){
      tempSigma[i,j]<-object@multp*(-.95)^abs(i-j)
    }}
  cor_errors<-mvtnorm::rmvnorm(object@n, sigma=tempSigma)  
  if (object@pw==TRUE){
    for (i in 1:length(object@changepoints)) {
      if (i == 1)   {     
        rpptemp=sample(rpp,length(rpp), replace = FALSE)
        rppL=head(rpptemp,floor(object@r*object@d*0.5))
        rppR=tail(rpptemp,floor(object@r*object@d*0.5))
        for (j in rppL){
          k=1
          cor_errors[1:object@changepoints[1],j]=cor_errors[1:object@changepoints[1],rppR[k]]
          k=k+1
        }
        next
      }
      if (i == length(object@changepoints)) {
        rpptemp=sample(rpp,length(rpp), replace = FALSE)
        rppL=head(rpptemp,floor(object@r*object@d*0.5))
        rppR=tail(rpptemp,floor(object@r*object@d*0.5))
        for (j in rppL){
          k=1
          cor_errors[(object@changepoints[length(object@changepoints)]+1):object@n,j]=cor_errors[(object@changepoints[length(object@changepoints)]+1):object@n,rppR[k]]
          k=k+1
        }
        break
      } else {
        rpptemp=sample(rpp,length(rpp), replace = FALSE)
        rppL=head(rpptemp,floor(object@r*object@d*0.5))
        rppR=tail(rpptemp,floor(object@r*object@d*0.5))
        for (j in rppL){
          k=1
          cor_errors[(object@changepoints[i]+1):object@changepoints[i+1],j]=cor_errors[(object@changepoints[i]+1):object@changepoints[i+1],rppR[k]]
          k=k+1
        }
      }
    }
  }
  object@cor_errors=cor_errors
  return(object)
})