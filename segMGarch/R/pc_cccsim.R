#' A method to simulate nonstationary high-dimensional CCC GARCH models.
#' @name pc_cccsim-class
#' @param object a simMGarch object
#' @description A S4 method that takes as an input a \code{simMGarch} object and outputs a simulated nonstationary CCC model. The formulation of the of the 
#' piecewise constant CCC model is given in the \code{simMGarch} class.
#' @references
#' Cho, Haeran, and Karolos Korkas. "High-dimensional GARCH process segmentation with an application to Value-at-Risk." arXiv preprint arXiv:1706.01155 (2018).
#' @examples
#' pw.CCC.obj <- new("simMGarch")
#' pw.CCC.obj <- pc_cccsim(pw.CCC.obj)
#' par(mfrow=c(1,2))
#' ts.plot(pw.CCC.obj@y[1,],main="a single simulated time series",ylab="series")
#' ts.plot(pw.CCC.obj@h[1,],main="a single simulated conditional variance",ylab="variance")
#' @import Rcpp foreach doParallel parallel iterators
#' @importFrom stats rnorm acf rgeom
#' @importFrom utils head tail 
#' @useDynLib segMGarch, .registration = TRUE
#' @export
#' @docType methods
#' @rdname pc_cccsim-methods
#' @aliases pc_cccsim pc_cccsim-class pc_cccsim-methods
setGeneric(name="pc_cccsim",
           def=function(object)
           {
             standardGeneric("pc_cccsim")
           }
)
#' @rdname pc_cccsim-methods
setMethod(f="pc_cccsim", signature= "simMGarch", definition = function(object) {
    object@n=object@n+object@BurnIn
    object@changepoints=object@changepoints+object@BurnIn
    e.mat = pc_Sigma(object)
    e.mat = e.mat@cor_errors
    A0=A1=B1=Y=H=matrix(0,object@n,object@d)
    for (i in 1:object@d){
      A0[,i]=gen_pc_coef(object,object@a0)
      A1[,i]=gen_pc_coef(object,object@a1)
      B1[,i]=gen_pc_coef(object,object@b1)
    }
    Y[1,] = rnorm(object@d)
    H[1,] = sqrt(A0[1,]+A1[1,]*rnorm(object@d)^2)  
    for (i in 2:object@n) {
      H[i,]=sqrt(A0[i,]  +A1[i,]*(Y[i-1,]^2) + B1[i,]*H[i-1,]^2)
      Y[i,]=H[i,]*e.mat[i,]
    }
    object@changepoints=object@changepoints-object@BurnIn
    object@y = t(Y[-(1:object@BurnIn),])
    object@h = t(H[-(1:object@BurnIn),])
    return(object)
  }
)

