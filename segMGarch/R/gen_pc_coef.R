#' A method to generate piecewise constant coefficients
#' @name gen_pc_coef-class
#' @param coef A vector of coefficients.
#' @param object A \code{simMGarch} object.
#' @description An auxilliary method to calculate piecewise constant coefficients for a user-specified vector of coefficients. The change-points are controlled
#' by the \code{changepoints} slot in the \code{simMGarch} object.
#' @references
#' Cho, Haeran, and Karolos Korkas. "High-dimensional GARCH process segmentation with an application to Value-at-Risk." arXiv preprint arXiv:1706.01155 (2018).
#' @examples
#' pw.CCC.obj <- new("simMGarch")
#' coef.vector <- gen_pc_coef(pw.CCC.obj,c(0.2,0.4))
#' ts.plot(coef.vector,main="piecewise constant coefficients",ylab="coefficient",xlab="time")
#' @import Rcpp foreach doParallel parallel iterators
#' @useDynLib segMGarch, .registration = TRUE
#' @export
#' @rdname gen_pc_coef-class
#' @aliases gen_pc_coef gen_pc_coef-class gen_pc_coef-methods
setGeneric(name="gen_pc_coef",
           def=function(object,coef)
           {
             standardGeneric("gen_pc_coef")
           }
)
#' @rdname gen_pc_coef-class
setMethod(f="gen_pc_coef", signature= "simMGarch", definition = function(object,coef) {
    num.breaks = length(object@changepoints)
    num.coeff = length(coef)
    if (length(coef)==1) coef=rep(coef,num.coeff)
    Coef.vec = c()
    for (i in 1:num.coeff) {
      if (i == 1){      
        Coef.vec=c(Coef.vec,rep(coef[1],object@changepoints[1]))
        next
      }
      if (i == length(coef)) {
        Coef.vec=c(Coef.vec,rep(tail(coef,1),object@n - object@changepoints[length(coef)-1]))
        break
      } else {
        Coef.vec=c(Coef.vec,rep(coef[i],object@changepoints[i] - object@changepoints[i-1]))
      }
    }
    return(Coef.vec)
  }
)