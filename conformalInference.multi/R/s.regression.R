#' Computing modulation function for residuals.
#'
#' It is an helper function for \code{\link{conformal.multidim.split}}  and
#' \code{\link{conformal.multidim.msplit}}
#'
#' @param mat_residual A vector of the residuals obtained via multivariate modeling.
#' @param type A string indicating the type of modulation function chosen.
#' The alternatives are "identity","st-dev","alpha-max".
#' @param alpha The value of the confidence interval.
#' @param tau A number between 0 and 1 used for the randomized version of the algorithm
#' @return It returns local scoring values for the residuals.
#' @description It computes values for local scoring.
#'
#' @importFrom stats cov sd var
#' @references "Conformal Prediction Bands for Multivariate Functional Data"
#' by Diquigiovanni, Fontana, Vantini (2021) <arXiv:2106.01792>.
#' @export computing_s_regression


computing_s_regression=function(mat_residual,type,alpha,tau){



  check.s_regression(mat_residual,type)
  q=dim(mat_residual)[2]


  #----naive cases: just one observation and type %in% c("st-dev","alpha-max")

  if(is.atomic(mat_residual)==TRUE & is.vector(mat_residual)==TRUE & type=="st-dev") stop("st-dev can not be computed when the number of observations is equal to 1.")
  if(is.atomic(mat_residual)==TRUE & is.vector(mat_residual)==TRUE & type=="alpha-max") {

    out=abs(mat_residual)
    return(out)
  }
  #---non-naive cases

  if (type=="identity"){
    out=rep(1,q)

    return(out)
  }

  if (type=="st-dev") {
    out=apply(mat_residual,2,sd)
    return(out)
  }

  if (type=="alpha-max"){


    check.num.01(tau)

    abs_mat_residual=abs(mat_residual)


    #----------------------------------------------CHECKS ON alpha-----------------------------------------------

    if(ceiling(dim(abs_mat_residual)[1]+tau-(dim(abs_mat_residual)[1]+1)*alpha) >= dim(abs_mat_residual)[1]) {
      out=apply(abs_mat_residual,2,max)
      return(out)}

    if(ceiling(dim(abs_mat_residual)[1]+tau-(dim(abs_mat_residual)[1]+1)*alpha) <= 0)           {
      out=rep(1,q)
      return(out)}

    #----------------------------------------------S ALPHA-MAX----------------------------------------------------

    sequence_sup=apply(abs_mat_residual,1,max)
    gamma=sort(sequence_sup,decreasing=FALSE)[ceiling(dim(abs_mat_residual)[1]+tau-(dim(abs_mat_residual)[1]+1)*alpha)]
    position_functions_in_H=which(sequence_sup <= gamma)
    out=apply(abs_mat_residual[position_functions_in_H,],2,max)
    return(out)

  }


}


extremes = function(l,tau,alpha,rho,k_s){

if ((ceiling(l+tau-(l+1)*alpha))==1) v=0
else{v=sum(sort(rho,decreasing=FALSE)[1:(ceiling(l+tau-(l+1)*alpha)-1)]==k_s)}
if ((ceiling(l+tau-(l+1)*alpha))==l) r=0
else{r=sum(sort(rho,decreasing=FALSE)[(ceiling(l+tau-(l+1)*alpha)+1):length(rho)]==k_s)}

return (tau > (alpha*(l+1)-floor(alpha*(l+1)-tau)+r)/(r+v+2))

}
