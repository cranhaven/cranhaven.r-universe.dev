#' Cook and Vougas(2009) nonlinear unit root test function
#'
#' This function allows you to make Cook and Vougas(2009) nonlinear unit root test
#' @param x series name,
#' @param model if model A 1, if model B 2, if model C 3, model D 4
#' @param max_lags maximum lag(optimal lag selected by AIC)
#' @keywords nonlinear unit root test
#' @export
#' @importFrom car linearHypothesis
#' @importFrom stats AIC
#' @importFrom minpack.lm nlsLM
#' @importFrom tsDyn setar
#' @examples
#'\donttest{
#'set.seed(12345)
#'x <- rnorm(1000)
#'Cook_Vougas_2009_unit_root(x,model=1,max_lags=3)
#'
#'data(IBM)
#'Cook_Vougas_2009_unit_root(x=IBM,model=3,max_lags=3)
#'
#'}
#'


Cook_Vougas_2009_unit_root<-function(x,model,max_lags){

  if (model==1){
    try({
      n=length(x)
      trend<-seq(0,length(x)-1,1)
      nonlin_model=nlsLM(x ~ a1 + a2*(1/(1+exp(-a3*(trend-(a4*n))))),start=list(a1=0,a2=0,a3=1,a4=0.5),control = nls.control(maxiter = 500))
      res=residuals(nonlin_model)
    },silent = T)
  }
  if (model==2){
    try({
      n=length(x)
      trend<-seq(0,length(x)-1,1)
      nonlin_model=nlsLM(x ~ a1 + a2*(1/(1+exp(-a3*(trend-(a4*n))))) + a5*trend,start=list(a1=0,a2=0,a3=1,a4=0.5,a5=0),control = nls.control(maxiter = 500))
      res=residuals(nonlin_model)
    },silent = T)
  }
  if (model==3){
    try({
      n=length(x)
      trend<-seq(0,length(x)-1,1)
      nonlin_model=nlsLM(x ~ a1 + a2*(1/(1+exp(-a3*(trend-(a4*n))))) + a5*trend + a6*trend*(1/(1+exp(-a3*(trend-(a4*n))))), start=list(a1=0,a2=0,a3=1,a4=0.5,a5=0,a6=0),control = nls.control(maxiter = 500))
      res=residuals(nonlin_model)
    },silent = T)
  }
  if (model==4){
    try({
      n=length(x)
      trend<-seq(0,length(x)-1,1)
      nonlin_model=nlsLM(x ~ a1 + a6*trend*(1/(1+exp(-a3*(trend-(a4*n))))), start=list(a1=0,a3=1,a4=0.5,a6=0),control = nls.control(maxiter = 500))
      res=residuals(nonlin_model)
    },silent = T)
  }
  say=99999999999999999999999999999

  for(i in 1:max_lags){

    model = setar(res,m=max_lags,model="MTAR",th=0,type="ADF",common="both", include = "none")

    if (AIC(model)<say){
      uygun_lag=i
      say=AIC(model)
    }
    model = setar(res,m=uygun_lag,model="MTAR",th=0,type="ADF",common="both", include = "none")
  }

  p1 = linearHypothesis(model, c("phiL.1=0", "phiH.1=0"), test="F")
  p2 = linearHypothesis(model, c("phiL.1=phiH.1"), test="F")
  tval=p1[,"Chisq"][2]
  tval2=p2[,"Chisq"][2]
  tval2ol=p2[,"Pr(>Chisq)"][2]
  my_list <- list("Model"=summary(model),"Selected lag"=uygun_lag,"p1=p2=0 Statistic"=tval/2)
  return(my_list)
}



