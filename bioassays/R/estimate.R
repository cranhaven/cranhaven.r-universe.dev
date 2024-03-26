#'@name estimate
#'@aliases estimate
#'@title Estimate Samples from Standard Curve
#'@description This function will estimate the unknown variable (example: concentration) based on a standard curve.
#'@usage estimate (data, colname = "blankminus", fitformula = fiteq, method = "linear/nplr")
#'
#'@param data data in dataframe format
#'@param colname column name whose values has to be estimated
#'@param fitformula formula used for fitting standard curve
#'@param method method = "linear" if standard curve is linear in nature. method = "nplr" if standard curve is nonparametric logistic curve.
#'
#'@details For linear standard curve 'fitformula' need to generated using \code{\link[stats]{lm}}.
#'For nonparametric logistic curve 'fitformula' need to generated using \code{\link[nplr]{nplr}}.
#'
#'@return A dataframe with estimated values added to right as a new column "estimated".
#'
#'@author A.A Palakkan
#'
#'@examples
#'## loading data
#'data(data_DF1)
#'
#'## Filtering standards
#'std<- dplyr::filter(data_DF1, data_DF1$id=="STD")
#'std <- aggregate(std$blankminus ~ std$concentration, FUN = mean )
#'colnames (std) <-c("con", "OD")
#'
#'## 3-parametric regression curve fitting
#'fit1<-nplr::nplr(std$con,std$OD,npars=3,useLog = FALSE)
#'
#'## Linear regression curve fitting
#'fit2<- stats::lm(formula = con ~ OD,data = std)
#'
#'## Estimating the 'blankminus'
#'## eg:1 Based on nonparametric logistic regression fitting
#'estimated_nplr <- estimate(data_DF1,colname = "blankminus",fitformula = fit1,method = "nplr")
#'
#'## eg:2 Based on linear regression fitting
#'estimated_lr<-estimate(data_DF1,colname="blankminus",fitformula=fit2,method="linear")
#'
#'@keywords math
#'
#'@importFrom nplr getEstimates
#'@importFrom stats coefficients
#'@export
#'
#'
#'

estimate<-function(data,colname="blankminus",fitformula=fiteq, method="linear/nplr"){

fiteq<-NULL

if(method=="nplr"){
  projected <- nplr::getEstimates(fitformula,targets=data[,colname],conf.level= .95)[c(3)]
  newlayout<-data.frame(cbind(data,estimated=projected))
  colnames(newlayout)[ncol(newlayout)]<-"estimated"
  return(newlayout)
}

if(method=="linear") {
  coe<-stats::coefficients(fitformula)
  names(coe)<-NULL
  xvalue<-data[,colname]
  projected<-coe[1]+(coe[2]*xvalue)
  newlayout<-data.frame(cbind(data,estimated=projected))
  colnames(newlayout)[ncol(newlayout)]<-"estimated"
  return(newlayout)
}

}
