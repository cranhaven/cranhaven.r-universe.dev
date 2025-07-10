#' Test whether two ZIC values differ significantly based on minimum ZIC for time series data
#'
#' @param x time series data (maximum of 1000 data points).
#' @param model1 AR and MA coefficients of Model 1.
#' @param model2 AR and MA coefficients of Model 2.
#' @param model_ZIC type of the information criterion, it can be "AIC", "BIC", or "AICc" (Default is the "AIC").
#' @param alpha significance level \eqn{\alpha} for the hypothesis testing (Default is 0.05).
#'
#' @return p-value with significance status.
#' @export
#' @description Test whether two ZIC values differ significantly based on minimum ZIC for time series data.
#' @details Consider the hypothesis: Under the null hypothesis that the two expected discrepancies are equal.
#' @details \deqn{H_0: ZIC_i=ZIC_j      ,    H_1: ZIC_i\neq ZIC_j}
#' @details \deqn{Z_0=\frac{(\hat{ZIC_i}-\hat{ZIC_j})-0}{\sqrt{SD(ZIC_i,ZIC_j)}} \sim N(0,1)}  is calculated empirically.
#'
#'
#' @usage tsZIC.test(x,model1,model2,model_ZIC="AIC",alpha=0.05)
#'
#' @importFrom stats pnorm
#' @importFrom stats qnorm
#' @importFrom stats arima
#' @importFrom stats toeplitz
#' @importFrom stats logLik
#' @importFrom ltsa tacvfARMA
#' @importFrom ltsa TrenchInverse
#' @importFrom tidytable crossing
#' @importFrom psych tr
#' @references Linhart, H. (1988). A test whether two AIC's differ significantly. South African Statistical Journal, 22(2), 153-161.
#' @examples
#' library(ConfZIC)
#' data(Sunspots)
#' x=Sunspots
#' model1=try(arima(x,order=c(1,0,1),method="ML",include.mean=FALSE),silent = TRUE)
#' model2=try(arima(x,order=c(1,0,0),method="ML",include.mean=FALSE),silent = TRUE)
#' tsZIC.test(x,model1,model2,model_ZIC="AIC",alpha=0.05)

tsZIC.test<-function(x,model1,model2,model_ZIC="AIC",alpha=0.05){

  if (alpha<0 || alpha>1) {stop("Significance level should be between 0 and 1!")}
  if (length(x)>=1000){stop("No of data points are out of range: need to less than 1000")}
  if (model_ZIC!= "AIC" && model_ZIC!= "BIC" && model_ZIC!= "AICc"){stop("model_ZIC should be either AIC,BIC, or AICc")}


  n=length(x)
  fit1=model1
  fit2=model2

  alpha=alpha

  phival1=fit1$model$phi
  thetaval1=fit1$model$theta
  phival2=fit2$model$phi
  thetaval2=fit2$model$theta

  if (length(phival1)==0) {phi1=0} else { phi1=phival1}
  if (length(thetaval1)==0) {theta1=0} else {theta1=thetaval1}
  if (length(phival2)==0) {phi2=0} else {phi2=phival2}
  if (length(thetaval2)==0) {theta2=0} else {theta2=thetaval2}

  ########################
  gval1<-try(tacvfARMA(phi1,-theta1,n-1))
  Cov_gval1<-toeplitz(gval1[1:n])
  CovInv_gval1=TrenchInverse(Cov_gval1)

  gval2<-try(tacvfARMA(phi2,-theta2,n-1))
  Cov_gval2<-toeplitz(gval2[1:n])
  CovInv_gval2=TrenchInverse(Cov_gval2)

  ############################################
  pt1=length(fit1$coef)
  pt2=length(fit2$coef)


  pt1=length(fit1$coef)
  pt2=length(fit2$coef)

  if (model_ZIC=="AIC"){
    GIC1=(-2/n) * fit1$loglik +pt1/n
    GIC2=(-2/n) * fit2$loglik +pt2/n
  }


  if (model_ZIC=="BIC"){
    GIC1=(-2/n) * fit1$loglik +pt1*log(n)/(2*n)
    GIC2=(-2/n) * fit2$loglik +pt2*log(n)/(2*n)
  }

  if (model_ZIC=="AICc"){

    GIC1=(-2/n) * fit1$loglik +pt1/(n-pt1-1)
    GIC2=(-2/n) * fit2$loglik +pt2/(n-pt2-1)
  }

  GIC1=GIC1*n
  GIC2=GIC2*n

  minGIC=min(GIC1,GIC2)

  if (minGIC==GIC1) Sigma_f=Cov_gval1
  if (minGIC==GIC2) Sigma_f=Cov_gval2




  # calculate the covariance matrix
  sigma_11=(2/n^2)*tr(Sigma_f%*%CovInv_gval1%*%Sigma_f%*%CovInv_gval1)
  sigma_12=(2/n^2)*tr(Sigma_f%*%CovInv_gval1%*%Sigma_f%*%CovInv_gval2)
  sigma_22=(2/n^2)*tr(Sigma_f%*%CovInv_gval2%*%Sigma_f%*%CovInv_gval2)



  sd=sqrt((sigma_11+sigma_22-2*sigma_12)/n)
  Diff_GIC=-(GIC1-GIC2)
  val=Diff_GIC/sd
  p_value=2* pnorm(abs(val),lower.tail = FALSE)

  #calculate the z value

  Zval=qnorm(1-alpha/2)

  if (p_value<alpha){Sig=1} else {Sig=0}

  if (Sig==1){final_pvalue=round(as.numeric(p_value),6); sig_value="significant"}
  if (Sig==0){final_pvalue=round(as.numeric(p_value),6); sig_value=" not significant"}

  message("p-value is ",p_value,  ", and test is " ,sig_value, " at level ", alpha)



}

