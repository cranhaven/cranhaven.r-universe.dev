#' Test whether two ZIC values differ significantly based on minimum ZIC for regression data
#'
#' @param model1 an object of class ``lm".
#' @param model2 an object of class ``lm".
#' @param data a matrix of \eqn{n} by \eqn{(m+1)} where \eqn{m} is the number of independent variables.First column should be the dependent variable and the rest of the \eqn{m} columns should be the independent variables of the dataset. Maximum of \eqn{m} should be 10.
#' @param model_ZIC type of the information criterion, it can be "AIC", "BIC", or "AICc" (Default is the "AIC").
#' @param alpha significance level \eqn{\alpha} for the hypothesis testing (Default is 0.05).
#' @return p-value with significance status.
#' @export
#'
#' @description Test whether two ZIC values differ significantly based on minimum ZIC for regression data.
#' @details Consider the hypothesis: Under the null hypothesis that the two expected discrepancies are equal.
#' @details \deqn{H_0: ZIC_i=ZIC_j     ,    H_1: ZIC_i\neq ZIC_j}
#' @details \deqn{Z_0=\frac{(\hat{ZIC_i}-\hat{ZIC_j})-0}{\sqrt{SD(ZIC_i,ZIC_j)}}\sim N(0,1)}  is calculated empirically.
#'
#' @usage regZIC.test(model1,model2,model_ZIC="AIC",data,alpha=0.05)
#'
#' @importFrom stats pnorm
#' @importFrom stats qnorm
#' @references Linhart, H. (1988). A test whether two AIC's differ significantly. South African Statistical Journal, 22(2), 153-161.
#' @examples library(ConfZIC)
#' data(Concrete)
#' x=Concrete
#' Y=x[,9] #dependent variable
#' #independent variables
#' X1=x[,1];X2=x[,2];X3=x[,3];X4=x[,4];
#' X5=x[,5];X6=x[,6];X7=x[,7];X8=x[,8];
#' mydata=cbind(Y,X1,X2,X3,X4,X5,X6,X7,X8) #data matrix
#' model1=lm(Y~X1); model2=lm(Y~X1+X2)
#' regZIC.test(model1,model2,model_ZIC="BIC",data=mydata,alpha=0.05)


regZIC.test<-function(model1,model2,model_ZIC="AIC",data,alpha=0.05){
  Y=NULL

  if (alpha<0 || alpha>1) {stop("Significance level should be between 0 and 1!")}
  if (ncol(data)>=10){stop("No of data points are out of range: m should be less than 10")}
  if (model_ZIC!= "AIC" && model_ZIC!= "BIC" && model_ZIC!= "AICc"){stop("model_ZIC should be either AIC,BIC, or AICc")}


  n=nrow(data)
  alpha=alpha

  model_ZIC=model_ZIC

  M1=model1; M2=model2;


  if (model_ZIC=="AIC"){
    GIC1=(-2/n) *(-logLik(M1)) +2*M1$rank/n
    GIC2=(-2/n) *(-logLik(M2)) +2*M2$rank/n
  }


  if (model_ZIC=="BIC"){
    GIC1=(-2/n) *(-logLik(M1)) +2*M1$rank*log(n)/(2*n)
    GIC2=(-2/n) *(-logLik(M2)) +2*M2$rank*log(n)/(2*n)
  }

  if (model_ZIC=="AICc"){
    GIC1=(-2/n) * (-logLik(M1)) +2*M1$rank/(n-M1$rank-1)
    GIC2=(-2/n) * (-logLik(M2)) +2*M2$rank/(n-M2$rank-1)
  }


  a1=summary(M1)
  a2=summary(M2)
  M1_logL=((-1/2)*log(2*pi)-((1/(2*(a1$sigma^2)))*((Y-M1$fitted.values)^2))-log(sqrt(a1$sigma^2)))
  M1_variance=(1/n)*sum(M1_logL*M1_logL)-((1/n)*sum(M1_logL))*((1/n)*sum(M1_logL))

  M2_logL=((-1/2)*log(2*pi)-((1/(2*(a2$sigma^2)))*((Y-M2$fitted.values)^2))-log(sqrt(a2$sigma^2)))
  M2_variance=(1/n)*sum(M2_logL*M2_logL)-((1/n)*sum(M2_logL))*((1/n)*sum(M2_logL))

  M_12= (1/n)*sum(M1_logL*M2_logL)-((1/n)*sum(M1_logL))*((1/n)*sum(M2_logL))

  sd=sqrt(M1_variance+M2_variance-2*M_12)/sqrt(n)


  val=(GIC1-GIC2)/sd;
  p_value=2* pnorm(abs(val),lower.tail = FALSE)

  Zval=qnorm(1-alpha/2)

  if (p_value<alpha){Sig=1} else {Sig=0}

  if (Sig==1){final_pvalue=round(as.numeric(p_value),6); sig_value="significant"}
  if (Sig==0){final_pvalue=round(as.numeric(p_value),6); sig_value=" not significant"}

  message("p-value is ",p_value,  ", and test is " ,sig_value, " at level ", alpha)



}
