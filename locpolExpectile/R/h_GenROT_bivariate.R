#' @title Rule-of-Thumb bandwidth selectors for bivariate covariate setting
#'
#' @description General Rule-of-Thumb bandwidth selector for the expectile regression
#' proposed by Adam and Gijbels (2021b) see Formula (26) for a bivariate covariate setting.
#'  The weight functions are chosen to be equal to the indicator
#'  functions on \eqn{[min(Z_{ki})+0.1,max(Z_{ki})-0.1]} for \code{k=1,2}
#'  (i.e. for the two covariates) and \code{j=0} and \code{p=1}.
#'
#' @param Z1 The first covariate data values.
#' @param Z2 The second covariate data values.
#' @param Y The response data values.
#' @param kernel The kernel used to perform the estimation. In default setting,
#' \code{kernel=gaussK}. See details in \code{\link[locpol]{Kernels}}.
#' @param omega Numeric vector of level between 0 and 1 where 0.5 corresponds
#' to the mean.

#' @return \code{\link{h_GenROT_bivariate}} provides the general Rule-of-Thumb bandwidth selector
#' for the expectile regression, in the bivariate covariate setting,
#' proposed by Adam and Gijbels (2021b).
#'
#' @import expectreg
#' @import locpol
#' @rdname h_GenROT_bivariate
#'
#'
#'
#' @references{
#'
#' Adam, C. and Gijbels, I. (2021b). Partially linear expectile regression using
#' local polynomial fitting. In Advances in Contemporary Statistics and Econometrics:
#' Festschrift in Honor of Christine Thomas-Agnan, Chapter 8, pages 139–160. Springer, New York.
#'
#' }
#'

#'
#' @import expectreg
#' @import locpol
#' @examples
#' library(locpol)
#' library(lestat)
#' set.seed(6)
#' dist <- muniformdistribution(rep(0, 2), rep(1, 2))
#' values<-simulate(dist,200)
#' Z_1<-values[,1]
#' Z_2<-values[,2]
#' Z<-rbind(Z_1,Z_2)
#' gamma=cbind(3,-0.4)
#' set.seed(7)
#' eta_1<-rnorm(100,0,1)
#' X1=(gamma%*%Z)+(1.5*eta_1)
#' set.seed(8)
#' eta_2<-rnorm(100,0,2)
#' X2=(gamma%*%Z)+(1.5*eta_2)
#' X<-rbind(X1,X2)
#' set.seed(9)
#' epsilon<-rt(100,3)
#' delta_true<-rbind(0,-0.8)
#' Y=as.numeric((t(delta_true)%*%X)+(0.2*exp(1.5*(gamma%*%Z)))+epsilon)
#'
#' h=h_GenROT_bivariate(Z1=Z_1,Z2=Z_2,Y=Y,kernel=gaussK,omega=0.1)
#' #h=0.1241427
#'
#' @name h_GenROT_bivariate
#' @export
#'
#'
#'
h_GenROT_bivariate<-function(Z1,Z2,Y,omega,kernel=gaussK)
{
  k_2_1=max(Z1)-0.1
  k_1_1=min(Z1)+0.1
  k_2_2=max(Z2)-0.1
  k_1_2=min(Z2)+0.1
  C_p_nu=((locpol::computeRK(kernel)^2)*2)^(1/6)
  #function in locpol
  #k_O will be the indicator function
  exp_tilt=expectreg::expectreg.ls(Y~rb(Z1,type="parametric")+rb(Z2,type="parametric")
                        +rb(Z1^2,type="parametric")+rb(Z2^2,type="parametric")
                        +rb(Z1^3,type="parametric")+rb(Z2^3,type="parametric")
                        +rb(Z1^4,type="parametric")+rb(Z2^4,type="parametric")
                        +rb(Z1^5,type="parametric")+rb(Z2^5,type="parametric")
                        ,estimate="laws",smooth ="schall",expectiles=omega)
  exp_tilt_estimate=as.numeric(exp_tilt$intercepts)+
    (as.numeric(exp_tilt$coefficients[1]) * Z1)+(as.numeric(exp_tilt$coefficients[2]) * Z2)+
    (as.numeric(exp_tilt$coefficients[3])*Z1^2)+(as.numeric(exp_tilt$coefficients[4])*Z2^2)+
    (as.numeric(exp_tilt$coefficients[5]) * Z1^3)+(as.numeric(exp_tilt$coefficients[6]) * Z2^3)+
    (as.numeric(exp_tilt$coefficients[7])*Z1^4)+(as.numeric(exp_tilt$coefficients[8])*Z2^4)+
    (as.numeric(exp_tilt$coefficients[9]) * Z1^5)+(as.numeric(exp_tilt$coefficients[10]) * Z2^5)
  sum=0
  s_1=0
  s_2=0
  for(i in 1:length(Y))
  {
    s=0
    if(Y[i]<=exp_tilt_estimate[i])
    {
      s=((1-omega)*(Y[i]-exp_tilt_estimate[i]))^2
      s_1=s_1+1
    }else
    {
      s=((omega)*(Y[i]-exp_tilt_estimate[i]))^2
      s_2=s_2+1
    }
    sum=sum+s
  }

  denominateur=((omega*s_2*(1/length(Z1)))+((1-omega)*s_1*(1/length(Z1))))^2
  numerateur=(((k_2_1-k_1_1)*(k_2_2-k_1_2))*sum*(1/length(Z1)))/(denominateur)


  sum_2=0
  for(i in 1:length(Z1))
  {
    s_2=0
    if(k_1_1<= Z1[i] && Z1[i]<=k_2_1 && k_1_2<= Z2[i] && Z2[i]<=k_2_2)
    {
      s_2=(((2*as.numeric(exp_tilt$coefficients[3]))+(6*as.numeric(exp_tilt$coefficients[5])*Z1[i])
            +(12*as.numeric(exp_tilt$coefficients[7])*((Z1[i])^2))+(20*as.numeric(exp_tilt$coefficients[9])*((Z1[i])^3)))
           +(2*as.numeric(exp_tilt$coefficients[4]))+(6*as.numeric(exp_tilt$coefficients[6])*Z2[i])
           +(12*as.numeric(exp_tilt$coefficients[8])*((Z2[i])^2))+(20*as.numeric(exp_tilt$coefficients[10])*((Z2[i])^3)))^2
    }else
    {
      s_2=0
    }
    sum_2=sum_2+s_2
  }

  Denominateur=sum_2*(1/length(Z1))
  h_optimal_expectile=C_p_nu*((numerateur/Denominateur)^(1/6))*(length(Z1)^(-1/6))
  return(h_optimal_expectile)
}



