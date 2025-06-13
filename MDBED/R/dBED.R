#' @title Joint density function of the bivariate exponential distribution (BED) based on the Moran-Downton model
#'
#' @description Given the values of the parameters, this function provides the joint density value of the BED for a positive pair or pairs
#' (x,y). The required inputs are the correlation coefficient, the scale parameters of the marginal distributions,
#' and the pair/s (x,y).
#'
#' @usage dBED(rho,Betax,Betay,x,y)
#'
#' @param rho Correlation coefficient between the marginal distributions of x and y.
#' @param Betax Scale parameter of the marginal distribution of x.
#' @param Betay Scale parameter of the marginal distribution of y.
#' @param x A value or set of values (vector) of the marginal distribution of x. It must be the same size of y.
#' @param y A value or set of values (vector) of the marginal distribution of y. It must be the same size of x.
#'
#' @importFrom Rdpack reprompt
#'
#' @details The values of the joint density function are computed based on Eq.18 described in \insertCite{Nagao1971;textual}{MDBED}.
#'
#' @return The value of the joint PDF of the pair/s (x,y).
#'
#' @note The equation of the PDF is based on the Bessel function. Therefore,  for very extreme values this function may reaches infinity. It might
#' generate NA values.
#'
#' @author Luis F. Duque <lfduquey@@gmail.com> <l.f.duque-yaguache2@@newcastle.ac.uk>
#'
#' @references \insertAllCited{}
#'
#' @examples dBED(rho=0.85,Betax=1,Betay=1,x=0.6,y=0.8)
#'
#' @export
dBED<-function(rho,Betax,Betay,x,y){

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# 1. Checking of vector sizes

  if (length(x)-length(y)!=0){stop("Different vector's sizes")}

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# 2. Theoretical Joint PDF
# Eq. 18 Nagao and Kandoya (1971)

  PDF<-function(rho1,x1,y1,Betax1,Betay1){

    A<- 1/((1-rho1)*Betax1*Betay1)
    B<-exp(-((x1/Betax1)+(y1/Betay1))/(1-rho1))
    C<-besselI(2*sqrt(rho1*(x1/Betax1)*(y1/Betay1))/(1-rho1),0)
    d1<-A*B*C
    return(d1)
  }


#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# 3. Apply the PDF to the pair of values

  d<-length(x)

  for (i in 1:length(x)){
    d[i]<-PDF(rho,x[i],y[i],Betax,Betay)
  }

  return(d)

}
