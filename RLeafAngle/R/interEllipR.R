#' Compute the fraction leaf area for rotated-ellipsoidal function given leaf angle intervals (in degree) and the parameters.
#' @description Compute the fraction leaf area given leaf angle intervals (in degree) and the parameters of rotated-ellipsoidal function.
#' @param AngleLower The lower limit of leaf angle intervals in degree.
#' @param AngleUpper The upper limit of leaf angle intervals in degree.
#' @param lambdaR The parameter of rotated-ellipsoidal function.
#' @return Compute the fraction leaf area of rotated-ellipsoidal function.
#' @author Wei-Min Wang (wmwang AT gmail.com)
#' @references  Wang, W. M., Li, Z. L., & Su, H. B. (2007). 
#' Comparison of leaf angle distribution functions: effects on extinction coefficient and 
#' fraction of sunlit foliage. Agricultural and Forest Meteorology, 143(1), 106-122.
#' @examples
#' interEllipR(40,50,1.1)
#' @export
#' 

interEllipR<-function(AngleLower, AngleUpper, lambdaR)
{
  ellipLADR<-function(x, lambdaER)
  {
    if((lambdaER-1)<1*10**(-5))
    {
      delta<-2
    }
    else
    {
      if(lambdaER>1)
      {
        emiga<-(1-lambdaER**(-2))**(0.5)
        delta<-lambdaER+(log((1+emiga)/(1-emiga))/(2*emiga*lambdaER))
      }
      else
      {
        emiga<-(1-lambdaER**(2))**(0.5)
        delta<-lambdaER+asin(emiga)/emiga
      }
    }
    up<-2*(lambdaER**3.0)*cos(x)
    #delta<-lamdaE+1.774*(lamdaE+1.182)**(-0.733)
    down<-delta*(sin(x)**2+lambdaER**2*cos(x)**2)**2
    up/down
  }
  pi180<-pi/180.0
  fraction<-integrate(ellipLADR, lower=AngleLower*pi180, upper=AngleUpper*pi180, lambdaER=lambdaR)
  fraction[[1]]
}