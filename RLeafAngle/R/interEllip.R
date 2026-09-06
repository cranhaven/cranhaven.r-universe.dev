#' Compute the fraction leaf area given leaf angle intervals (in degree) and the parameters of ellipsoidal function.
#' @description Compute the fraction leaf area given leaf angle intervals (in degree) and the parameters of ellipsoidal function.
#' @param AngleLower The lower limit of leaf angle intervals in degree.
#' @param AngleUpper The upper limit of leaf angle intervals in degree.
#' @param lambda The parameter of ellipsoidal function.
#' @return Compute the fraction leaf area of ellipsoidal function.
#' @author Wei-Min Wang (wmwang AT gmail.com)
#' @references  Wang, W. M., Li, Z. L., & Su, H. B. (2007). 
#' Comparison of leaf angle distribution functions: effects on extinction coefficient and 
#' fraction of sunlit foliage. Agricultural and Forest Meteorology, 143(1), 106-122.
#' @examples
#' interEllip(40,50,1.1)
#' @export
#' 

interEllip<-function(AngleLower, AngleUpper, lambda)
{
  ellipLAD <- function(x,lambdaE) 
  {
    if((lambdaE-1)<1*10**(-5))
    {
      delta<-2
    }
    else
    {
      if(lambdaE>1)
      {
        emiga<-(1-lambdaE**(-2))**(0.5)
        delta<-lambdaE+(log((1+emiga)/(1-emiga))/(2*emiga*lambdaE))
      }
      else
      {
        emiga<-(1-lambdaE**(2))**(0.5)
        delta<-lambdaE+asin(emiga)/emiga
      }
    }
    up<-2*(lambdaE**3.0)*sin(x)
    down<-delta*(cos(x)**2+lambdaE**2*sin(x)**2)**2
    up/down
  }
  pi180<-pi/180.0
  fraction<-integrate(ellipLAD, lower=AngleLower*pi180, upper=AngleUpper*pi180, lambdaE=lambda)
  fraction[[1]]
}
