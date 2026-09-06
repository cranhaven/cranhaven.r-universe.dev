#' Compute the ellipsoidal distribution of leaf zenith angle.
#' @description Compute the ellipsoidal distribution of leaf zenith angle.
#' @param lambda The parameter of ellipsoidal function.
#' @return The ellipsoidal distribution of leaf zenith angle.
#' @author Wei-Min Wang (wmwang AT gmail.com)
#' @references  Wang, W. M., Li, Z. L., & Su, H. B. (2007). 
#' Comparison of leaf angle distribution functions: effects on extinction coefficient and 
#' fraction of sunlit foliage. Agricultural and Forest Meteorology, 143(1), 106-122.
#' @examples
#' sDis<-ellipsoidalDis(1)
#' plot(c(4.5, 13.5, 22.5, 31.5, 40.5, 49.5, 58.5, 67.5, 76.5, 85.5), sDis, 
#' xlab=expression(Leaf~zenith~angle~~(""^"o")), ylab="Leaf area freqency")
#' @export
#' 


ellipsoidalDis<-function(lambda)
{
  angz<-c(0, 9.0, 18.0, 27.0, 36.0, 45.0, 54.0, 63.0, 72.0, 81.0, 90.0) 
  sDis<-0
  pi180<-pi/180.0
  
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
  
  for(i in 1:(length(angz)-1))
  {
    fraction<-integrate(ellipLAD, lower=angz[i]*pi180, upper=angz[i+1]*pi180,lambdaE=lambda)
    sDis[i]<-fraction[[1]]
  }
  sDis
}