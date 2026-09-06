#' Compute the Beta distribution of leaf zenith angle.
#' @description Compute the Beta distribution of leaf zenith angle.
#' @param meu One of the parameters for Beta function. 
#' @param neu One of the parameters for Beta function.
#' @return The Beta distribution of leaf zenith angle.
#' @author Wei-Min Wang (wmwang AT gmail.com)
#' @references  Wang, W. M., Li, Z. L., & Su, H. B. (2007). 
#' Comparison of leaf angle distribution functions: effects on extinction coefficient and 
#' fraction of sunlit foliage. Agricultural and Forest Meteorology, 143(1), 106-122.
#' @examples
#' sDis<-BetaDis(1.616,2.188)
#' plot(c(4.5, 13.5, 22.5, 31.5, 40.5, 49.5, 58.5, 67.5, 76.5, 85.5), sDis, 
#' xlab=expression(Leaf~zenith~angle~~(""^"o")), ylab="Leaf area freqency")
#' @rawNamespace importFrom("graphics", "hist", "plot")
#' importFrom("stats", "integrate", "var")
#' @export
#' 


BetaDis<-function(meu,neu)
{
  angz<-c(0, 9.0, 18.0, 27.0, 36.0, 45.0, 54.0, 63.0, 72.0, 81.0, 90.0) 
  sDis<-0
  pi180<-pi/180.0
  
  betaLAD<-function(LeafAngle,xmeu,xneu) 
  {
    degree<-90.0
    front<-(1/360.0)*(1/90.0)*gamma(xmeu+xneu)/(gamma(xmeu)*gamma(xneu))
    back<-(1-LeafAngle/90.0)**(xmeu-1)*(LeafAngle/90.0)**(xneu-1)
    front*back
  }

  for(i in 1:(length(angz)-1))
  {
    fraction<-integrate(betaLAD, lower=angz[i], upper=angz[i+1], xmeu=meu, xneu=neu)
    sDis[i]<-fraction[[1]]*360.0
  }
  sDis
}