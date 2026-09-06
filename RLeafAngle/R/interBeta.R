#' Compute the fraction leaf area given leaf angle intervals (in degree) and the parameters of Beta function.
#' @description Compute the fraction leaf area given leaf angle intervals (in degree) and the parameters of Beta function.
#' @param AngleLower The lower limit of leaf angle intervals in degree.
#' @param AngleUpper The upper limit of leaf angle intervals in degree.
#' @param meu One of the parameters for Beta function. 
#' @param neu One of the parameters for Beta function. 
#' @return Compute the fraction leaf area of Beta function.
#' @author Wei-Min Wang (wmwang AT gmail.com)
#' @references  Wang, W. M., Li, Z. L., & Su, H. B. (2007). 
#' Comparison of leaf angle distribution functions: effects on extinction coefficient and 
#' fraction of sunlit foliage. Agricultural and Forest Meteorology, 143(1), 106-122.
#' @examples
#' interBeta(40,50,1.616,2.188)
#' @export
#' 


interBeta<-function(AngleLower, AngleUpper, meu, neu)
{
  
  betaLAD<-function(LeafAngle,xmeu,xneu) 
  {
    degree<-90.0
    front<-(1/360.0)*(1/90.0)*gamma(xmeu+xneu)/(gamma(xmeu)*gamma(xneu))
    back<-(1-LeafAngle/90.0)**(xmeu-1)*(LeafAngle/90.0)**(xneu-1)
    front*back
  }
  fraction<-integrate(betaLAD, lower=AngleLower, upper=AngleUpper, xmeu=meu, xneu=neu)
  fraction[[1]]
}
