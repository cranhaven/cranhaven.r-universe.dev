#' Compute the fraction leaf area given leaf angle intervals (in degree) and the parameters of Verhoef's leaf angle distribution function.
#' @description Compute the fraction leaf area given leaf angle intervals (in degree) and the parameters of Verhoef's leaf angle distribution function.
#' @param AngleLower The lower limit of leaf angle intervals in degree.
#' @param AngleUpper The upper limit of leaf angle intervals in degree.
#' @param ap One of the parameters of Verhoef's leaf angle distribution function. 
#' @param bp One of the parameters of Verhoef's leaf angle distribution function.
#' @return Compute the fraction leaf area of Verhoef's leaf angle distribution function.
#' @author Wei-Min Wang (wmwang AT gmail.com)
#' @references  Wang, W. M., Li, Z. L., & Su, H. B. (2007). 
#' Comparison of leaf angle distribution functions: effects on extinction coefficient and 
#' fraction of sunlit foliage. Agricultural and Forest Meteorology, 143(1), 106-122.
#' @examples
#' intervalTrig(40,50, -0.325, -0.173)
#' @export
#' 

intervalTrig<-function(AngleLower, AngleUpper, ap, bp)
{
  #### return the distribution for given leaf inclination intervals.
  trigit<-function(theta, a, b)
  {
    ### to compute the cumulative leaf inclination distribution F
    x <- 2 * theta
    deltaX <- 1.0
    t <- 1e-6
    y <- 0.0
    while(abs(deltaX) >= t)
    {
      y <- a * sin(x) + 0.5 * b * sin(2*x)
      deltaX <- 0.5 * ( y - x + 2 * theta)
      x <- x + deltaX
    }
    
    (2/pi)*(y+theta)		
  } 
  pi180 <- pi/180.0
  fraction<-trigit(AngleUpper*pi180, ap, bp)-trigit(AngleLower*pi180, ap, bp)
  fraction
}