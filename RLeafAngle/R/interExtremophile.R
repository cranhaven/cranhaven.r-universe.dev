#' Compute the fraction leaf area of Extremophile distribution given leaf angle intervals (in degree).
#' @description Compute the fraction leaf area of Extremophile distribution given leaf angle intervals (in degree).
#' @param AngleLower The lower limit of leaf angle intervals in degree.
#' @param AngleUpper The upper limit of leaf angle intervals in degree.
#' @return Compute the fraction leaf area of Extremophile function.
#' @author Wei-Min Wang (wmwang AT gmail.com)
#' @references  Wang, W. M., Li, Z. L., & Su, H. B. (2007). 
#' Comparison of leaf angle distribution functions: effects on extinction coefficient and 
#' fraction of sunlit foliage. Agricultural and Forest Meteorology, 143(1), 106-122.
#' @examples
#' interExtremophile(40,50)
#' @export
#' 


interExtremophile<-function(AngleLower, AngleUpper)
{
  extremophileLAD<-function(LeafAngle)
  {
    #extremophile- oblique leaves least frequent. 
    2*(1+cos(4*LeafAngle))/pi
  }       
  pi180<-pi/180.0
  fraction<-integrate(extremophileLAD, lower=AngleLower*pi180, upper=AngleUpper*pi180)
  fraction[[1]]
}