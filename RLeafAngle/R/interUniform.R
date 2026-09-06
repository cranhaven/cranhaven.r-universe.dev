#' Compute the fraction leaf area of Uniform distribution given leaf angle intervals (in degree).
#' @description Compute the fraction leaf area of Uniform distribution given leaf angle intervals (in degree).
#' @param AngleLower The lower limit of leaf angle intervals in degree.
#' @param AngleUpper The upper limit of leaf angle intervals in degree.
#' @return Compute the fraction leaf area of Uniform function.
#' @author Wei-Min Wang (wmwang AT gmail.com)
#' @references  Wang, W. M., Li, Z. L., & Su, H. B. (2007). 
#' Comparison of leaf angle distribution functions: effects on extinction coefficient and 
#' fraction of sunlit foliage. Agricultural and Forest Meteorology, 143(1), 106-122.
#' @examples
#' interUniform(40,50)
#' @export
#' 


interUniform<-function(AngleLower, AngleUpper)
{
  uniformLAD<-function(LeafAngle)
  {
    #uniform- same proportion of leaves at any angle.
    (2/pi)*rep(1, length(LeafAngle)) 
  }   
  pi180<-pi/180.0
  fraction<-integrate(uniformLAD, lower=AngleLower*pi180, upper=AngleUpper*pi180)
  fraction[[1]]
}