#' Compute the fraction of sunlit folige given solar position, LAI and G value with the assumption of the symmetric distribution of leaf azimuth angle.
#' @description Compute the fraction of sunlit folige given solar position, LAI and G value with the assumption of the symmetric distribution of leaf azimuth angle.
#' @param Theta Solar zenith angle.
#' @param G The G value, namely the projection of foliage area. 
#' @param LAI Leaf area index.
#' @return The fraction of sunlit foliage.
#' @author Wei-Min Wang (wmwang AT gmail.com)
#' @references  Wang, W. M., Li, Z. L., & Su, H. B. (2007). 
#' Comparison of leaf angle distribution functions: effects on extinction coefficient and 
#' fraction of sunlit foliage. Agricultural and Forest Meteorology, 143(1), 106-122.
#' @examples
#' computeSunlit(30, 0.5, 2.0)
#' 
#' @export
#' 
#' 


computeSunlit<-function(Theta, G, LAI) 
{
  pi180 <- pi/180
  kb<-G/cos(Theta*pi180)
  sun_LAI<-(1-exp(-kb*LAI))/kb
  fsun<-sun_LAI/LAI
  fsun
}