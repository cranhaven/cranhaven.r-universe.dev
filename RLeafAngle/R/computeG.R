#' Compute the mean projection of unit leaf area on the plane perpendicular to beam direction, namely, G parameter.
#' @description Compute the mean projection of unit leaf area on the plane perpendicular to beam direction, namely, G parameter.
#' @param LeafAngleZ The center angles list of leaf zenith angle intervals.
#' @param FractionZ The leaf area fraction list given leaf zenith angle intervals list. 
#' @param LeafAngleA The center angles list of leaf azimuth angle intervals.
#' @param FractionA The leaf area fraction list given leaf azimuth angle intervals list. 
#' @param theta The zenith angle of beam direction.
#' @param alpha The azimuth angle of beam direction.
#' @return The mean projection of unit leaf area on the plane perpendicular to beam direction, namely, G value.
#' @author Wei-Min Wang (wmwang AT gmail.com)
#' @references  Wang, W. M., Li, Z. L., & Su, H. B. (2007). 
#' Comparison of leaf angle distribution functions: effects on extinction coefficient and 
#' fraction of sunlit foliage. Agricultural and Forest Meteorology, 143(1), 106-122.
#' @examples
#' angleZ<-c(4.5, 13.5, 22.5, 31.5, 40.5, 49.5, 58.5, 67.5, 76.5, 85.5)
#' angleA<-c(10,30,50,70,90,110,130,150,170,190,210,230,250,270,290,310,330,350)
#' sADis<-sysAziDis()
#' sZDis<-sphericalDis()
#' for(solarZenith in 10:80)
#' {
#'   print(computeG(angleZ,sZDis,angleA,sADis,solarZenith,40))
#' }
#' 
#' @export
#' 

computeG<-function(LeafAngleZ, FractionZ, LeafAngleA, FractionA, theta, alpha)
{
  pi180 <- pi/180
  sumGA <- 0
  sumGZ <- 0
  for(i in 1:length(LeafAngleZ))
  {
    sumGA <- 0
    for(j in 1:length(LeafAngleA))
    {
      cosr <- cos(theta*pi180)*cos(LeafAngleZ[i]*pi180)+sin(theta*pi180)*
        sin(LeafAngleZ[i]*pi180)*cos((alpha-LeafAngleA[j])*pi180)
      sumGA <- sumGA + abs(cosr) * FractionA[j]
    }
    sumGZ <- sumGZ + sumGA * FractionZ[i]
  }
  sumGZ
}