
#' Compute the parameter of rotated-ellipsoidal function given leaf angle distribution mearuements.
#' @description Compute the parameter of rotated-ellipsoidal function given leaf angle distribution mearuements.
#' @param LeafAngles The measurements of leaf angle distribution.
#' @return The parameter of ellipsoidal function given leaf angle distribution mearuements.
#' @author Wei-Min Wang (wmwang AT gmail.com)
#' @references  Wang, W. M., Li, Z. L., & Su, H. B. (2007). 
#' Comparison of leaf angle distribution functions: effects on extinction coefficient and 
#' fraction of sunlit foliage. Agricultural and Forest Meteorology, 143(1), 106-122.
#' @examples
#' data(Pisek)
#' computeLambdaR(Pisek[[2]])
#' 
#' @export
#' 

computeLambdaR<-function(LeafAngles)
{
  meanAngle <- mean(LeafAngles)
  meanAngleR <- 90.0 - meanAngle
  pi180 <- pi/180.0
  lambdaTR<-(meanAngleR*pi180/9.65)**(-0.6061)-3
  lambdaTR
}