
#' Compute the parameter of ellipsoidal function given leaf angle distribution mearuements.
#' @description Compute the parameter of ellipsoidal function given leaf angle distribution mearuements.
#' @param LeafAngles The measurements of leaf angle distribution.
#' @return The parameter of ellipsoidal function given leaf angle distribution mearuements.
#' @author Wei-Min Wang (wmwang AT gmail.com)
#' @references  Wang, W. M., Li, Z. L., & Su, H. B. (2007). 
#' Comparison of leaf angle distribution functions: effects on extinction coefficient and 
#' fraction of sunlit foliage. Agricultural and Forest Meteorology, 143(1), 106-122.
#' @examples
#' data(Pisek)
#' computeLambda(Pisek[[2]])
#' 
#' @export
#' 

computeLambda<-function(LeafAngles)
{
  meanAngle <- mean(LeafAngles)
  pi180 <- pi/180.0
  lambdaT<-(meanAngle*pi180/9.65)**(-0.6061)-3
  lambdaT
}