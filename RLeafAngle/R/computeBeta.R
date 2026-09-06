#' Compute the parameter of Beta function given leaf angle distribution mearuements.
#' @description Compute the parameter of Beta function given leaf angle distribution mearuements.
#' @param LeafAngles The measurements of leaf angle distribution.
#' @return The two parameters of Beta function given leaf angle distribution mearuements.
#' @author Wei-Min Wang (wmwang AT gmail.com)
#' @references  Wang, W. M., Li, Z. L., & Su, H. B. (2007). 
#' Comparison of leaf angle distribution functions: effects on extinction coefficient and 
#' fraction of sunlit foliage. Agricultural and Forest Meteorology, 143(1), 106-122.
#' @examples
#' data(Pisek)
#' computeBeta(Pisek[[2]])
#' @export
#' 

computeBeta<-function(LeafAngles)
{
  pi180 <- pi/180.0
  MeanAngle <- mean(LeafAngles)*pi180
  VarAngle <- var(LeafAngles)*pi180
  alpha <- ((1 - MeanAngle) / VarAngle - 1 / MeanAngle) * MeanAngle ^ 2
  beta <- alpha * (1 / MeanAngle - 1)
  c(alpha,beta)
}