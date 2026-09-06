#' Compute the G value given lamda (the parameter of ellipsoidal function) and beam direction.
#' @description Compute the G value given lamda (the parameter of ellipsoidal function) and beam direction.
#' @param theta The zenith angle of beam direction.
#' @param lambda The parameter of ellipsoidal function given leaf angle distribution mearuements.
#' @return The G value.
#' @author Wei-Min Wang (wmwang AT gmail.com)
#' @references  Wang, W. M., Li, Z. L., & Su, H. B. (2007). 
#' Comparison of leaf angle distribution functions: effects on extinction coefficient and 
#' fraction of sunlit foliage. Agricultural and Forest Meteorology, 143(1), 106-122.
#' @examples
#' computeGEllip(1.0, 30)
#' 
#' @export
#' 

computeGEllip<-function(lambda,theta) 
{
  pi180<-pi/180.0
  sqrt(lambda**2+tan(theta*pi180)**2)/(lambda+1.774*(lambda+1.182)**(-0.733))
}
