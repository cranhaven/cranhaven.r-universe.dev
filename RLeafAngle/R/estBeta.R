#' Estimate the parameters of Beta distribution given leaf angle measurements.
#' @description Estimate the parameters of Beta distribution given leaf angle measurements.
#' @param LeafAngles The leaf angle measurements. 
#' @return the parameters of Beta distribution.
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
#'

estBeta<-function(LeafAngles)
{
  pi180 <- pi/180.0
  x <- LeafAngles*pi180
  xbar <- mean(x)
  xvar <- var(x)
  a <- (xbar*(1-xbar)/xvar - 1)*xbar
  b <- (1-xbar)*a/xbar
  
  #c(a,b)
  alpha <- ((1 - xbar) / xvar - 1 / xbar) * xbar ^ 2
  beta <- alpha * (1 / xbar - 1)
  #return(params = list(alpha = alpha, beta = beta))  
  c(alpha, beta)
}