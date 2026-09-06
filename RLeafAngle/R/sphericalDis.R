#' Compute the spherical distribution of leaf zenith angle.
#' @description Compute the spherical distribution of leaf zenith angle.
#' @return The spherical distribution of leaf zenith angle.
#' @author Wei-Min Wang (wmwang AT gmail.com)
#' @references  Wang, W. M., Li, Z. L., & Su, H. B. (2007). 
#' Comparison of leaf angle distribution functions: effects on extinction coefficient and 
#' fraction of sunlit foliage. Agricultural and Forest Meteorology, 143(1), 106-122.
#' @examples
#' sDis<-sphericalDis()
#' plot(c(4.5, 13.5, 22.5, 31.5, 40.5, 49.5, 58.5, 67.5, 76.5, 85.5), sDis, 
#' xlab=expression(Leaf~zenith~angle~~(""^"o")), ylab="Leaf area freqency")
#' @export


sphericalDis<-function()
{
  angz<-c(0, 9.0, 18.0, 27.0, 36.0, 45.0, 54.0, 63.0, 72.0, 81.0, 90.0) 
  sDis<-0
  sphericalLAD<-function(LeafAngle)
  {
    #spherical- leaf angle frequency same as as for surface elements 
    #of a sphere.
    sin(LeafAngle)
  }
  pi180<-pi/180.0
  for(i in 1:(length(angz)-1))
  {
    fraction<-integrate(sphericalLAD, lower=angz[i]*pi180, upper=angz[i+1]*pi180)
    sDis[i]<-fraction[[1]]
  }
  sDis
}

