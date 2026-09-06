#' Compute the symmetric distribution of leaf azimuth angle.
#' @description Compute the symmetric distribution of leaf azimuth angle.
#' @return The symmetric distribution of leaf azimuth angle.
#' @author Wei-Min Wang (wmwang AT gmail.com)
#' @references  Wang, W. M., Li, Z. L., & Su, H. B. (2007). 
#' Comparison of leaf angle distribution functions: effects on extinction coefficient and 
#' fraction of sunlit foliage. Agricultural and Forest Meteorology, 143(1), 106-122.
#' @examples
#' aDis<-sysAziDis()
#' plot(c(10,30,50,70,90,110,130,150,170,190,210,230,250,270,290,310,330,350), aDis, 
#' xlab=expression(Leaf~azimuth~angle~~(""^"o")), ylab="Leaf area freqency")
#' @export

sysAziDis<-function()
{
  LeafAngleA <- c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200, 220, 240, 260, 280,
            300, 320, 340, 360) 
  ladA<-0
  symA<-function(x)
  {
    #symmetric azimuth distribution.  
    #rewrite this code to predict Leaf Area percent
    #for given leaf azimuthal angle.    
    (1/360.0)*rep(1, length(x)) 
  } 
  
  for(i in 1:(length(LeafAngleA)-1))
  {
    ladA[i] <- integrate(symA, stop.on.error = TRUE, lower = LeafAngleA[i], upper = LeafAngleA[i+1])[1][[1]]
    #angac[i] <- (anga[i]+anga[i+1])/2
  }
  ladA
}



