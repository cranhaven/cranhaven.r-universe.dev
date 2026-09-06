#' Retrieve the density of leaf area given leaf angle measurements.
#' @description Retrieve the density of leaf area given leaf angle measurements.
#' @param LeafAngles The measurements of leaf angle distribution.
#' @return Leaf area density given leaf angle intervals (0,10,20,30,40,50,60,70,80,90).
#' @author Wei-Min Wang (wmwang AT gmail.com)
#' @references  Wang, W. M., Li, Z. L., & Su, H. B. (2007). 
#' Comparison of leaf angle distribution functions: effects on extinction coefficient and 
#' fraction of sunlit foliage. Agricultural and Forest Meteorology, 143(1), 106-122.
#' @examples
#' data(Falster) 
#' LADdensity(Falster[[2]])
#' @export



LADdensity<-function(LeafAngles)
{
  hh <- hist(LeafAngles, plot=FALSE, breaks=c(0,10,20,30,40,50,60,70,80,90))
  return (hh[[3]]*10.0)
}




