#' Plot the density of leaf area given leaf angle measurements.
#' @description Plot the density of leaf area given leaf angle measurements.
#' @param LeafAngles The measurements of leaf angle distribution.
#' @param main An overall title for the plot.
#' @param type The type of plot.
#' @param pch The symbol of plot.
#' @author Wei-Min Wang (wmwang AT gmail.com)
#' @references  Wang, W. M., Li, Z. L., & Su, H. B. (2007). 
#' Comparison of leaf angle distribution functions: effects on extinction coefficient and 
#' fraction of sunlit foliage. Agricultural and Forest Meteorology, 143(1), 106-122.
#' @examples
#' data(Pisek)
#' plotdensity(Pisek[[2]])
#' @export
#' 


plotdensity<-function(LeafAngles, main="Leaf angle distribution", type="l", pch=20)
{
  hh <- hist(LeafAngles, plot=FALSE, breaks=c(0,9,18,27,36,45,54,63,72,81,90))
  plot(c(4.5, 13.5, 22.5, 31.5, 40.5, 49.5, 58.5, 67.5, 76.5, 85.5), hh[[3]]*10.0, 
       xlab=expression(Leaf~inclination~angle~~(""^"o")), ylab="Leaf area freqency", 
       main=main, type=type, pch=pch)
}
