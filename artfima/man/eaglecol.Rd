\name{eaglecol}
\alias{eaglecol}
\docType{data}
\title{
Tree-ring indicies for Douglas Fir, Colorado, 1107-1964.
}

\description{
Tree-ring indicies for Douglas Fir, Colorado, 1107-1964.
There are 858 consecutive values.
When the environment is suboptimal, tree ring growth is limited by the climate,
usually either ambient temperature or precipitation.
For this tree-ring time series, the tree is located on a mountain
and the limiting growth factor is temperature.
}

\usage{data("eaglecol")}

\format{
  The format is:
 Time-Series [1:858] from 1107 to 1964: 78 62 26 100 121 97 102 85 214 245 ...
}

\source{
Laboratory of Tree-ring Research (LTRR), The University of Arizona
\url{http://ltrr.arizona.edu/}
}

\references{
Fritts, H.C. et al. (1971) 
Multivariate techniques for specifying tree-growth and climatic relationships 
and for reconstructing anomalies in Paleoclimate. 
Journal of Applied Meteorology, 10, pp.845-864.

Hipel, K.W. and McLeod, A.I. (1994). 
Time Series Modelling of Water Resources and Environmental Systems.
Elsevier.
\url{http://www.stats.uwo.ca/faculty/aim/1994Book/default.htm}

McLeod, A.I. & Hipel, K.W. (1978), Preservation of the rescaled adjusted range, 
Water Resources Research 14, 491-516.
}

\examples{
data(eaglecol)
plot(eaglecol)
\dontrun{#confidence ellipse
library("ellipse") #needs this package!
ansTFD <- artfima(eaglecol)
v <- ansTFD$varbeta
bHat <- c(ansTFD$dHat, ansTFD$lambdaHat)
xy <- ellipse(v, centre=bHat, level=0.9)
plot(xy, type="l", lwd=2, xlab=expression(delta), ylab=expression(lambda))
points(matrix(bHat,ncol=2), pch=16, cex=3, col="blue")
#setwd("D:/DropBox/R/2016/artfima/Explore_ts_data/eaglecol")
#postscript(file="eaglecolCI.eps")
#plot(xy, type="l", lwd=2, xlab=expression(delta), ylab=expression(lambda))
#points(matrix(bHat,ncol=2), pch=16, cex=3, col="blue")
#graphics.off()
}
\dontrun{#forecast comparison

}

}
\keyword{datasets}



