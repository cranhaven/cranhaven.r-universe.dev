\name{proc.cdf.2d}
\alias{proc.cdf.2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Compute a 2D Binned Kernel Density Estimate from LC/MS data in CDF format.
	}
\description{
 This function provided a method to compute the density estimate of a LC/MS data matrix based on each point's density. It will return a set of peak's centre information including the point's coordinate in each coordinate axis and all the distances between the peak point and grid boundaries.
 }
\usage{
proc.cdf.2d(filename, mz.cut = 5e-4, rt.cut = 50, mz.search.range = 2e-3, 
rt.search.range = 200, mz.search.step = 5e-4, rt.search.step = 50, 
intensity.limit.quantile = 0.1, bPlot = FALSE, transform.mz=FALSE, transform.mz.const=0.1) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{The cdf file name. If the file is not in the working directory, the path needs to be given.}
  \item{mz.cut}{
 The divided gird width in m/z when calculate the density of each point.  }
  \item{rt.cut}{
 The divided gird width in RT when calculate the density of each point. }
 \item{mz.search.range}{maximum peak width in m/z}
 \item{rt.search.range}{(maximum peak width in RT}
 \item{mz.search.step}{maximum search step in m/z}
 \item{rt.search.step}{maximum search step in RT}
 \item{intensity.limit.quantile}{intensity threshold}
 \item{bPlot}{Whether to plot}
 \item{transform.mz}{Whether to apply a nonlinear transformation to m/z values before alignment.}
 \item{transform.mz.const}{A constant used in the m/z transformation function}
}
\value{
\item{finalMatrix}{
	A matrix contains the information of peaks.
	Each row contains one peak's information and each colunm represent one aspect of the peak's information.
	Column 1 's value represent the X Position of each peak's centre.
	Column 2 's value represent the Y Position of each peak's centre.
	Column 3 's value represent the distance between centre of the peak and the top boudary of divided grid.
	Column 4 's value represent the distance between centre of the peak and the bottom boudary of divided grid.
	Column 5 's vlaue represent the peak's value .
	Column 6 's value represent the distance between centre of the peak and the left boudary of divided grid.
	Column 7 's value represent the distance between centre of the peak and the right boudary of divided grid.
}
}
\examples{
library(msdata)
filepath <- system.file("microtofq", package = "msdata")
file <- list.files(filepath, pattern="MM14.mzML",
                     full.names=TRUE, recursive = TRUE)
peakInfo <- proc.cdf.2d(file)
}
\keyword{distribution }% use one of  RShowDoc("KEYWORDS")
\keyword{density}% __ONLY ONE__ keyword per line

