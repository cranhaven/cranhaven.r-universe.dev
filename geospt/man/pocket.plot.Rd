\name{pocket.plot}
\alias{pocket.plot}

\title{
graphs the probability or standardized variance in the directions north-south or east-west
}
\description{
The pocket-plot (so named because of its use in detecting pockets
of non-stationarity) is a technique necessary to identify a localized
area that is atypical with respect to the stationarity model. It is built
to exploit the spatial nature of the data through the coordinates
of rows and columns (east "X" and north "Y", respectively).
}
\usage{
pocket.plot(data, graph, X, Y, Z, Iden=F, ...)
}

\arguments{
\item{data}{data frame should contain the dependent variable and coordinates X and Y, data must be gridded}
\item{graph}{type of graph associated with the probability or standardized variance plot pocket in the directions north-south or east-west;
Probabilities PocketPlot by rows, ie horizontal "south-north" (PPR), Probabilities PocketPlot by columns, ie vertical "east-west" (PPC), 
PocketPlot of variance by rows, ie horizontal "south-north" (PVR) and PocketPlot of variance by columns, ie vertical "east-west" (PVC)}
\item{X}{defined by the spatial coordinates}
\item{Y}{defined by the spatial coordinates}
\item{Z}{regionalized variable with which you construct the statistics associated with the probability or standardized variance, these are plotted in the so-called pocket plot}
\item{Iden}{ logical. The users can identify the points by themselves, TRUE or FALSE. FALSE by default is used.}
\item{...}{arguments to be passed to ...}
}
\details{For identifying outliers, this function uses a modification of the boxplot.with.outlier.label function, available at 
\url{https://www.r-statistics.com/2011/01/how-to-label-all-the-outliers-in-a-boxplot/}
}
\value{
returns (or plots) the pocket plot
}
\references{
Cressie, N.A.C. 1993. \emph{Statistics for Spatial Data}. Wiley.

Gomez, M., Hazen, K. 1970. \emph{Evaluating sulfur and ash distribution in coal seems by statistical
response surface regression analysis}. U.S. Bureau of Mines Report RI 7377.
}
\examples{
# Core measurements (in \% coal ash) at reoriented locations. 
# Units on the vertical axis are \% coal ash.

# These data was found in mining samples originally reported by 
# Gomez and Hazen (1970), and later used by Cressie (1993). 

# These data are available in the sp and gstat packages

library(gstat)
data(coalash) 
plot(coalash[,1:2], type="n", xlab="x", ylab="y") 
text(coalash$x,coalash$y,coalash$coalash,cex=0.6)

# Pocket plot in the north-south direction. 
# Units on the vertical axis are root (\% coal ash) 

# Plot generated with the function pocket.plot 
# Clearly rows 2, 6, and 8 are atypical 

# This serves as verification that these rows are potentially problematic

# Analysis of local stationarity in probabilities of the coal in south-north direction

pocket.plot(coalash, "PPR", coalash$x, coalash$y, coalash$coalash, FALSE)

# Analysis of local stationarity in variance of the coal in south-north direction 

pocket.plot(coalash, "PVR", coalash$x, coalash$y, coalash$coalash, FALSE) 

# Analysis of local stationarity in probabilities of the coal in east-west direction 

pocket.plot(coalash, "PPC", coalash$x, coalash$y, coalash$coalash, FALSE) 

# Analysis of local stationarity in variance of the coal in east-west direction 

pocket.plot(coalash, "PVC", coalash$x, coalash$y, coalash$coalash, FALSE)
}

\keyword{ spatial }
