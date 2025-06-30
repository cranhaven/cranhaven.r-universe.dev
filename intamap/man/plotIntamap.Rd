\name{plotIntamap}
\alias{plotIntamap}
\alias{plot.default}
\alias{plot.copula}
\alias{plot.idw}
\alias{plot.automap}
%\alias{plot.psgp}
\alias{plot.linearVariogram}
\alias{plot.transGaussian}
\alias{plot.yamamoto}

\title{ plot intamap objects}
\description{
Plotting function for \code{intamap}-objects of the type described in 
\code{\link{intamap-package}}
}
\usage{
plotIntamap(object, zcol = "all", sp.layout = NULL, plotMat = c(2,2), ...)
\method{plot}{copula}(x,  ...)
\method{plot}{idw}(x, ...)
\method{plot}{automap}(x, ...)
%\method{plot}{psgp}(x, ...)
\method{plot}{linearVariogram}(x, ...)
\method{plot}{transGaussian}(x, ...)
\method{plot}{yamamoto}(x, ...)
}
\arguments{
\item{object}{  a list object. Most arguments necessary for interpolation
are passed through this object. See \code{\link{intamap-package}} for further 
description of the necessary content of this variable}
\item{x}{intamap object, when plot is called directly}
\item{zcol}{a list of column names to be plotted; if equal to all, the column names
            will correspond to all possible column names from \code{outputWhat}
            (see \code{\link{createIntamapObject}}) }
\item{sp.layout}{an object that can contain lines, points and polygons that function as extra layout;
           see \code{\link[sp]{spplot}} for more information}
\item{plotMat}{an array of lengt two with the number of rows and columns of plots per page}
\item{...}{ other parameters that can be passed to other plot functions (e.g. 
           \code{\link{plot}}, \code{\link[sp]{spplot}}, \code{\link[automap]{automapPlot}}
           and \code{\link[lattice]{xyplot}} }
} 

\value{ 
A plot of some of the elements of \code{object}. This will typically be the 
sample variogram and the fitted variogram model (if a method based on variograms 
has been used) and all the predictions. 
}


\details{
All the plot methods above are simple wrapper functions around the plotIntamap function.
}

\references{ 
Pebesma, E., Cornford, D., Dubois, G., Heuvelink, G.B.M., Hristopulos, D., Pilz, J., Stohlker, U., Morin, G., Skoien, J.O. INTAMAP: The design and implementation f an interoperable automated interpolation Web Service. Computers and Geosciences 37 (3), 2011. 
}

\examples{
data(meuse)
meuse$value = log(meuse$zinc)
data(meuse.grid)
coordinates(meuse) = ~x+y
coordinates(meuse.grid) = ~x+y
object = interpolate(meuse, meuse.grid, 
                     outputWhat = list(mean = TRUE, variance = TRUE, 
                     excprob = 7, excprob = 8, quantile = 0.9, quantile = 0.95),
                     methodName = "automap")
plot(object)
}

\author{ Jon Olav Skoien }
\keyword{spatial}
