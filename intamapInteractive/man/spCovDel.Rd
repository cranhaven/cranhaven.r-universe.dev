\name{spCovDel}
\alias{spCovDel}
\title{Optimize the network with spatial coverage methods}
\description{The function spCovDel allows to build optimization scenarios based on spatial coverage method.
}

\usage{
spCovDel(observations, candidates, nDiff, plotOptim = TRUE, ...)
}

\arguments{
\item{observations}{ object of class \code{\link[base]{data.frame}} with x,y coordinates}
\item{candidates}{ not compulsory used only for plotting purpose -- a 
         \code{\link[sp:SpatialPolygons]{SpatialPolygonsDataFrame}} describing the study area }
\item{nDiff}{ number of stations to add or delete }
\item{plotOptim}{logical; to plot the result or not}
\item{...}{ other arguments to be passed on at lower level functions such as 
              \code{\link[spatstat.geom:nndist]{nndist}}}
}

\details{
This function allows to build optimization scenarios based on spatial coverage method. 
When action is "del", the function maximizes the mean distance of measurements with direct neighbours using function 
\code{\link[spatstat.geom:nndist]{nndist}}. The heuristic search uses a 
swapping algorithm to converge more rapidly to the best solution.
}

\value{
\code{\link[base]{data.frame}} of optimized locations
}

\author{ Olivier Baume }
\keyword{spatial}
