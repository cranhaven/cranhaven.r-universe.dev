\name{spCovAdd}
\alias{spCovAdd}
\title{Spatial coverage method to add new measurements}
\description{This function spCovAdd allows to build optimization scenarios based on spatial coverage method.
}
\usage{
spCovAdd( observations, candidates, nDiff, nGridCells, plotOptim = TRUE, nTry, ... )
}

\arguments{
\item{observations}{ object of class \code{\link[base:data.frame]{data.frame}} with x,y coordinates}
\item{candidates}{ a \code{\link[sp:SpatialPolygons]{SpatialPolygonsDataFrame}} to explore: in use when optimizing 
the implementation of new measurement stations to an existing network }
\item{nDiff}{ number of stations to add or delete }
\item{nGridCells}{ number of grid cells to work on spatial coverage strafication }
\item{plotOptim}{logical; to plot the result or not}
\item{nTry}{ the method will try \code{nTry} initial configurations and will keep 
    the best solution in order to reduce the risk of ending up with an unfavorable solution }
\item{...}{ other arguments to be passed on at lower level functions such as 
\code{\link[spcosa:stratify-methods]{stratify}} }
}

\details{
This function allows to build optimization scenarios based on spatial coverage method. 
The scenario action is "add". To add new measurement locations to the running network, 
the function uses function \code{\link[spcosa:stratify-methods]{stratify}} from package \code{spcosa}. 
Function stratify adds new strata to the domain study. 
}

\value{
\code{\link[base]{data.frame}} of optimized locations
}

\references{
D. J. Brus, J. de Gruijter, J. van Groenigen (2006). Designing spatial coverage samples using
the k-means clustering algorithm. In A. McBratney M. Voltz and P. Lagacherie,
editor, Digital Soil Mapping: An Introductory Perspective, Developments in Soil
Science, vol. 3., Elsevier, Amsterdam.

}
\author{ Olivier Baume }
\keyword{spatial}
