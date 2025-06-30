\name{coarsenGrid}
\alias{coarsenGrid}
\title{ Coarsening of a spatial grid}

\description{\code{coarsenGrid} is a function that resamples a SpatialGridDataFrame.
}


\usage{
coarsenGrid(object,coarse=2,offset = sample(c(0:(coarse-1)),2,replace=TRUE))
}

\arguments{
\item{object}{a \code{\link[sp:SpatialPixelsDataFrame]{SpatialGridDataFrame}} or gridded
              \code{\link[sp:SpatialPixelsDataFrame]{SpatialPixelsDataFrame}}}
\item{coarse}{an integer telling how much the grid should be coarsened}
\item{offset}{integer giving the relative offset of the first point, see details 
                below for a closer description} 
} 


\details{
The function \code{coarsenGrid} is a function that samples from a 
\code{\link[sp:SpatialPixelsDataFrame]{SpatialGridDataFrame}}.
The argument \code{coarse} indicates that every \code{coarse} row and column will
be sampled, starting with the row and column represented in \code{offset}. offset = c(0,0) 
implies that the smallest x- and y-coordinates will be a part of the resampled
data set, offset = c(1,1) implies that sampling will start on the second row and column.
}

\value{ 
a \code{\link[sp:SpatialPixelsDataFrame]{SpatialGridDataFrame}}.
}


\references{ 

Pebesma, E., Cornford, D., Dubois, G., Heuvelink, G.B.M., Hristopulos, D., Pilz, J., Stohlker, U., Morin, G., Skoien, J.O. INTAMAP: The design and implementation of an interoperable automated interpolation Web Service. Computers and Geosciences 37 (3), 2011. 

}

\author{ Jon Olav Skoien}
\examples{
data(meuse.grid)
gridded(meuse.grid) = ~x+y
newMeuseGrid = coarsenGrid(meuse.grid,coarse=2,offset=c(1,1))
}
\keyword{spatial}
