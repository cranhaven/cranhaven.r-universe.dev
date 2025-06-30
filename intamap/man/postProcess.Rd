\name{postProcess}
\alias{postProcess}
\alias{postProcess.default}
\alias{postProcess.idw}
\title{ pre-process data}
\description{
post-processing of data for the \code{\link{intamap-package}}. The function will typically call
other functions for adding back biases, aggregation etc.
}
\usage{
\method{postProcess}{default}(object, ...)
\method{postProcess}{idw}(object, ...)
}
\arguments{
\item{object}{  a list object. Most arguments necessary for interpolation
are passed through this object. See \code{\link{intamap-package}} for further 
description of the necessary content of this variable}
\item{...}{ other parameters that can be passed to functions called from \code{preProcess}}
} 

\value{ 
An object of same type as above, but with new elements. Most important from the default
method is the \code{outputTable}, a matrix, organized in a convenient way for the calling WPS;
     first row: x-coordinates,  second row: y-coordinates; further rows: 
     output elements as specified by \code{outputWhat} (see \code{\link{createIntamapObject}}
}

\details{
 The function \code{postProcess} includes code for postprocessing an object after interpolation. 
The function can easily be replaced by more specific methods relevant for a 
certain data set, doing more data specific things in addition to what is done in the default method.
}

\references{ 
Pebesma, E., Cornford, D., Dubois, G., Heuvelink, G.B.M., Hristopulos, D., Pilz, J., Stohlker, U., Morin, G., Skoien, J.O. INTAMAP: The design and implementation f an interoperable automated interpolation Web Service. Computers and Geosciences 37 (3), 2011. 
}

\author{ Edzer J. Pebesma }
\keyword{spatial}
