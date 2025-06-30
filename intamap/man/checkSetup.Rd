\name{checkSetup}
\alias{checkSetup}
\title{ check setup }
\description{
checkSetup will do some sanity checks on input data provided through object.
}
\usage{
checkSetup(object, quiet = FALSE)
}
\arguments{
\item{object}{ object, to be passed to \code{\link{preProcess}}, see \code{\link{intamap-package}}}
\item{quiet}{ logical; TRUE to suppress OK statement }
} 

\details{
 \code{checkSetup} is a function that makes certain tests on the intamap object to 
 make sure that it is suited for interpolation. Particularly, it will issue a warning
 or an error if one of the following conditions are met:
 \itemize{
   \item \code{observations} is not an element of \code{object}
   \item \code{observations} contain less than 20 observations
   \item Some of the observation locations are duplicated
   \item \code{formulaString} is not an element of \code{object}
   \item None of the columns of \code{observations} has a name that corresponds to the independent variable of \code{formulaString} 
   \item \code{predictionLocations} is not an element of \code{object}
   \item \code{predictionLocations} is not a \code{\link[sp:Spatial-class]{Spatial}} object
   \item \code{targetCRS} is given, but \code{observations} and \code{predictionLocations} do not have CRS set
   \item \code{addBias} includes biases that are not part of \code{removeBias}
  }
  The function will issue a warning if it appears that \code{predictionLocations} 
  and \code{observations} share a small region. This warning is given as it is 
  a likely cause of errors, although it can also happen if \code{predictionLocations}
  are limited to one small cluster.
}

\value{ 
returns TRUE if check passes, will halt with error when some some error condition is met.
}

\references{ 

Pebesma, E., Cornford, D., Dubois, G., Heuvelink, G.B.M., Hristopulos, D., Pilz, J., Stohlker, U., Morin, G., Skoien, J.O. INTAMAP: The design and implementation f an interoperable automated interpolation Web Service. Computers and Geosciences 37 (3), 2011. 

}

\author{ Edzer J. Pebesma }
\keyword{spatial}



