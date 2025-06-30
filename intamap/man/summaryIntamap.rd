\name{summaryIntamap}
\alias{summaryIntamap}
\alias{summary.copula}
\alias{summary.idw}
\alias{summary.automap}
%\alias{summary.psgp}
\alias{summary.transGaussian}
\alias{summary.linearVariogram}
\alias{summary.yamamoto}

\title{ summary intamap objects}
\description{
summary function for \code{intamap}-objects of the type described in 
\code{\link{intamap-package}}
}
\usage{
summaryIntamap(object, ...)
\method{summary}{copula}(object, ...)
\method{summary}{idw}(object, ...)
\method{summary}{automap}(object, ...)
%\method{summary}{psgp}(object, ...)
\method{summary}{linearVariogram}(object, ...)
\method{summary}{transGaussian}(object, ...)
\method{summary}{yamamoto}(object, ...)
}
\arguments{
\item{object}{  a list object. Most arguments necessary for interpolation
are passed through this object. See \code{\link{intamap-package}} for further 
description of the necessary content of this variable}
\item{...}{parameters to be passed to the default summary function for each element}
} 

\value{ 
A summary of some of the elements of \code{object}. 
}


\references{ 
Pebesma, E., Cornford, D., Dubois, G., Heuvelink, G.B.M., Hristopulos, D., Pilz, J., Stohlker, U., Morin, G., Skoien, J.O. INTAMAP: The design and implementation f an interoperable automated interpolation Web Service. Computers and Geosciences 37 (3), 2011. 
}

\author{ Jon Olav Skoien }
\keyword{spatial}
