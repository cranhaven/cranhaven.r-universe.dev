\name{preProcess}
\alias{preProcess}
\alias{preProcess.default}
\alias{preProcess.idw}
\title{ pre-processing of data}
\description{ pre-processing of the data for the \code{\link{intamap-package}} package. 
              
%              (Replacable) function for pre-processing of data, 
%              able to interact with bias correction functions in  the package
%              \code{intamapInteractive}.
}
\usage{
\method{preProcess}{default}(object, ...)
\method{preProcess}{idw}(object, ...)
}
\arguments{
\item{object}{ see \code{\link{intamap-package}}; list that should at
  least contain (i) an element called \code{observations} of class
  \code{\link[sp:SpatialPoints]{SpatialPointsDataFrame}}. The measured values should be named
  \code{value}, and (ii) an element \code{params} of class \code{list}, by calling the
  function \code{\link{getIntamapParams}}. (iii) Usually, the object will also
  contain an element called \code{predictionLocations},
  of a class extending \code{\link[sp:Spatial-class]{Spatial}}. 
  
}
\item{...}{Additional parameters}
% to the bias correction functions in the 
%           package \code{intamapInteractive} that can be called by
%           preProcess, such as \code{\link[intamapInteractive]{findLocalBias}},
%           \code{\link[intamapInteractive]{removeLocalBias}}, \code{\link[intamapInteractive]{findBoundaryLines}},
%           \code{\link[intamapInteractive]{findRegionalBias}} and \code{\link[intamapInteractive]{removeRegionalBias}}}
}
\details{
The function \code{preProcess} includes code for preprocessing an object before interpolation. 
The function can easily be replaced by more specific methods relevant for a 
certain data set. Functions can be called from \code{preProcess} according to the settings in parameters
in the \code{object}, set by the function \code{\link{getIntamapParams}}. 
%The default version allows for bias correction, 
%by calling \code{\link[intamapInteractive]{biasCorr}} in the package
%\code{intamapInteractive}.

}


\value{ 
The input object is returned, after its components have been pre-processed.
}

\references{ 

Pebesma, E., Cornford, D., Dubois, G., Heuvelink, G.B.M., Hristopulos, D., Pilz, J., Stohlker, U., Morin, G., Skoien, J.O. INTAMAP: The design and implementation f an interoperable automated interpolation Web Service. Computers and Geosciences 37 (3), 2011. 

}

\author{ Jon Olav Skoien }
\keyword{spatial}
