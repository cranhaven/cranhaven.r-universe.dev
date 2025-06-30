\name{timeModels}
\alias{timeModels}
\title{models for estimating prediction time in intamap package}
\description{
This is a standard model for estimating the prediction time within the 
\code{\link{intamap-package}}. It was created by the function
\code{\link{generateTimeModels}}, on a 64 bits Linux server running 
\code{R} version 2.9.0 and \code{\link{intamap-package}} version 1.1.15. 

The prediction time will depend on the speed of the local machine, on the version
of \code{R} and \code{\link{intamap-package}}, and on the installed libraries. 
It is therefore strongly recommended to run \code{\link{generateTimeModels}} on the local
machine and store the result in the workspace if the predicted interpolation 
time is of real interest. \code{timeModels} in the workspace will be chosen if available.

It is not necessary to load the data set, this happens automatically in predictTime
if \code{timeModels} if the object is not already existing in the workspace.
}

\usage{
data(timeModels)
}
\author{Jon Olav Skoien}

\references{ 

Pebesma, E., Cornford, D., Dubois, G., Heuvelink, G.B.M., Hristopulos, D., Pilz, J., Stohlker, U., Morin, G., Skoien, J.O. INTAMAP: The design and implementation f an interoperable automated interpolation Web Service. Computers and Geosciences 37 (3), 2011. 

}
