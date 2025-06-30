\name{intamapExampleObject}
\alias{intamapExampleObject}
\docType{data}
\title{Simulated Intamap Object}
\description{
Intamap object of class "copula" containing a simulated data set with 196 spatial locations. 
}
\usage{data(intamapExampleObject)}
%\format{
% The format is:
%
%List of 6
% $ observations       :Formal class 'SpatialPointsDataFrame' [package "sp"] with 5 slots\cr
%  .. ..@ data       :'data.frame':      196 obs. of  1 variable:                 \cr
%  .. .. ..$ value: num [1:196] 9.77 9.75 9.87 9.51 9.38 ...                         \cr
%  .. ..@ coords.nrs : int [1:2] 1 2                                                    \cr
%  .. ..@ coords     : num [1:196, 1:2] 1 1 1 1 1 1 1 1 1 1 ...                            \cr
%  .. .. ..- attr(*, "dimnames")=List of 2                                                    \cr
%  .. .. .. ..$ : NULL                                                                          \cr
%  .. .. .. ..$ : chr [1:2] "x" "y"                                                                \cr
%  .. ..@ bbox       : num [1:2, 1:2] 1 1 14 14                                                       \cr
%  .. .. ..- attr(*, "dimnames")=List of 2                                                               \cr
%  .. .. .. ..$ : chr [1:2] "x" "y"                                                                         \cr
%  .. .. .. ..$ : chr [1:2] "min" "max"                                                                        \cr
%  .. ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slots                                              \cr
%  .. .. .. ..@ projargs: chr NA                                                   \cr
% $ formulaString      :Class 'formula' length 3 value ~ 1                            \cr
%  .. ..- attr(*, ".Environment")=<environment: 0x022e3c7c>                              \cr
% $ predictionLocations:Formal class 'SpatialPoints' [package "sp"] with 3 slots            \cr
%  .. ..@ coords     : num [1:196, 1:2] 1.5 1.5 1.5 1.5 1.5 1.5 1.5 1.5 1.5 1.5 ...            \cr
%  .. .. ..- attr(*, "dimnames")=List of 2                                                        \cr
%  .. .. .. ..$ : NULL                                                                               \cr
% .. ..@ bbox       : num [1:2, 1:2] 1.5 1.5 14.5 14.5                                                    \cr
 % .. .. ..- attr(*, "dimnames")=List of 2                                                                    \cr
 % .. .. .. ..$ : chr [1:2] "x" "y"                                                                              \cr
 % .. .. .. ..$ : chr [1:2] "min" "max"                                               \cr
 % .. ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slots                     \cr
%  .. .. .. ..@ projargs: chr NA                                                            \cr
% $ params             :List of 11                                                             \cr
%  ..$ doAnisotropy     : logi TRUE                                                               \cr
%  ..$ removeBias       : logi NA                                                                    \cr
%  ..$ addBias          : logi NA                                                                       \cr
%  ..$ biasRemovalMethod: chr "LM"                                                                         \cr
%  ..$ doSegmentation   : logi FALSE                                                                          \cr
%  ..$ nmax             : num Inf                                                                                \cr
%  ..$ ngrid            : num 100                                                                                   \cr
%  ..$ nsim             : num 100                                                                                      \cr
%  ..$ processType      : chr "gaussian"                                                \cr
%  ..$ confProj         : logi FALSE                                                       \cr
%  ..$ debug.level      : num 0                                                               \cr
%  ..- attr(*, "class")= chr "IntamapParams"                                                     \cr
% $ outputWhat         :List of 4                                                                   \cr
%  ..$ mean    : logi TRUE                                                                             \cr
%  ..$ variance: logi TRUE                                                                                \cr
%  ..$ excprob : num 10                                                                                      \cr
%  ..$ quantile: num 0.9                                                                                        \cr
% $ blockWhat          : chr "none"                                                                                \cr
% - attr(*, "class")= chr "copula"                                                                                    \cr
%}
\details{
The data set is 
a realization of a random field generated using a Gaussian copula and generalized extreme value distributed margins (location=40,shape=0.5, scale=30). The correlation function is Matern (Stein's representation) with range=4, kappa=3 and nugget=0.05.
Furthermore, there is geometric anisotropy with direction=pi/3 and ratio=2. 
}
\seealso{\code{\link{spatialPredict.copula}}, \code{\link{estimateParameters.copula}}}

\references{ 
Pebesma, E., Cornford, D., Dubois, G., Heuvelink, G.B.M., Hristopulos, D., Pilz, J., Stohlker, U., Morin, G., Skoien, J.O. INTAMAP: The design and implementation f an interoperable automated interpolation Web Service. Computers and Geosciences 37 (3), 2011. 
}

\examples{
\dontrun{
data(intamapExampleObject)
## estimate parameters for the copula model
intamapExampleObject<-estimateParameters(intamapExampleObject)
## make predictions at unobserved locations
intamapExampleObject<-spatialCopula(intamapExampleObject)
}
}
\keyword{datasets}
