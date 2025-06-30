\name{predictTime}
\alias{predictTime}

\title{Time prediction for intamap package methods}

\description{Functions that gives a time estimate for an intamap function given the 
number of observations and predictionLocations}

\usage{predictTime(nObs, nPred, class, formulaString, calibration=FALSE, 
                   outputWhat, FUN = "spatialPredict",...)}

\arguments{
\item{nObs}{An integer or an array of integers containing the number of observations.}
\item{nPred}{An integer or an array of integers containing the number of predictions.}
\item{class}{class of intamapObject, which interpolation method to be used}
\item{formulaString}{the formula of the request, mainly to see if the request has independent variables}
\item{calibration}{enables or disables time calibration - not properly implemented yet}
\item{outputWhat}{List defining the requested type of output, see 
        \code{\link{createIntamapObject}} }
\item{FUN}{A string with the intamap functions name, now obsolete}
\item{...}{other arguments needed to define the intamap object.}
}
 
\details{The function is based on \code{timeModels} being available in the workspace. 
         This is a \code{\link[stats:loess]{loess}}-model that has been fitted to different calls to a range of 
         interpolation requests with synthetically generated data in
         \code{\link{generateTimeModels}}. }


\note{RUN FIRST generateTimeModels() or estimateTimeModel() in order to save time Models to workspace. It might take some time!}

\value{An integer or an array of integers with the predicted times.}

\references{ 
Pebesma, E., Cornford, D., Dubois, G., Heuvelink, G.B.M., Hristopulos, D., Pilz, J., Stohlker, U., Morin, G., Skoien, J.O. INTAMAP: The design and implementation f an interoperable automated interpolation Web Service. Computers and Geosciences 37 (3), 2011. 
}
