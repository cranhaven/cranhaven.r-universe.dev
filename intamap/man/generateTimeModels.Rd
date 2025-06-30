\name{generateTimeModels}
\alias{generateTimeModels}

\title{Generate time models}

\description{function that generates time models and saves them in workspace.}

\usage{generateTimeModels(genClasses = NULL, noGenClasses = NULL, nSam = 1, test = FALSE,
             debug.level = 0)}

\arguments{
\item{genClasses}{list of particular classes for which time models should be generated}
\item{noGenClasses}{list of particular classes for which time models should not be generated}
\item{nSam}{number of attempts to be tried for each combination of predictions and observations,
            defaults to 1, higher number should be used for better accuracy. nSam/2 is used for
            copulas, to reduce computation time.}
\item{test}{logical; if true, the time models are generated based on fewer iterations, for speed }
\item{debug.level}{if \code{debug.level >= 1}, the function will store 
            tables with the prediction times for each model in the workspace.}
} 

\value{
The function generates a \code{timeModels} object, which can be used to estimate
prediction times for different requests to the \code{\link{interpolate}} function
in the \code{\link{intamap-package}}, via \code{\link{predictTime}}.
}




\details{This function calculates a time model for different interpolation types
          in the \code{\link{intamap-package}} and returns a list object
					with the estimated models. It's users responsibility to store the model in the 
					workspace. The normal procedure would be to run the function without arguments. However,
          it is both possible to define a list for which classes the user want to 
          generate models, or a list of classes that are not of interest.
          
          The time model is based on creation of a set of synthetical data sets
          of different size, both regarding number of observations and prediction locations.
          The function will estimate parameters and make predictions with the different combinations,
          and for each method, fit a local polynomial regression model (\code{\link[stats:loess]{loess}}) 
          
          This model can then be used by \code{\link{predictTime}} to estimate the 
          prediction time for an interpolation request with a certain number 
          of observations and prediction locations. }
\examples{
\dontrun{
timeModels=generateTimeModels()
q("yes")
## restart R in the same directory
}
}

\references{ 

Pebesma, E., Cornford, D., Dubois, G., Heuvelink, G.B.M., Hristopulos, D., Pilz, J., Stohlker, U., Morin, G., Skoien, J.O. INTAMAP: The design and implementation f an interoperable automated interpolation Web Service. Computers and Geosciences 37 (3), 2011. 

}
\keyword{spatial}
