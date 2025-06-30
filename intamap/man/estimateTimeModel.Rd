\name{estimateTimeModel}
\alias{estimateTimeModel}

\title{estimateTimeModel}

\description{Function that takes time samples function that can read intamap objects}

\usage{estimateTimeModel(FUN, class, formulaString, debug.level, ...)}

\arguments{
\item{FUN}{A string with function's name}
\item{class}{class of intamapObject, which interpolation method to be used.}
\item{formulaString}{the formula of the request, mainly to see if the request has independent variables.}
\item{debug.level}{if \code{debug.level >= 1}, the function will store 
            tables with the prediction times for each model in the workspace.}
\item{...}{other arguments as defined in the \link{createIntamapObject} function}
} 
\details{
This function uses createIntamapObject function to create synthetic data, in order to 
take time samples for the function with string name "FUN". The calculated model is stored, 
as an element of a list, in a local file (workspace for now) and it's used in order to give quick time estimates.
}



\value{The function does not return a variable but stores the result in an element list with the same name.}

\references{ 

Pebesma, E., Cornford, D., Dubois, G., Heuvelink, G.B.M., Hristopulos, D., Pilz, J., Stohlker, U., Morin, G., Skoien, J.O. INTAMAP: The design and implementation f an interoperable automated interpolation Web Service. Computers and Geosciences 37 (3), 2011. 

}

\keyword{spatial}
