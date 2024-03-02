\name{setTSmodelParameters}
\alias{setTSmodelParameters}
\alias{setTSmodelParameters.TSestModel}
\alias{setTSmodelParameters.ARMA}
\alias{setTSmodelParameters.SS}

\title{Set TSmodel Parameter Information}
\description{
    Complete parameter vector information based on parameter array settings.
    This function is used internally and is not normally called by a user. 
}
\usage{
    setTSmodelParameters(model, constants=model$constants)
    \method{setTSmodelParameters}{TSestModel}(model, constants=TSmodel(model)$constants)
    \method{setTSmodelParameters}{ARMA}(model, constants=model$constants)
    \method{setTSmodelParameters}{SS}(model, constants=model$constants)
}
\arguments{
    \item{model}{An object of class TSmodel.}
    \item{constants}{A list of logical arrays indicating TRUE for any model
    array entries that should be treated as constants.}
}
\value{An object of class 'TSmodel'.}
\seealso{
  \code{\link{setArrays}}
}
\examples{
    data("eg1.DSE.data.diff", package="dse")
    model <- estVARXls(eg1.DSE.data.diff)
    model <- setTSmodelParameters(model)
}
\concept{DSE}
\keyword{internal}

