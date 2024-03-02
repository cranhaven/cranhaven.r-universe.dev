\name{observability}
\alias{observability}
\alias{observability.ARMA}
\alias{observability.SS}
\alias{observability.TSestModel}

\title{Calculate Model Observability Matrix}
\description{
    Calculate the singular values of the observability matrix of a model.
}
\usage{
    observability(model)
    \method{observability}{ARMA}(model)
    \method{observability}{SS}(model)
    \method{observability}{TSestModel}(model)
}
\arguments{
    \item{model}{An object containing a TSmodel.}
}
\value{The singular values of the observability matrix.}
\details{
    If all singular values are significantly different from zero the
    model is observable.
}
\seealso{
    \code{\link{reachability}},
    \code{\link{stability}}
    \code{\link{McMillanDegree}}
}
\examples{
    data("eg1.DSE.data.diff", package="dse")
    model <- toSS(estVARXls(eg1.DSE.data.diff))
    observability(model)
}
\concept{DSE}
\keyword{ts}

