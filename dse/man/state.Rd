\name{state}
\alias{state}
\title{Extract State}
\description{Extract state information from estimated SS model.}
\usage{
    state(obj, smoother=FALSE, filter=!smoother)
}
\arguments{
    \item{obj}{
    An object of class `TSestModel' with state information (filter or smoother)
     or containing an 'SS' model from which to estimate the state.}
    \item{smoother}{logical indicating if the smoother state should be returned.}.
    \item{filter}{logical indicating if the filtered state should be returned.}.
}
\value{A time series matrix of the estimated state series.}
\details{One and only one of smoother and filter should be TRUE).}
\seealso{
    \code{\link{smoother}},
    \code{\link{SS}},
    \code{\link{l.SS}}
    }

\concept{DSE}
\keyword{ts}

