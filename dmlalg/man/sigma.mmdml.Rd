\name{sigma.mmdml}
\alias{sigma.mmdml}
\title{Extract Residual Standard Deviation 'Sigma' from mmdml Fits}
\usage{
\method{sigma}{mmdml}(object, ...)
}
\arguments{
\item{object}{An object of class \code{mmdml}. This object usually results
from a function call to \code{\link{mmdml}}.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
A number representing the estimated standard deviation.
    First, for each of the \code{S} repetitions, the standard deviations computed
    on the \code{K} sample splits are aggregated by taking the mean.
    Second, the \code{S} mean-aggregated estimates are aggregated by
    the median. This final value is returned.
}
\description{
Extract the estimated standard deviation of the errors,
    the “residual standard deviation”,
    from a fitted \code{mmdml} model.
}
\examples{
## See example(mmdml) for examples
}
\seealso{
\code{\link{mmdml}}
}
