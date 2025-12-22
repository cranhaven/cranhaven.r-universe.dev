\name{residuals.mmdml}
\alias{residuals.mmdml}
\alias{resid.mmdml}
\title{Confidence Intervals for coefficient estimates of regsDML fits}
\usage{
\method{residuals}{mmdml}(object, scaled = FALSE, ...)
}
\arguments{
\item{object}{An object of class \code{mmdml}. This object usually results
    from a function call to \code{\link{mmdml}}.}

\item{scaled}{A boolean specifying whether scaled residuals should be returned.
    It is set to FALSE by default.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
A list whose elements correspond to the first \code{nr_res} sets of
    residuals of the \code{S} residuals.
}
\description{
A list whose elements correspond to the potentially scaled first
    \code{nr_res} sets of
    residuals of the \code{S} residuals.
}
\examples{
## See example(mmdml) for examples
}
\seealso{
\code{\link{mmdml}}
}
