\name{dwdss}
\alias{dwdss}
\title{second derivative function of profit in GOM dataset}
\usage{
dwdss(s,Z)
}
\arguments{
\item{s}{stock}

\item{Z}{parameter vector}
}
\value{
The second derivative of w with respect to s
}
\description{
\code{dwdss} evaluated \eqn{\frac{d}{ds}(\frac{dw}{ds})} in the reef-fishy example of GOM dataset (Fenichel and Abbott, 2014).
}
\details{
This function is adopted in \code{GOM} dataset.
}
\references{
Fenichel, Eli P. and Joshua K. Abbott. (2014) "\href{http://www.journals.uchicago.edu/doi/abs/10.1086/676034}{Natural Capital: From Metaphor to Measurement}."
             \emph{Journal of the Association of Environmental Economists}. 1(1/2):1-27.
}
\seealso{
\code{\link{GOM}}
}
