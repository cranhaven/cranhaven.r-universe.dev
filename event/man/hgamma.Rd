\name{hgamma}
\title{Log Hazard Function for a Gamma Process}
\alias{hgamma}
\usage{
hgamma(y, shape, rate=1, scale=1/rate)
}
\description{Log Hazard Function for a Gamma Process}
\arguments{
\item{y}{Vector of times.}
\item{shape}{Shape parameter.}
\item{rate}{Rate parameter.}
\item{scale}{Scale parameter.}
}
\value{
\code{hgamma} returns the log hazard function for a gamma process with
the given parameter values.
}
\author{J.K. Lindsey}
\seealso{
\code{\link[event]{hboxcox}}, \code{\link[event]{hburr}},
\code{\link[event]{hexp}}, \code{\link[event]{hgextval}},
\code{\link[event]{hcauchy}}, \code{\link[event]{hggamma}},
\code{\link[event]{hhjorth}}, \code{\link[event]{hinvgauss}},
\code{\link[event]{hlaplace}}, \code{\link[event]{hlnorm}},
\code{\link[event]{hlogis}}, \code{\link[event]{hglogis}},
\code{\link[event]{hnorm}}, \code{\link[event]{hstudent}},
\code{\link[event]{hweibull}}, \code{\link[event]{hgweibull}}.
}
\examples{
hgamma(1:10, 3, 2)
}
\keyword{distribution}
