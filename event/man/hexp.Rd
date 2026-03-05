\name{hexp}
\title{Log Hazard Function for a Poisson Process}
\alias{hexp}
\usage{
hexp(y, rate)
}
\description{Log Hazard Function for a Poisson Process}
\arguments{
\item{y}{Vector of times.}
\item{rate}{Vector of rates.}
}
\value{
\code{hexp} returns the log hazard function for a Poisson process with
the given parameter value.
}
\author{J.K. Lindsey}
\seealso{
\code{\link[event]{hboxcox}}, \code{\link[event]{hburr}},
\code{\link[event]{hexp}}, \code{\link[event]{hgextval}},
\code{\link[event]{hgamma}}, \code{\link[event]{hggamma}},
\code{\link[event]{hhjorth}}, \code{\link[event]{hinvgauss}},
\code{\link[event]{hlaplace}}, \code{\link[event]{hlnorm}},
\code{\link[event]{hlogis}}, \code{\link[event]{hglogis}},
\code{\link[event]{hnorm}}, \code{\link[event]{hstudent}},
\code{\link[event]{hweibull}}, \code{\link[event]{hgweibull}}.
}
\examples{
hexp(1:10, 3)
}
\keyword{distribution}
