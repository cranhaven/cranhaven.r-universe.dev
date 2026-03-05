\name{hweibull}
\title{Log Hazard Function for a Weibull Process}
\alias{hweibull}
\usage{
hweibull(y, s, m)
}
\description{Log Hazard Function for a Weibull Process}
\arguments{
\item{y}{Vector of times.}
\item{s}{Shape parameter.}
\item{m}{Scale parameter.}
}
\value{
\code{hweibull} returns the log hazard function for a Weibull
process with the given parameter values.
}
\author{J.K. Lindsey}
\seealso{
\code{\link[event]{hboxcox}}, \code{\link[event]{hburr}},
\code{\link[event]{hexp}}, \code{\link[event]{hgextval}},
\code{\link[event]{hcauchy}}, \code{\link[event]{hgamma}},
\code{\link[event]{hggamma}}, \code{\link[event]{hhjorth}},
\code{\link[event]{hinvgauss}}, \code{\link[event]{hlaplace}},
\code{\link[event]{hlogis}}, \code{\link[event]{hglogis}},
\code{\link[event]{hlnorm}}, \code{\link[event]{hnorm}},
\code{\link[event]{hstudent}}, \code{\link[event]{hgweibull}}.
}
\examples{
hweibull(1:10, 1.5, 2)
}
\keyword{distribution}
