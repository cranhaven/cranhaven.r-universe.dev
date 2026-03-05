\name{hcauchy}
\title{	Log Hazard Function for a Cauchy Process}
\alias{hcauchy}
\usage{
hcauchy(y, m, s)
}
\description{	Log Hazard Function for a Cauchy Process}
\arguments{
\item{y}{Vector of times.}
\item{m}{Location parameter.}
\item{s}{Dispersion parameter.}
}
\value{
\code{hcauchy} returns the log hazard function for a Cauchy process with
the given parameter values.
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
hcauchy(1:10, 3, 2)
}
\keyword{distribution}
