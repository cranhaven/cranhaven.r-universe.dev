\name{hnorm}
\title{Log Hazard Function for a Normal Process}
\alias{hnorm}
\usage{
hnorm(y, m, s)
}
\description{Log Hazard Function for a Normal Process}
\arguments{
\item{y}{Vector of times.}
\item{m}{Mean parameter.}
\item{s}{Variance parameter.}
}
\value{
\code{hnorm} returns the log hazard function for a normal
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
\code{\link[event]{hlnorm}}, \code{\link[event]{hstudent}},
\code{\link[event]{hweibull}}, \code{\link[event]{hgweibull}}.
}
\examples{
hnorm(1:10, 3, 2)
}
\keyword{distribution}
