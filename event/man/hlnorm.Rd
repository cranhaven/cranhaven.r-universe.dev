\name{hlnorm}
\title{Log Hazard Function for a Log Normal Process}
\alias{hlnorm}
\usage{
hlnorm(y, m, s)
}
\description{Log Hazard Function for a Log Normal Process}
\arguments{
\item{y}{Vector of times.}
\item{m}{Mean parameter.}
\item{s}{Variance parameter.}
}
\value{
\code{hlnorm} returns the log hazard function for a log normal
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
\code{\link[event]{hnorm}}, \code{\link[event]{hstudent}},
\code{\link[event]{hweibull}}, \code{\link[event]{hgweibull}}.
}
\examples{
hlnorm(1:10, 3, 2)
}
\keyword{distribution}
