\name{hstudent}
\title{Log Hazard Function for a Student t Process}
\alias{hstudent}
\usage{
hstudent(y, m, s, f)
}
\description{Log Hazard Function for a Student t Process}
\arguments{
\item{y}{Vector of times.}
\item{m}{Location parameter.}
\item{s}{Scale  parameter.}
\item{f}{Degrees of freedom.}
}
\value{
\code{hstudent} returns the log hazard function for a Student t
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
\code{\link[event]{hnorm}}, \code{\link[event]{hnorm}},
\code{\link[event]{hweibull}}, \code{\link[event]{hgweibull}}.
}
\examples{
hstudent(1:10, 3, 2, 5)
}
\keyword{distribution}
