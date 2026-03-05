\name{hlogis}
\title{Log Hazard Function for a Logistic Process}
\alias{hlogis}
\usage{
hlogis(y, m, s)
}
\description{Log Hazard Function for a Logistic Process}
\arguments{
\item{y}{Vector of times.}
\item{m}{Location parameter.}
\item{s}{Scale parameter.}
}
\value{
\code{hlogis} returns the log hazard function for a logistic
process with the given parameter values.
}
\author{J.K. Lindsey}
\seealso{
\code{\link{hboxcox}}, \code{\link{hburr}}, \code{\link{hexp}},
\code{\link{hgextval}}, \code{\link{hcauchy}}, \code{\link{hgamma}},
\code{\link{hggamma}}, \code{\link{hhjorth}}, \code{\link{hinvgauss}},
\code{\link{hlaplace}}, \code{\link{hlnorm}}, \code{\link{hglogis}},
\code{\link{hnorm}}, \code{\link{hstudent}}, \code{\link{hweibull}},
\code{\link{hgweibull}}.
}
\examples{
hlogis(1:10, 3, 2)
}
\keyword{distribution}
