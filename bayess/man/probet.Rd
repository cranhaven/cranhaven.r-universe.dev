\name{probet}
\alias{probet}
\title{
Coverage of the interval \eqn{(a,b)} by the Beta cdf
}
\description{
This function computes the coverage of the interval \eqn{(a,b)} by the Beta 
\eqn{\mathrm{B}(\alpha,\alpha (1-c)/c)} distribution.
}
\usage{
probet(a, b, c, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
lower bound of the prior 95\%~confidence interval
}
  \item{b}{
upper bound of the prior 95\%~confidence interval
}
  \item{c}{
mean parameter of the prior distribution
}
  \item{alpha}{
scale parameter of the prior distribution
}
}
\value{
numerical value between 0 and 1 corresponding to the coverage
}
\seealso{
\code{\link{solbeta}}
}
\examples{
probet(.1,.5,.3,2)
}
\keyword{capture-recapture}
\keyword{prior elicitation}
\keyword{beta distribution}
