\name{lightspeed}
\alias{lightspeed}
\docType{data}
\title{ Simon Newcomb's measurements of the speed of light }
\description{
  Simon Newcom's measured in the late 1900's the time it took light to cover a
  certain distance. The data are reported in Stigler(1977) and have
  been widely used since to illustrate statistical inference.
}
\usage{data(lightspeed)}
\format{
  A vector with 66 observations of the travel time of light.
  }
\source{
  Stigler, S.M. (1977) Do robust estimators work with real data? Annals
  of Statistics 5, 1055--1098.
  }
\references{
  van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\examples{
data(lightspeed)
qqnorm(lightspeed)
qqline(lightspeed)
}
\keyword{datasets}
