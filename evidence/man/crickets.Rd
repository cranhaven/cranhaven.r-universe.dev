\name{crickets}
\alias{crickets}
\docType{data}
\title{ Made-up data that are not unlike the actual data collected by
  Nespolo et al.(2003). }
\description{
  Nespolo et al.(2003) collected data on the metabolic rates (as
  measured by oxygen consumption) of crickets kept and acclimated at three
  different temperatures. Since the original data were not available and
  only a statistical summary was published, we simulated these data to
  approximately agree with the statistical summary.
}
\usage{data(crickets)}
\format{
  A data frame with 292 observations on the following 3 variables:
  \describe{
    \item{\code{VO2}}{oxygen consumption in \eqn{\mu}l/h (a measure of basal metabolic rate)}
    \item{\code{mass}}{weight of the cricket in mg}
    \item{\code{temp}}{temperature in degrees C.}
  }
}
\source{Nespolo et al., 2003.
    }
\references{
      Nespolo, R.F., Lardies, M.A., and Bozinovic,
    F. 2003. Intrapopulational variation in the standard metabolic rate
    of insects: repeatability, thermal dependence and sensitivity of
    (Q[10]) on oxygen consumption in a cricket. Journal of
    Experimental Biology 206, 4309--4315.
    
    van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
  }
\examples{
data(crickets)
crickets7 <- subset(crickets, crickets$temp==7)
with(crickets7, scatter.smooth(mass, VO2))
}
\keyword{datasets}
