\title{Object Conversion}
\name{conversion}
\alias{mrss.to.mdes}
\alias{mrss.to.power}
\alias{power.to.mdes}
\alias{mdes.to.power}
\alias{mdes.to.pctl}

\description{
  Use \code{mrss.to.mdes()} to convert an object returned from MRSS functions into an object returned from MDES functions, \code{mrss.to.power()} to convert an object returned from MRSS functions into an object returned from power functions, \code{power.to.mdes()} to convert an object returned from power functions into an object returned from MDES functions, \code{mdes.to.power()} to convert an object returned from MDES functions into an object returned from power functions, and \code{mdes.to.pctl()} to convert effect sizes or an object returned from MDES functions into percentiles.
}

\usage{
  mrss.to.mdes(object)
  mrss.to.power(object)
  power.to.mdes(object)
  mdes.to.power(object)
  mdes.to.pctl(object)
}

\arguments{
  \item{object}{an object returned from one of the functions in \pkg{PowerUpR} package.}
}


\examples{
design1 <- power.bira2(es=.15, rho2=.35, omega2=.10, n=83, J=10)
design2 <- power.to.mdes(design1)
mdes.to.pctl(design2)
}
