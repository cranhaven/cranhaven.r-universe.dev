\name{loadOpenBUGS}
\alias{loadOpenBUGS}
\title{Load OpenBUGS from given directory}
\description{Load OpenBUGS from given directory, in case it was not
  detected on installation of BRugs.}
\usage{
loadOpenBUGS(dir)
}
\arguments{
\item{dir}{Directory where OpenBUGS is installed, typically something
  like \code{"c:/Program Files/OpenBUGS/OpenBUGS321"}. This should
  contain the OpenBUGS shared library \code{libOpenBUGS.dll}. }
}
\value{
  Returns silently if the library was loaded successfully.
}
\keyword{interface}
\concept{OpenBUGS}
\concept{WinBUGS}
\concept{MCMC}
