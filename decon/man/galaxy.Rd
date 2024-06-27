\name{galaxy}
\docType{data}
\alias{galaxy}
\usage{data(galaxy)}
\title{The observed position-velocity data of low surface brightness galaxies}
\description{
The astronomical position-velocity data is from a sample of 26 low surfaces brightness (LSB) galaxies. The data contain 318 stars with their radiuses in kiloparsec (kpc), observed velocities of stars in km/s (relative to center, corrected for inclination) from 26 LSB galaxies. It was known that the velocities were measured with errors. In the data set, each  velocity includes its estimated standard deviation of measurement errors. 
}
\format{A data frame with 318 observations on 3 variables.
  \tabular{lll}{
    \code{Rkpc}  \tab numeric  \tab radiuses in kiloparsec (kpc) \cr
    \code{V}   \tab numeric  \tab velocities of stars in km/s \cr
    \code{Err}  \tab numeric  \tab standard deviations of measurement errors in measuring velocities \cr
  }
}
\seealso{\code{\link{DeconNpr}}}
\references{
De Blok, W.J.G., McGaugh, S.S., and Rubin, V.C. (2001).
High-resolution rotation curves of low surface brightness galaxies: Mass Models.  \emph{The Astronomical Journal}, 122, 2396-2427.
}
\keyword{datasets}
