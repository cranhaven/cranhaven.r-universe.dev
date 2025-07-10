\name{tensor}
\alias{tensor}
\docType{data}
\title{
 Sea Surface Temperatures
}
\description{
 A 5-by-6 grid of sea-surface temperatures from 5
degrees N, 180 degrees W to 5 degrees S, 110 degrees W recorded hourly from 7:00PM on 4/26/94 to 3:00AM on 7/19/94, yielding 2000 epochs.
}
\usage{data("tensor")}
\format{
  The format is:
 num [1:5, 1:2000, 1:6] 28.1 28.4 28.1 28.6 29.3 ...
}

\references{
Mark Rogers, Lei Li, and Stuart J Russell, "Multilinear dynamical systems for tensor time series," in Advances in Neural Information Processing Systems (NIPS), 2013, pp. 2634–2642.
}
\examples{
data(tensor)
}
\keyword{datasets}
