\name{lssd}

\alias{lssd}

\title{
LSSD estimation for the HKp parameters.
}

\description{
The function lssd is used to estimate the
\ifelse{html}{\out{&sigma;}}{\eqn{\sigma}{sigma}} and H parameters of the HKp,
using the LSSD (Least Squares based on Standard Deviation) method as described
in Koutsoyiannis (2003) and Tyralis and Koutsoyiannis (2011, Section 2.3).
}

\usage{
lssd(data, k1, p = 2, q = 0, interval = c(0.001, 0.999))
}

\arguments{
  \item{data}{time series data}
  \item{k1}{maximum aggregation scale}
  \item{p}{Parameter used to determine the weights}
  \item{q}{Parameter used to determine the penalty factor}
  \item{interval}{H interval estimation}
}

\value{
Vector of LSSD estimates of \ifelse{html}{\out{&sigma;}}{\eqn{\sigma}{sigma}}
and H.
}

\author{Hristos Tyralis}

\references{
Koutsoyiannis D (2003) Climate change, the Hurst phenomenon, and hydrological
statistics. \emph{Hydrological Sciences Journal} \bold{48(1)}:3--24.
\doi{10.1623/hysj.48.1.3.43481}.

Tyralis H, Koutsoyiannis D (2011) Simultaneous estimation of the parameters
of the Hurst-Kolmogorov stochastic process.
\emph{Stochastic Environmental Research & Risk Assessment} \bold{25(1)}:21--33.
\doi{10.1007/s00477-010-0408-x}.
}

\examples{
# Estimate the parameters for the Nile time series.

lssd(data = Nile, k1 = floor(length(Nile)/10), p = 2)
}

\keyword{ts}