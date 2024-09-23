\name{lsv}

\alias{lsv}

\title{
LSV estimation for the HKp parameters.
}

\description{
The function lsv is used to estimate the
\ifelse{html}{\out{&sigma;}}{\eqn{\sigma}{sigma}} and H parameters of the HKp,
using the LSV (Least Squares based on Variance) method as described in Tyralis
and Koutsoyiannis (2011, Section 2.4).
}

\usage{
lsv(data, k1, p = 6, q = 0, interval = c(0.001, 0.999))
}

\arguments{
  \item{data}{time series data}
  \item{k1}{maximum aggregation scale}
  \item{p}{Parameter used to determine the weights}
  \item{q}{Parameter used to determine the penalty factor}
  \item{interval}{H interval estimation}
}

\value{
Vector of LSV estimates of \ifelse{html}{\out{&sigma;}}{\eqn{\sigma}{sigma}} and
H.
}

\author{Hristos Tyralis}

\references{
Tyralis H, Koutsoyiannis D (2011) Simultaneous estimation of the parameters
of the Hurst-Kolmogorov stochastic process.
\emph{Stochastic Environmental Research & Risk Assessment} \bold{25(1)}:21--33.
\doi{10.1007/s00477-010-0408-x}.
}

\examples{
# Estimate the parameters for the Nile time series.

lsv(data = Nile, k1 = floor(length(Nile)/10), p = 6, q = 50)
}

\keyword{ts}