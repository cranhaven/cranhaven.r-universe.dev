\name{mleHK}

\alias{mleHK}

\title{
Maximum likelihood estimation for the HKp parameters.
}

\description{
The function mleHK is used to estimate the
\ifelse{html}{\out{&mu;}}{\eqn{\mu}{mu}},
\ifelse{html}{\out{&sigma;}}{\eqn{\sigma}{sigma}} and H parameters of the HKp.
The method for their estimation is described in eqs.8-9 (Tyralis and
Koutsoyiannis 2011).
}

\usage{
mleHK(data, interval = c(0.0001, 0.9999), tol = .Machine$double.eps^0.25)
}
\arguments{
  \item{data}{time series data}
  \item{interval}{H interval estimation}
  \item{tol}{estimation error tolerance}
}

\value{
Vector of maximum likelihood estimates of
\ifelse{html}{\out{&mu;}}{\eqn{\mu}{mu}},
\ifelse{html}{\out{&sigma;}}{\eqn{\sigma}{sigma}} and H.
}

\note{The function likelihoodfunction.c is called from the C library of the
package. Ideas for creating this function came from McLeod et al. (2007).}

\author{Hristos Tyralis}

\references{
McLeod AI, Yu H, Krougly ZL (2007) Algorithms for linear time series
analysis: With R package.
\emph{Journal of Statistical Software} \bold{23(5)}:1--26.
\doi{10.18637/jss.v023.i05}.

Tyralis H, Koutsoyiannis D (2011) Simultaneous estimation of the parameters
of the Hurst-Kolmogorov stochastic process.
\emph{Stochastic Environmental Research & Risk Assessment} \bold{25(1)}:21--33.
\doi{10.1007/s00477-010-0408-x}.
}

\examples{
# Estimate the parameters for the Nile time series.

mleHK(Nile)
}

\keyword{ts}