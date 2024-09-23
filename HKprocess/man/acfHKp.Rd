\name{acfHKp}

\alias{acfHKp}

\title{Autocorrelation of HKp}

\description{
The Hurst-Kolmogorov stochastic process (HKp) is a persistent process. The term
HKp is an alternative of the term Fractional Gaussian Noise (FGN, see
Koutsoyiannis 2010). Its autocorrelation function (ACF) is given by eq.16
(Koutsoyiannis 2002).
}

\usage{acfHKp(H, maxlag)}

\arguments{
    \item{H}{Hurst parameter }
    \item{maxlag}{ACF computed at lags 0, 1, ..., maxlag}
}

\value{
Vector of autocorrelations at lags 0, 1, ..., maxlag.
}

\note{The parameter H should be in (0, 1), see Koutsoyiannis (2002)}

\author{Hristos Tyralis}

\references{
Koutsoyiannis D (2002) The Hurst phenomenon and fractional Gaussian noise made
easy. \emph{Hydrological Sciences Journal} \bold{47(4)}:573--595.
\doi{10.1080/02626660209492961}.

Koutsoyiannis D (2010) A random walk on water. \emph{Hydrology and Earth
System Sciences} \bold{14}:585--601.
\doi{10.5194/hess-14-585-2010}.
}

\examples{
# Compute the ACF at lags 0, 1, ..., 10 when H = 0.8.

acfHKp(0.8, 10)
}

\keyword{ts}