\name{pairwise.Eff}
\alias{pairwise.Eff}
\title{Pairwise table of effects with magnitudes}

\description{Pairwise table of effects with magnitudes}

\usage{pairwise.Eff(vec, fac, eff="K", dec=2, mad=FALSE)}

\arguments{
\item{vec}{Values}
\item{fac}{Groups}
\item{eff}{Effect, either 'K' or 'cohen.d', or 'cliff.delta'}
\item{dec}{Decimals to round}
\item{mad}{Use MAD-based nonparametric modification of K?}
}

\details{
Pairwise table of effect sizes.

At the moment, classic Lyubischev's K (a.k.a. SSSMD),
effsize::cliff.delta() and effsize::cohen.d() are supported.
}

\value{List with test outputs.}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
pairwise.Eff(hwc$WEIGHT, hwc$COLOR)
pairwise.Eff(hwc$WEIGHT, hwc$COLOR, mad=TRUE)
pairwise.Eff(hwc$WEIGHT, hwc$COLOR, eff="cohen.d")
pairwise.Eff(hwc$WEIGHT, hwc$COLOR, eff="cliff.delta")
}

\keyword{htest}
