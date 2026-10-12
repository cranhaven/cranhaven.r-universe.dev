\name{gmcpic.test}
\alias{gmcpic.test}
\title{Function implementing the Generalized Monte Carlo plug-in test with calibration (GMCPIC test)}
\usage{
gmcpic.test(x, B, M, weights, threshold)
}
\arguments{
  \item{x}{[2-column matrix] Column 1 (resp. 2) contains
  the vector of observed frequencies in population 1 (resp.
  2).}

  \item{B}{[Integer] Number of Monte Carlo simulations.}

  \item{M}{[Integer] Number of repetitions for the
  calibration.}

  \item{weights}{[Numeric] Vector of weights in [0,1] that
  are tried for the calibration.}

  \item{threshold}{[Numeric] Targeted risk level of the
  test; value in [0,1].}
}
\value{
list with INPUT arguments (\code{x}, \code{B}, \code{M},
\code{weights} and \code{threshold}) and the following
items: \item{calibrated.weight}{Weight selected by the
calibration procedure.} \item{p.value}{Test p-value.}
\item{reject.null.hypothesis}{Logical indicating whether
the null hypothesis is rejected or not at the risk level
specified by \code{threshold}.} \item{Message}{Details
about the p-value interpretation.}
}
\description{
The GMCPIC test is a procedure to test the equality of the
vectors of probabilities of two multinomial draws. The test
statistics that is used is the multinomial-density
statistic.
}
\details{
The GMCPIC test was developed to test the similarity of two
pathogen compositions based on small samples and sparse
data.
}
\examples{
## Load Pathogen Compositions of M. oryzae collected in Madagascar
data(PathogenCompositionMoryzaeMadagascar)
x=t(PathogenCompositionMoryzaeMadagascar)

## Apply the GMCPIC test (use B=10^3, M=10^4 to get a robust result)
\donttest{testMada=gmcpic.test(x, B=10^2, M=10^3, weights=seq(0.5,0.99,by=0.01),threshold=0.05)}
\donttest{testMada}

## Apply the Chi-squared test
chisq.test(x, simulate.p.value = TRUE, B = 10000)
}
\author{
Samuel Soubeyrand <samuel.soubeyrand@inra.fr>

Vincent Garreta

Maintainer: Jean-Francois Rey
}
\references{
  Soubeyrand S, Garreta V, Monteil C, Suffert F, Goyeau H,
Berder J, Moinard J, Fournier E, Tharreau D,
Morris C, Sache I (2017). Testing differences between
pathogen compositions with small samples and sparse data.
Phytopathology 107: 1199-1208. http://doi.org/10.1094/PHYTO-02-17-0070-FI
}
\keyword{misc}

