\name{senmscores}
\alias{senmscores}

\title{
  Computes M-scores for M-tests.
}
\description{
Computes M-scores for an M-test with one outcome in 1-to-k matched sets, for fixed k>=1.  For the one-sample problem or matched pairs, Maritz (1979) proposed robust tests and confidence intervals based on Huber's (1981) M-estimates.  These tests are extended to matching with several controls in Rosenbaum (2007).
}
\usage{
senmscores(y, z, mset, inner = 0, trim = 3, lambda = 1/2)
}

\arguments{
  \item{y}{
    A vector of length N for one outcome.
}
  \item{z}{
    A vector whose N coordinates are 1 for treated, 0 for control.
}
  \item{mset}{
    A vector of length N giving the matched set.
}
  \item{inner}{
    See trim.
}
  \item{trim}{
    The two values, inner and trim, define the M-statistic's psi-function. The psi-function is an odd function, psi(y) = -psi(-y), so it suffices to define it for nonnegative y.  For nonnegative y, psi(y) equals 0 for y between 0 and inner, rises linearly from 0 to 1 for y between inner and trim, and equals 1 for y greater than trim.  There are two requirements: inner must be nonnegative, and trim must be larger than inner.
}
  \item{lambda}{
    A number strictly between 0 and 1.  The M-scores are psi(y/s) where s is the lambda quantile of the within-set absolute pair differences.
}
}
\details{
The choice of psi-function to increase insensitivity to unmeasured bias is discussed in Rosenbaum (2013), where the parameter inner is proposed.
}
\value{
    A vector of length N containing the M-scores.
}
\references{
Huber, P. (1981). Robust Statistics.  NY: Wiley.

Maritz, J. S. (1979). A note on exact robust condence intervals for location. Biometrika 66, 163-170.

Rosenbaum, P. R. (2007) Sensitivity analysis for m-estimates, tests and confidence intervals in matched observational studies. Biometrics, 2007, 63, 456-464. <doi:10.1111/j.1541-0420.2006.00717.x>

Rosenbaum, P. R. (2013). Impact of multiple matched controls on design sensitivity in observational studies. Biometrics 69 118-127. (Introduces inner trimming.) <doi:10.1111/j.1541-0420.2012.01821.x>

Rosenbaum, P. R. (2015). Two R packages for sensitivity analysis in observational studies. Observational Studies, v. 1. (Free on-line.)
}
\author{
Paul R. Rosenbaum
}
\note{
The function is essentially a wrapper for the mscoresv function in the sensitivitymult package.  It is easier to use senmscores when using the informedSen package.
}




\examples{
data(HDL)
shdl<-senmscores(HDL$hdl,HDL$z,HDL$mset)
plot(HDL$hdl,shdl)
}
\keyword{htest}
