\name{criterion}
\alias{criterion}
\title{
  BH-FDR and Romano Criterion
}
\description{
  Compute BH-FDR step up criterion, or Romano step-down criterion
}
\usage{
  criterion(alpha, delta, N.tests, FDP.control.method = c("BHFDR", "Romano"))
}
\arguments{
  \item{alpha}{
    The false discovery rate (in the BH case) or the upper bound on the
    probability that the FDP exceeds lambda (Romano case)
  }
  \item{delta}{
    If the "FDP.control.method" is set to 'Romano' then the user can set the 
    exceedance thresh-hold for the FDP tail probability control
    \eqn{P\{ FDP > \delta \} < \alpha}. The default value is \eqn{\alpha}.
  }
  \item{N.tests}{
    The number of simultaneous hypothesis tests. 
  }
  \item{FDP.control.method}{
    A character string specifying how the false discovery proportion (FDP) is to be
    controlled. You may specify the whole word or any shortened uniquely
    identifying truncation. \cr
     "BHFDR": the usual BH-FDR \cr
    "Romano": use Romano's method which guarantees probability less than 
              alpha that the FDP exceeds alpha. 
  }
}
\value{
  The step down or step up criterion, which is a vector of length N.tests
}
\references{
  Benjamini Y, Hochberg Y. Controlling the false discovery rate - a practical
  and powerful approach to multiple testing. J. R. Stat. Soc. Ser. B
  Stat. Methodol. 1995; 57(1):289-300.

  Romano J.P. and Shaikh A.M. On stepdown control of the false discovery
  proportion. IMS Lecture Notes--Monograph Series. 2006; 49:33-50.
  DOI: 10.1214/074921706000000383.
}
\author{
  Grant Izmirlian <izmirlian at nih dot gov>
}
\examples{
  library(pwrFDR)

  crit.b <- criterion(N.tests=1000, alpha=0.15, FDP.control.method="BHFDR")
  crit.r <- criterion(N.tests=1000, alpha=0.15, FDP.control.method="Romano")
  crit.r.17 <- criterion(N.tests=1000, alpha=0.15, delta=0.17, FDP.control.method="Romano")
  matplot(1:1000, cbind(crit.b, crit.r, crit.r.17), type="l", lty=1, col=2:4)
}
\keyword{htest}% use one of  RShowDoc("KEYWORDS")
\keyword{multiple testing}% __ONLY ONE__ keyword per line
\keyword{Benjamini-Hochberg}
\keyword{Romano}
