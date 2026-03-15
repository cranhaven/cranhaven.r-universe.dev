%  $Author: sinnwell $
%  $Date: 2009/04/27 21:21:57 $
%  $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/hapPower.demo.Rd,v 1.5 2009/04/27 21:21:57 sinnwell Exp $
%  $Locker:  $

\name{hapPower.demo}
\alias{hapPower.demo}
\docType{data}
\title{  Set of haplotypes and frequencies for power and sample size
  calculations }
\description{
  An example set of haplotypes and frequencies for power and sample size
  calculations in haplo.power.cc and haplo.power.qt
}
\usage{data(hapPower.demo)}
\format{
  A data frame with 21 observations on the following 6 variables.
  \describe{
    \item{\code{loc.1}}{allele 1 in the haplotype }
    \item{\code{loc.2}}{allele 2 in the haplotype }
    \item{\code{loc.3}}{allele 3 in the haplotype }
    \item{\code{loc.4}}{allele 4 in the haplotype }
    \item{\code{loc.5}}{allele 5 in the haplotype }
    \item{\code{haplo.freq}}{numeric, frequency of haplotype}
  }
}

\references{
 Schaid, DJ. Power and sample size for testing associations of
 haplotypes with complex traits. \emph{Ann Hum Genet} (2005) 70:116-130.
}
\examples{
data(hapPower.demo)
}
\keyword{datasets}
