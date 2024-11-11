\name{sd.rtm.ToM}
\alias{sd.rtm.ToM}
\title{
  Extractor function for asymptotic sd[T_m/M_m] under selected FDP control method
}
\description{
  A function which extracts the asymptotic standard deviation for the
  true positive proportion, T_m/M_m, under the selected FDP control method
  from the supplied \code{pwr} object, which is the result of a call to
  the main function, \link{pwrFDR}.
}
\usage{
  sd.rtm.ToM(object)
}
\arguments{
  \item{object}{
    An object of class, \code{pwr}, which is the result of a call to
    the main function, \link{pwrFDR}
  }
}
\details{
  The true positive proportion (TPP), T_m/M_m, is the proportion
  of all test statistics distributed according to the alternative that
  are declared significant by the selected FDP control method. Whereas
  the ensemble type I error in the multiple testing experiment is
  handled via control of the distribution of the FDP, V_m/R_m, the
  ensemble power is optimized via the distribution of the TPP. The
  most commonly used ensemble power is based upon the expected TPP,
  or true postive rate, E[TPP], which also called the average power.
  In situations of just adequate power or near adequate power,
  especially when testing less than 1000 simultaneous tests or so, the
  distribution of the TPP will be non-negligiby dispersed and this
  means that the TPP in a given multiple testing experiment for which
  sample size was based on the average power will likely not be close
  to the promised average power. For this reason, it is preferable to
  use a concept of ensemble power which is based upon the excedance
  probability for the TPP, or tail probability of the TPP (tp-TPP).

  \eqn{P(TPP > \lambda) >= 1- \epsilon_{II}}
  
  This package uses asymptotic approximation to derive the tp-TPP
  ensemble power under any one of the avaialbe FDP control methods,
  BHFDR, BHCLT or Romano.
}
\value{
  Returns the asymptotic standard deviation of the true postive 
  proportion, sd[T_m/M_m], as an un-named numeric.
}
\references{
  Izmirlian G. (2020) Strong consistency and asymptotic normality for
    quantities related to the Benjamini-Hochberg false discovery rate
    procedure. Statistics and Probability Letters; 108713,
    <doi:10.1016/j.spl.2020.108713>.
    
  Izmirlian G. (2017) Average Power and \eqn{\lambda}-power in
    Multiple Testing Scenarios when the Benjamini-Hochberg False
    Discovery Rate Procedure is Used. <arXiv:1801.03989>
}
\author{
  Grant Izmirlian <izmirlig at mail dot nih dot gov>
}
\seealso{
  \code{\link{sd.rtm.Rom}}
  \code{\link{sd.rtm.VoR}}
}
\examples{
  rslt.BHFDR  <- pwrFDR(effect.size=0.79, n.sample=46, r.1=0.05, alpha=0.15)
  rslt.Auto.1 <- pwrFDR(effect.size=0.79, n.sample=46, r.1=0.05, alpha=0.15, N.tests=51,
                       FDP.control.method="Auto")
  rslt.Auto.2 <- pwrFDR(effect.size=0.79, n.sample=46, r.1=0.05, alpha=0.15, N.tests=49,
                       FDP.control.method="Auto")

  ## Asymptotic standard deviation under BHFDR
  sdrtmToMBHFDR  <- sd.rtm.ToM(rslt.BHFDR)

  ## Asymptotic standard deviation under BHCLT
  sdrtmToMAuto1 <- sd.rtm.ToM(rslt.Auto.1)

  ## Asymptotic standard deviation under Romano
  sdrtmToMAuto2 <- sd.rtm.ToM(rslt.Auto.2)
}
\keyword{Benjamini-Hochberg}
\keyword{Multiple.Testing}
\keyword{average.power}
\keyword{k.power}
\keyword{lambda.power}
