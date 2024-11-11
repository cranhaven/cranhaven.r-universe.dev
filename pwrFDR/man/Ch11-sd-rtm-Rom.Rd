\name{sd.rtm.Rom}
\alias{sd.rtm.Rom}
\title{
  Extractor function for asymptotic sd[R_m/m] under selected FDP control method
}
\description{
  A function which extracts the asymptotic standard deviation for the
  postive call proportion, R_m/m, under the selected FDP control method
  from the supplied \code{pwr} object, which is the result of a call to
  the main function, \link{pwrFDR}.
}
\usage{
  sd.rtm.Rom(object)
}
\arguments{
  \item{object}{
    An object of class, \code{pwr}, which is the result of a call to
    the main function, \link{pwrFDR}
  }
}
\details{
  The siginificant call proportion (SCP), R_m/m, under the selected
  FDP control method, is directly related to the ensemble power, which
  in turn, is determined by the effect size for tests distributed under
  the alternative, the sample size, the proportion of tests which are
  distributed according to the alternative and the size, alpha, in the
  selected FDP control method. Its asymptotic standard error, e.g. the
  asymptotic standard deviation over the square root of the number of
  simultaneous tests, m, gives an indication of the range of values
  one can expect for the significant call proportion. The standard
  deviations of the ratios R_m/m, T_m/M_m, and V_m/R_m are used
  internally in control of the distribution of V_m/R_m for the BHCLT
  FDP control method, and in calculation of the tail probability power
  for T_m/M_m. 
}
\value{
  Returns the asymptotic standard deviation of the significant call
  proportion, sd[R_m/m], as an un-named numeric.
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
  \code{\link{sd.rtm.VoR}}
  \code{\link{sd.rtm.ToM}}
}
\examples{
  rslt.BHFDR  <- pwrFDR(effect.size=0.79, n.sample=46, r.1=0.05, alpha=0.15)
  rslt.Auto.1 <- pwrFDR(effect.size=0.79, n.sample=46, r.1=0.05, alpha=0.15, N.tests=51,
                       FDP.control.method="Auto")
  rslt.Auto.2 <- pwrFDR(effect.size=0.79, n.sample=46, r.1=0.05, alpha=0.15, N.tests=49,
                       FDP.control.method="Auto")

  ## Asymptotic standard deviation of positive call proportion under BHFDR
  sdrtmRomBHFDR  <- sd.rtm.Rom(rslt.BHFDR)

  ## Asymptotic standard deviation of positive call proportion under BHCLT
  sdrtmRomAuto1 <- sd.rtm.Rom(rslt.Auto.1)

  ## Asymptotic standard deviation of positive call proportionunder Romano
  sdrtmRomAuto2 <- sd.rtm.Rom(rslt.Auto.2)
}
\keyword{Benjamini-Hochberg}
\keyword{Multiple.Testing}
\keyword{k.power}
\keyword{lambda.power}
