\name{sd.rtm.VoR}
\alias{sd.rtm.VoR}
\title{
  Extractor function for asymptotic sd[V_m/R_m] under selected FDP control method
}
\description{
  A function which extracts the asymptotic standard deviation for the
  false discovery proportion, V_m/R_m, under the selected FDP control method
  from the supplied \code{pwr} object, which is the result of a call to
  the main function, \link{pwrFDR}.
}
\usage{
  sd.rtm.VoR(object)
}
\arguments{
  \item{object}{
    An object of class, \code{pwr}, which is the result of a call to
    the main function, \link{pwrFDR}
  }
}
\details{
  The false discovery proportion (FDP), V_m/R_m, under the selected
  FDP control method, is the proportion of null distributed test
  statistics that were deemed significant calls by the FDP control
  method. The most well known of available FDP methods is the
  Benjamini-Hochberg False Discovery Rate (BH-FDR) procedure. It
  ensures that the expected value of the FDP will be less than
  alpha, E[FDP] < alpha. The other two included FDP control methods,
  "Romano" and "BHCLT", control the probability that the FDP exceeds
  a given value, delta:

  \eqn{P( V_m/R_m > \delta ) < \alpha}

  In most cases, the choice \eqn{\delta=\alpha} is appropriate but
  \eqn{\delta} is a distinct parameter to allow greater flexibility.
  The choice "Auto" will select the most appropriate choice from the
  three, BHFDR, BHCLT and Romano. If the asymptotic standard error,
  sd.rtm.VoR/m^0.5 is greater than a control parameter (default value
  10\%), then one of the choices "BHCLT" or "Romano" will be made. As
  the "Romano" FDP control method is more conservative, there is a
  preference for the "BHCLT" method, which can be used if the number
  of simultaneous tests, m, is larger than 50. All of this is
  handled internally within the function \code{pwrFDR}. These
  extractor functions exist to allow the user 'under the hood'. 
}
\value{
  Returns the asymptotic standard deviation of the false discovery 
  proportion, sd[V_m/R_m], as an un-named numeric.
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
  \code{\link{sd.rtm.ToM}}
}
\examples{
  rslt.BHFDR  <- pwrFDR(effect.size=0.79, n.sample=46, r.1=0.05, alpha=0.15)
  rslt.Auto.1 <- pwrFDR(effect.size=0.79, n.sample=46, r.1=0.05, alpha=0.15, N.tests=51,
                       FDP.control.method="Auto")
  rslt.Auto.2 <- pwrFDR(effect.size=0.79, n.sample=46, r.1=0.05, alpha=0.15, N.tests=49,
                       FDP.control.method="Auto")

  ## Asymptotic standard deviation under BHFDR
  sdrtmVoRBHFDR  <- sd.rtm.VoR(rslt.BHFDR)

  ## Asymptotic standard deviation under BHCLT
  sdrtmVoRAuto1 <- sd.rtm.VoR(rslt.Auto.1)

  ## Asymptotic standard deviation under Romano
  sdrtmVoRAuto2 <- sd.rtm.VoR(rslt.Auto.2)
}
\keyword{Benjamini-Hochberg}
\keyword{Multiple.Testing}
\keyword{average.power}
\keyword{k.power}
\keyword{lambda.power}
