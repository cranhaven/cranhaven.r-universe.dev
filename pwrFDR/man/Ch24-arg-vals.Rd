\name{arg.vals}
\alias{arg.vals}
\title{
  Extracts the full argument list and call attribute.
}
\description{
  Extracts the full argument list and call attribute from
  an object of class \code{pwr}, which is the result of a
  call to \code{pwrFDR}.
}
\usage{
  arg.vals(object)
}
\arguments{
  \item{object}{
  An object of class ‘pwr’, which is the result of a call to \code{pwrFDR}
  }
}
\value{
  A list with a \code{call} component and one component for each of the
  possible arguments, \code{effect.size}, \code{n.sample},\code{r.1},
  \code{alpha}, \code{N.tests}, \code{lambda}, \code{FDP.control.method},
  \code{delta}, \code{groups}, \code{type}, \code{grpj.per.grp1},
  \code{method} and \code{control}, with defaults filled in. 
}
\references{
  Izmirlian G. (2020) Strong consistency and asymptotic normality for
    quantities related to the Benjamini-Hochberg false discovery rate
    procedure. Statistics and Probability Letters; 108713,
    <doi:10.1016/j.spl.2020.108713>.

  Izmirlian G. (2017) Average Power and \eqn{\lambda}-power in
    Multiple Testing Scenarios when the Benjamini-Hochberg False
    Discovery Rate Procedure is Used. <arXiv:1801.03989>

  Jung S-H. (2005) Sample size for FDR-control in microarray data
    analysis. Bioinformatics; 21:3097-3104.
  
  Liu P. and Hwang J-T. G. (2007) Quick calculation for sample size while
    controlling false discovery rate with application to microarray
    analysis. Bioinformatics; 23:739-746.

  Lehmann E. L., Romano J. P.. Generalizations of the familywise error
    rate. Ann. Stat.. 2005;33(3):1138–1154.

  Romano Joseph P., Shaikh Azeem M.. Stepup procedures for control of
    generalizations of the familywise error rate. Ann. Stat..
    2006;34(4):1850-1873. 
}
\author{
  Grant Izmirlian <izmirlian at nih dot gov>
}
\examples{
   rslt <- pwrFDR(effect.size = 0.79, n.sample = 46, r.1 = 2000/54675, alpha = 0.15, 
                  N.tests = 1000, FDP.control.method = "Auto")

   arg.vals(rslt)
}
\keyword{FDR}
\keyword{Benjamini}
\keyword{Hochberg}
\keyword{microarrays}
\keyword{Multiple.Testing}
\keyword{average.power}
\keyword{k.power}
\keyword{lambda.power}
