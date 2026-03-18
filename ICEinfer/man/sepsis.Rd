\name{sepsis}
\alias{sepsis}
\docType{data}
\title{Artificial data.frame of 7 characteristics of patients treated for septic shock}
\description{These simulated data describe the characteristics of 94 pseudo-patients treated
for sepsis by two alternative Intensive Care Units.}
\usage{data(sepsis)}
\format{
  A data frame of 7 characteristics of 94 sepsis patients; no NAs.
  \describe{
    \item{patid}{Unique patient I.D. number; 1 through 94.}
    \item{icu}{Intensive Care Unit type: 1 is type "new", and 0 is type "std".}
    \item{qalypres}{Quality Adjusted Life Years preserved by treatment for sepsis.}
    \item{totcost}{Total Cost of Resources Utilized in treatment for sepsis.} 
    \item{age}{Patient Age in years (integer).}
    \item{apache}{Apache II score (integer).}
    \item{orgfails}{Number of Organ Failures experienced (integer).}
    }
}
\details{
 Since only 47 patients from each of 2 Intensive Care Units are provided, this example illustrates
 how the Law of Large Numbers can fail to apply to ICE inferences. Unlike the 3 other "ICEinfer"
 example data.frames provided, the "sepsis" data.frame contains 3 additional patient charcteristics
 (confounders) for potential use in covariate-adjustment via linear models.
}
\references{
  Obenchain RL, Sacristan JA.  In reply to: The negative side of cost-effectiveness ratios.
  \emph{JAMA} 1997; \bold{277}: 1931--1933.
  
  Hoch JS, Briggs AH, Willan AR. Something old, something new, something borrowed,
  something blue: a framework for the marriage of health econometrics and
  cost effectiveness analysis. \emph{Health Economics} 2002; \bold{11}: 415-430.
}
\examples{
    data(sepsis)
    ICEscale(sepsis, icu, qalypres, totcost, lambda = 50000)
    \dontrun{ demo(sepsis) }
}
\keyword{datasets}
