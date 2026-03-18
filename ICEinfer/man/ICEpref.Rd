\name{ICEpref}
\alias{ICEpref}
\title{Compute ICE Economic Preference Scores from Numeric Cost and Effe Measures}
\description{
  This function calculates Incremental Net Monetary Benefit (INMB) Scores that are expressed in Cost
  units. While these scores are of "linear" form, (lambda * Effe) - Cost, when eta == gamma / beta == 1,
  they are "Nonlinear" when eta is either > 1 or < 1.
}
\usage{
  ICEpref(tr, ex, cy, lambda = 1, beta = 1, eta = 3 + 2 * sqrt(2)) 
}
\arguments{
  \item{tr}{Binary [0 or 1] indicator of Treatment Regimen received by each patient [Experimental
   Unit]: tr == 0 denotes the "Standard" Regimen, tr == 1 denotes a "New" Regimen.}
  \item{ex}{Numeric vector representing Effe measures observed for 2 or more experimental units.}
  \item{cy}{Corresponding numeric vector reporting Total Cost Incurred by patients during treatment.
   The tr, ex and cy vectors must be of the same length, n >= 2, and 1 < sum(tr) < n.}
  \item{lambda}{Positive value for the fair, full-retail Shadow Price of Health. When ex is
   measured in QALYs and cy in U.S. Dollars, lambda = 50000 is traditionally assumed appropriate.}
  \item{beta}{Positive Returns-to-Scale Power parameter for ICE Preference calculations. beta = 1
   implies Linear (Constant) Returns-to-Scale.  A beta > 0 but < 1 implies Returns-to-Scale that
   are Diminishing and tend to be highly Realistic.  Any beta > 1 implies Increasing Returns-to-Scale.}
  \item{eta}{Positive Power Parameter Ratio: gamma / beta.  Generalized "linear" maps result when
   eta = 1.  An eta ratio > 1 can yield a realistic "Nonlinear" map, but maps with eta > 3+2*sqrt(2)
   = 5.828 (Obenchain's "Omega") violate the "Cartesian Monotonicity" Axiom.}
}
\value{
  Object of class "numeric" of the same length as the tr, ex and cy vectors input:
  \item{pref}{Computed ICE Economic Preference Scores.}
}
\references{
 Hoch JS, Briggs AH, Willan AR. Something old, something new, something borrowed,
 something blue: a framework for the marriage of health econometrics and
 cost-effectiveness analysis. \emph{Health Economics} 2002; \bold{11}: 415-430.
 
 Obenchain RL.  ICE Preference Maps: Nonlinear Generalizations of Net Benefit and Acceptability.
 \emph{Health Serv Outcomes Res Method}  2008; \bold{8}: 31-56. DOI 10.1007/s10742-007-0027-2.
 Open Access.
}
\author{Bob Obenchain <wizbob@att.net>}
\seealso{\code{\link{plot.ICEepmap}} and \code{\link{print.ICEepmap}}}
\examples{
  data(sepsis)
  attach(sepsis)
  icedr <- ICEpref(tr=icu, ex=qalypres, cy=totcost, lambda=50000, beta=0.6) # Diminishing Returns.
  summary(icedr)
}
\keyword{methods} 
