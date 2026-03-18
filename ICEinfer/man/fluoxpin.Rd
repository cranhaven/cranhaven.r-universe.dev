\name{fluoxpin}
\alias{fluoxpin}
\docType{data}
\title{Data from a double-blind clinical trial comparing fluoxetine plus pindolol with fluoxetine alone}
\description{These data are from a Spanish double-blind clinical trial in which 55 patients were
 randomized to fluoxetine (an SSRI) plus pindolol (a Beta Blocker) and 56 patients were randomized
 to fluoxetine plus placebo for treatment of major depressive disorder (MDD), Sacristan et al.
 (2000).
}
\usage{data(fluoxpin)}
\format{
  A data frame of 3 variables on 111 patients; no NAs.
  \describe{
    \item{respond}{Patients are considered to have responded to treatment when a 50\% or greater
     decrease in HAMD-17 total score occurred between baseline and end-point (at day 42), with
     no more than 10\% additional variation between intermediate visits.
     }
    \item{cost}{Resource utilization was prospectively collected alongside the clinical trial.
     Patients and caregivers were interviewed by the researcher concerning all resources consumed
     during the study period. Resources dictated by the protocol were not counted.  Costs are
     expressed in Pesetas (Pts.) at 1996 prices (1 Dollar = 145 Pts.)  Observed differences in average
     direct medical costs were mainly due to hospitalizations within the FlxPin = 0 group.   
     }
    \item{flxpin}{Treatment indicator variable. FlxPin = 1 implies receipt of fluoxetine 20 mg/day
     plus pindolol 7.5 mg/day (2.5 mg tid).  FlxPin = 0 implies receipt of fluoxetine 20 mg/day
     plus placebo (tid).
     }
  }
}
\details{
 Since both samples are rather small (55 and 56 patients) here and the Effectiveness variable,
 respond, is binary, this example illustrates how the Law of Large Numbers can fail to apply to
 ICE inferences.  Specifically, the bootstrap distribution of sample differences between AVERAGES
 appears to be quite different from bivariate normal in three ways: (i) The Bootstrap Distribution
 of ICE Uncertainty appears to consist of vertical stripes because the horizontal variable is
 discrete here while the vertical variable is continuous. (ii) The Bootstrap Distribution of cost
 differences appears to end somewhat abruptly near the horizontal axis at DeltaCost = 0, rather
 than have a long upwards tail like its downwards tail.  (iii)  The equal density contours of the
 bivariate Bootstrap Distribution appear to NOT be elliptical.  This third point can be
 dramatically illustrated by computing the Owen Empirical Likelihood contour that passes through
 the origin of the ICE plane.
}
\references{
  Hamilton M.  Development of a rating scale for primary depressive illness.  \emph{British
  Journal of Social and Clinical Psychology} 1967; \bold{6}: 278--296.
  
  Sacristan JA, Obenchain RL.  Reporting cost-effectiveness analyses with confidence.
  \emph{JAMA} 1997; \bold{277}: 375.
  
  Obenchain RL, Sacristan JA.  In reply to: The negative side of cost-effectiveness ratios.
  \emph{JAMA} 1997; \bold{277}: 1931--1933.
  
  Sacristan JA, Gilaberte I, Boto B, Buesching DP, Obenchain RL, Demitrack M, Perez Sola V,
  Alvarez E, and Artigas F. Cost-effectiveness of fluoxetine plus pindolol in patients with
  major depressive disorder: results from a randomized, double blind clinical trial. \emph{Int
  Clin Psychopharmacol} 2000; \bold{15}: 107--113.
  
  Owen AB.  \emph{Empirical Likelihood}  New York: Chapman and Hall/CRC.  2001.  
}
\examples{
    data(fluoxpin)
    ICEscale(fluoxpin, flxpin, respond, cost)
}
\keyword{datasets}
