\name{MannKendallLTP}

\alias{MannKendallLTP}

\title{
Mann-Kendall trend test under the scaling hypothesis.
}

\description{
The function MannKendallLTP applies the Mann-Kendall test under the scaling
hypothesis for the data (Hamed 2008).
}

\usage{
MannKendallLTP(data)
}

\arguments{
  \item{data}{time series data}
}

\value{
A list with three components.
  \item{Mann_Kendall}{Kendall's tau statistic, score, variance of score, Sen's
  slope, denominator D where tau=S/D and p-value for the Mann-Kendall test}
  \item{Significance_of_H}{H estimate (eq.21, Hamed 2008) of the modified
  variables and p-value}
  \item{Mann_Kendall_LTP}{Variance of score (p.356, Hamed 2008) and p-value for
  the Mann-Kendall test under the scaling hypothesis}
}

\note{The functions score.c, score0.c and VstarSfunction.c are called from the C
library of the package. The estimator of H for the stochastic process in eq(18)
(Hamed 2008) is the ML estimator in Tyralis and Koutsoyiannis (2011). The
denominator for the Mann-Kendall test is calculated according to eq(23.3.4) in
Hipel and McLeod (1994). The Mann-Kendall and modified Mann-Kendall test's
hypotheses are Ho: no trend vs H1: trend is present. The H test's hypotheses are
H0: H is not significant vs H1: H is significant.}

\author{Hristos Tyralis}

\references{
Hamed KH (2008) Trend detection in hydrologic data: The Mann-Kendall trend
test under the scaling hypothesis. \emph{Journal of Hydrology}
\bold{349(3--4)}:350--363. \doi{10.1016/j.jhydrol.2007.11.009}.

Hipel KW, McLeod AI (1994) \emph{Time series modelling of water resources and
environmental systems}. Amsterdam: Elsevier.

Tyralis H, Koutsoyiannis D (2011) Simultaneous estimation of the parameters
of the Hurst-Kolmogorov stochastic process. \emph{Stochastic Environmental
Research & Risk Assessment} \bold{25(1)}:21--33.
\doi{10.1007/s00477-010-0408-x}.
}

\examples{
# Modified Mann-Kendall test for the Nile time series.

MannKendallLTP(Nile)
}

\keyword{htest}

\keyword{ts}