\name{bira3}
\alias{bira3}
\alias{bira3r1}
\alias{mdes.bira3}
\alias{power.bira3}
\alias{mrss.bira3}
\alias{mdes.bira3r1}
\alias{power.bira3r1}
\alias{mrss.bira3r1}
\title{Three-Level Blocked Individual-level Random Assignment Design}

\description{
For three-level randomized block designs (treatment at level 1, with random effects across level 2 and 3 blocks), use \code{mdes.bira3()} to calculate the minimum detectable effect size, \code{power.bira3()} to calculate the statistical power, and \code{mrss.bira3()} to calculate the minimum required sample size.
}

\usage{
mdes.bira3(power=.80, alpha=.05, two.tailed=TRUE,
           rho2, rho3, esv2=NULL, esv3=NULL,
           omega2=esv2/rho2, omega3=esv3/rho3,
           p=.50, r21=0, r2t2=0, r2t3=0, g3=0,
           n, J, K)

power.bira3(es=.25, alpha=.05, two.tailed=TRUE,
            rho2, rho3, esv2=NULL, esv3=NULL,
            omega2=esv2/rho2, omega3=esv3/rho3,
            p=.50, r21=0, r2t2=0, r2t3=0, g3=0,
            n, J, K)

mrss.bira3(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
           n, J, K0=10, tol=.10,
           rho2, rho3, esv2=NULL, esv3=NULL,
           omega2=esv2/rho2, omega3=esv3/rho3,
           p=.50, r21=0, r2t2=0, r2t3=0, g3=0)
}

\arguments{
  \item{power}{statistical power \eqn{(1-\beta)}.}
  \item{es}{effect size.}
  \item{alpha}{probability of type I error.}
  \item{two.tailed}{logical; \code{TRUE} for two-tailed hypothesis testing, \code{FALSE} for one-tailed hypothesis testing. }
  \item{rho2}{proportion of variance in the outcome between level 2 units (unconditional ICC2).}
  \item{rho3}{proportion of variance in the outcome between level 3 units (unconditional ICC3).}
  \item{esv2}{effect size variability as the ratio of the treatment effect variance between level 2 units to the total variance in the outcome (level 1 + level 2 + level 3). Ignored when \code{omega2} is specified.}
  \item{esv3}{effect size variability as the ratio of the treatment effect variance between level 3 units to the total variance in the outcome (level 1 + level 2 + level 3). Ignored when \code{omega3} is specified.}
  \item{omega2}{treatment effect heterogeneity as ratio of treatment effect variance among level 2 units to the residual variance at level 2.}
  \item{omega3}{treatment effect heterogeneity as ratio of treatment effect variance among level 3 units to the residual variance at level 3.}
  \item{p}{average proportion of level 1 units randomly assigned to treatment within level 2 units.}
  \item{g3}{number of covariates at level 3.}
  \item{r21}{proportion of level 1 variance in the outcome explained by level 1 covariates.}
  \item{r2t2}{proportion of treatment effect variance among level 2 units explained by level 2 covariates.}
  \item{r2t3}{proportion of treatment effect variance among level 3 units explained by level 3 covariates.}
  \item{n}{harmonic mean of level 1 units across level 2 units (or simple average).}
  \item{J}{harmonic mean of level 2 units across level 3 units (or simple average).}
  \item{K}{number of level 3 units.}
  \item{K0}{starting value for \code{K}.}
  \item{tol}{tolerance to end iterative process for finding \code{K}.}
}

\value{
  \item{fun}{function name.}
  \item{parms}{list of parameters used in power calculation.}
  \item{df}{degrees of freedom.}
  \item{ncp}{noncentrality parameter.}
  \item{power}{statistical power \eqn{(1-\beta)}.}
  \item{mdes}{minimum detectable effect size.}
  \item{K}{number of level 3 units.}
}


\examples{
# cross-checks
mdes.bira3(rho3=.20, rho2=.15,
           omega3=.10, omega2=.10,
           n=69, J=10, K=100)
power.bira3(es = .045, rho3=.20, rho2=.15,
            omega3=.10, omega2=.10,
            n=69, J=10, K=100)
mrss.bira3(es = .045, rho3=.20, rho2=.15,
           omega3=.10, omega2=.10,
           n=69, J=10)
}
