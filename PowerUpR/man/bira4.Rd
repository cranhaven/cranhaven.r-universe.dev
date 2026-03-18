\name{bira4}
\alias{bira4}
\alias{bira4r1}
\alias{mdes.bira4}
\alias{power.bira4}
\alias{mrss.bira4}
\alias{mdes.bira4r1}
\alias{power.bira4r1}
\alias{mrss.bira4r1}
\title{Four-Level Blocked Individual-level Random Assignment Design}

\description{
For four-level randomized block designs (treatment at level 1, random effects across level 2, 3 and 4), use \code{mdes.bira4()} to calculate the minimum detectable effect size, \code{power.bira4()} to calculate the statistical power, and \code{mrss.bira4r1()} to calculate the minimum required sample size.
}

\usage{
mdes.bira4(power=.80, alpha=.05, two.tailed=TRUE,
           rho2, rho3, rho4, esv2=NULL, esv3=NULL, esv4=NULL,
           omega2=esv2/rho2, omega3=esv3/rho3, omega4=esv4/rho4,
           p=.50, r21=0, r2t2=0, r2t3=0, r2t4=0, g4=0,
           n, J, K, L)

power.bira4(es=.25, alpha=.05, two.tailed=TRUE,
            rho2, rho3, rho4, esv2=NULL, esv3=NULL, esv4=NULL,
            omega2=esv2/rho2, omega3=esv3/rho3, omega4=esv4/rho4,
            p=.50, r21=0, r2t2=0, r2t3=0, r2t4=0, g4=0,
            n, J, K, L)

mrss.bira4(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
           n, J, K, L0=10, tol=.10,
           rho2, rho3, rho4, esv2=NULL, esv3=NULL, esv4=NULL,
           omega2=esv2/rho2, omega3=esv3/rho3, omega4=esv4/rho4,
           p=.50, r21=0, r2t2=0, r2t3=0, r2t4=0, g4=0)
}

\arguments{
  \item{power}{statistical power \eqn{(1-\beta)}.}
  \item{es}{effect size.}
  \item{alpha}{probability of type I error.}
  \item{two.tailed}{logical; \code{TRUE} for two-tailed hypothesis testing, \code{FALSE} for one-tailed hypothesis testing. }
  \item{rho2}{proportion of variance in the outcome between level 2 units (unconditional ICC2).}
  \item{rho3}{proportion of variance in the outcome between level 3 units (unconditional ICC3).}
  \item{rho4}{proportion of variance in the outcome between level 4 units (unconditional ICC4).}
  \item{esv2}{effect size variability as the ratio of the treatment effect variance between level 2 units to the total variance in the outcome (level 1 + level 2 + level 3 + level 4). Ignored when \code{omega2} is specified.}
  \item{esv3}{effect size variability as the ratio of the treatment effect variance between level 3 units to the total variance in the outcome (level 1 + level 2 + level 3 + level 4). Ignored when \code{omega3} is specified.}
  \item{esv4}{effect size variability as the ratio of the treatment effect variance between level 4 units to the total variance in the outcome (level 1 + level 2 + level 3 + level 4). Ignored when \code{omega4} is specified.}
  \item{omega2}{treatment effect heterogeneity as ratio of treatment effect variance among level 2 units to the residual variance at level 2.}
  \item{omega3}{treatment effect heterogeneity as ratio of treatment effect variance among level 3 units to the residual variance at level 3.}
  \item{omega4}{treatment effect heterogeneity as ratio of treatment effect variance among level 4 units to the residual variance at level 4.}
  \item{p}{average proportion of level 1 units randomly assigned to treatment within level 2 units.}
  \item{g4}{number of covariates at level 4.}
  \item{r21}{proportion of level 1 variance in the outcome explained by level 1 covariates.}
  \item{r2t2}{proportion of treatment effect variance among level 2 units explained by level 2 covariates.}
  \item{r2t3}{proportion of treatment effect variance among level 3 units explained by level 3 covariates.}
  \item{r2t4}{proportion of treatment effect variance among level 4 units explained by level 4 covariates.}
  \item{n}{harmonic mean of level 1 units across level 2 units (or simple average).}
  \item{J}{harmonic mean of level 2 units across level 3 units (or simple average).}
  \item{K}{harmonic mean of level 3 units across level 4 units (or simple average).}
  \item{L}{number of level 4 units.}
  \item{L0}{starting value for \code{L}.}
  \item{tol}{tolerance to end iterative process for finding \code{L}.}
}

\value{
  \item{fun}{function name.}
  \item{parms}{list of parameters used in power calculation.}
  \item{df}{degrees of freedom.}
  \item{ncp}{noncentrality parameter.}
  \item{power}{statistical power \eqn{(1-\beta)}.}
  \item{mdes}{minimum detectable effect size.}
  \item{L}{number of level 4 units.}
}

\examples{
# cross-checks
mdes.bira4(rho4=.05, rho3=.15, rho2=.15,
           omega4=.50, omega3=.50, omega2=.50,
           n=10, J=4, K=4, L=27)
power.bira4(es = 0.142, rho4=.05, rho3=.15, rho2=.15,
            omega4=.50, omega3=.50, omega2=.50,
            n=10, J=4, K=4, L=27)
mrss.bira4(es = 0.142, rho4=.05, rho3=.15, rho2=.15,
           omega4=.50, omega3=.50, omega2=.50,
           n=10, J=4, K=4)
}
