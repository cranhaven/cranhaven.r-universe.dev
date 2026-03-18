\name{bcra4r3}
\alias{bcra4r3}
\alias{mdes.bcra4r3}
\alias{power.bcra4r3}
\alias{mrss.bcra4r3}
\title{Four-Level Blocked Cluster-level Random Assignment Design, Treatment at Level 3}

\description{
For four-level cluster-randomized block designs (treatment at level 3, with random effects across level 4 blocks), use \code{mdes.bcra4r3()} to calculate the minimum detectable effect size, \code{power.bcra4r3()} to calculate the statistical power, and \code{mrss.bcra4r3()} to calculate the minimum required sample size.
}

\usage{
mdes.bcra4r3(power=.80, alpha=.05, two.tailed=TRUE,
             rho2, rho3, rho4, esv4=NULL, omega4=esv4/rho4,
             p=.50, r21=0, r22=0, r23=0, r2t4=0, g4=0,
             n, J, K, L)

power.bcra4r3(es=.25, alpha=.05, two.tailed=TRUE,
              rho2, rho3, rho4, esv4=NULL, omega4=esv4/rho4,
              p=.50, r21=0, r22=0, r23=0, r2t4=0, g4=0,
              n, J, K, L)

mrss.bcra4r3(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
             n, J, K, L0=10, tol=.10,
             rho2, rho3, rho4, esv4=NULL, omega4=esv4/rho4,
             p=.50, r21=0, r22=0, r23=0, r2t4=0, g4=0)
}

\arguments{
  \item{power}{statistical power \eqn{(1-\beta)}.}
  \item{es}{effect size.}
  \item{alpha}{probability of type I error.}
  \item{two.tailed}{logical; \code{TRUE} for two-tailed hypothesis testing, \code{FALSE} for one-tailed hypothesis testing. }
  \item{rho2}{proportion of variance in the outcome between level 2 units (unconditional ICC2).}
  \item{rho3}{proportion of variance in the outcome between level 3 units (unconditional ICC3).}
  \item{rho4}{proportion of variance in the outcome between level 4 units (unconditional ICC4).}
  \item{esv4}{effect size variability as the ratio of the treatment effect variance between level 4 units to the total variance in the outcome (level 1 + level 2 + level 3 + level 4). \code{esv} also works. Ignored when \code{omega4} is specified.}
  \item{omega4}{treatment effect heterogeneity as ratio of treatment effect variance among level 4 units to the residual variance at level 4.}
  \item{p}{average proportion of level 3 units randomly assigned to treatment within level 4 units.}
  \item{g4}{number of covariates at level 4.}
  \item{r21}{proportion of level 1 variance in the outcome explained by level 1 covariates.}
  \item{r22}{proportion of level 2 variance in the outcome explained by level 2 covariates.}
  \item{r23}{proportion of level 3 variance in the outcome explained by level 3 covariates.}
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
mdes.bcra4r3(rho4=.05, rho3=.15, rho2=.15,
             omega4=.50, n=10, J=4, K=4, L=20)
power.bcra4r3(es = .316, rho4=.05, rho3=.15, rho2=.15,
              omega4=.50, n=10, J=4, K=4, L=20)
mrss.bcra4r3(es = .316, rho4=.05, rho3=.15, rho2=.15,
             omega4=.50, n=10, J=4, K=4)
}
