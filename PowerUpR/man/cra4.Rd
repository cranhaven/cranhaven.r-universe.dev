\name{cra4}
\alias{cra4}
\alias{cra4r4}
\alias{mdes.cra4}
\alias{power.cra4}
\alias{mrss.cra4}
\alias{mdes.cra4r4}
\alias{power.cra4r4}
\alias{mrss.cra4r4}
\title{Four-Level Cluster-randomized Trial}

\description{
For main treatment effects, use \code{mdes.cra4()} calculate the minimum detectable effect size, \code{power.cra4()} to calculate the statistical power, and \code{mrss.cra4()} to calculate the minimum required sample size.
}

\usage{
mdes.cra4(power=.80, alpha=.05, two.tailed=TRUE,
          rho2, rho3, rho4, p=.50, r21=0, r22=0, r23=0, r24=0, g4=0,
          n, J, K, L)

power.cra4(es=.25, alpha=.05, two.tailed=TRUE,
           rho2, rho3, rho4, p=.50, r21=0, r22=0, r23=0, r24=0, g4=0,
           n, J, K, L)

mrss.cra4(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
          n, J, K, L0=10, tol=.10,
          rho2, rho3, rho4, p=.50,
          r21=0, r22=0, r23=0, r24=0, g4=0)
}

\arguments{
  \item{power}{statistical power \eqn{(1-\beta)}.}
  \item{es}{effect size.}
  \item{alpha}{probability of type I error.}
  \item{two.tailed}{logical; \code{TRUE} for two-tailed hypothesis testing, \code{FALSE} for one-tailed hypothesis testing. }
  \item{rho2}{proportion of variance in the outcome between level 2 units (unconditional ICC2).}
  \item{rho3}{proportion of variance in the outcome between level 3 units (unconditional ICC3).}
  \item{rho4}{proportion of variance in the outcome between level 4 units (unconditional ICC4).}
  \item{p}{proportion of level 4 units randomly assigned to treatment.}
  \item{g4}{number of covariates at level 4.}
  \item{r21}{proportion of level 1 variance in the outcome explained by level 1 covariates.}
  \item{r22}{proportion of level 2 variance in the outcome explained by level 2 covariates.}
  \item{r23}{proportion of level 3 variance in the outcome explained by level 3 covariates.}
  \item{r24}{proportion of level 4 variance in the outcome explained by level 4 covariates.}
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
mdes.cra4(rho4=.05, rho3=.05, rho2=.10,
          n=10, J=2, K=3, L=20)
power.cra4(es = .412, rho4=.05, rho3=.05, rho2=.10,
           n=10, J=2, K=3, L=20)
mrss.cra4(es = .412, rho4=.05, rho3=.05, rho2=.10,
          n=10, J=2, K=3)
}
