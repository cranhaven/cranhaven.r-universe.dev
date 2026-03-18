\name{cra3}
\alias{cra3}
\alias{cra3r3}
\alias{bcra4f3}
\alias{mdes.cra3}
\alias{power.cra3}
\alias{mrss.cra3}
\alias{mdes.bcra4f3}
\alias{power.bcra4f3}
\alias{mrss.bcra4f3}
\alias{mdes.cra3r3}
\alias{mdes.mod331}
\alias{mdes.mod332}
\alias{mdes.mod333}
\alias{mdesd.mod331}
\alias{mdesd.mod332}
\alias{mdesd.mod333}
\alias{power.cra3r3}
\alias{power.mod331}
\alias{power.mod332}
\alias{power.mod333}
\alias{power.med331}
\alias{power.med321}
\alias{power.med311}
\alias{mrss.cra3r3}
\alias{mrss.mod331}
\alias{mrss.mod332}
\alias{mrss.mod333}
\alias{mdes.bcra4f3}
\alias{power.bcra4f3}
\alias{mrss.bcra4f3}

\title{Three-level Cluster-randomized Trials to Detect Main, Moderation, and Mediation Effects}

\description{
For main treatment effects, use \code{mdes.cra3()} to calculate the minimum detectable effect size, \code{power.cra3()} to calculate the statistical power, \code{mrss.cra3()} to calculate the minimum required sample size (number of clusters).

For moderator at level 1, use \code{mdesd.mod331()} to calculate the minimum detectable effect size, \code{power.mod331()} to calculate the statistical power, \code{mrss.mod331()} to calculate the minimum required sample size (number of clusters).

For moderator at level 2, use \code{mdesd.mod332()} to calculate the minimum detectable effect size, \code{power.mod332()} to calculate the statistical power, \code{mrss.mod332()} to calculate the minimum required sample size (number of clusters).

For moderator at level 3, use \code{mdesd.mod333()} to calculate the minimum detectable effect size, \code{power.mod333()} to calculate the statistical power, \code{mrss.mod333()} to calculate the minimum required sample size (number of clusters).

For mediator at level 3, use \code{power.med331()}, for mediator at level 2, use \code{power.med321()}, for mediator at level 1, use \code{power.med311()} to calculate the statistical power.

For cluster-randomized block designs (treatment at level 3, with fixed effects across level 4 blocks), use \code{mdes.bcra4f3()} to calculate the minimum detectable effect size, \code{power.bcra4f3()} to calculate the statistical power, and \code{mrss.bcra4f3()} to calculate the minimum required sample size (number of clusters per block).
}

\usage{
mdes.cra3(power=.80, alpha=.05, two.tailed=TRUE,
          rho2, rho3, p=.50, g3=0, r21=0, r22=0, r23=0,
          n, J, K)

mdesd.mod331(power=.80, alpha=.05, two.tailed=TRUE,
            rho2, rho3, omegam2, omegam3,
            r21=0, r2m3=0,
            p=.50, q=NULL, n, J, K)

mdesd.mod332(power=.80, alpha=.05, two.tailed=TRUE,
            rho2, rho3, omegam3, r21=0, r22=0, r2m3=0,
            p=.50, q=NULL, n, J, K)

mdesd.mod333(power=.80, alpha=.05, two.tailed=TRUE,
            rho2, rho3, r21=0, r22=0, r23=0,
            p=.50, q=NULL, n, J, K)

power.cra3(es=.25, alpha=.05, two.tailed=TRUE,
           rho2, rho3, g3=0, r21=0, r22=0, r23=0,
           p=.50, n, J, K)

power.mod331(es=.25, alpha=.05, two.tailed=TRUE,
             rho2, rho3, omegam2, omegam3,
             r21=0, r2m3=0,
             p=.50, q=NULL, n, J, K)

power.mod332(es=.25, alpha=.05, two.tailed=TRUE,
             rho2, rho3, omegam3, r21=0, r22=0, r2m3=0,
             p=.50, q=NULL, n, J, K)

power.mod333(es=.25, alpha=.05, two.tailed=TRUE,
             rho2, rho3, r21=0, r22=0, r23=0,
             p=.50, q=NULL, n, J, K)

power.med331(esa, esB, two.tailed=TRUE, alpha=.05,
             mc=TRUE, nsims=1000, ndraws=1000,
             rho2, rho3, gm3=4, r2m3=0, r21=0, r22=0,
             g3=5, r23=0, p=.50, n, J, K)

power.med321(esa, esB, two.tailed=TRUE, alpha=.05,
             mc=TRUE, nsims=1000, ndraws=1000,
             rhom3, rho2, rho3, r2m2=0,
             gm3=4, r2m3=0, r21=0, r22=0, g3=5, r23=0,
             p=.50, n, J, K)

power.med311(esa, esB, two.tailed=TRUE, alpha=.05,
             mc=TRUE, nsims=1000, ndraws=1000,
             rhom2, rhom3, rho2, rho3,
             r2m1=0, r2m2=0, gm3=4, r2m3=0,
             r21=0, r22=0, g3=5, r23=0,
             p=.50, n, J, K)

mrss.cra3(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
          n, J, K0=10, tol=.10,
          rho2, rho3, p=.50, g3=0, r21=0, r22=0, r23=0)

mrss.mod331(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
            rho2, rho3, omegam2, omegam3,
            r21=0, r2m3=0,
            p=.50, q=NULL, n, J, K0=10, tol=.10)

mrss.mod332(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
            rho2, rho3, omegam3, r21=0, r22=0, r2m3=0,
            p=.50, q=NULL, n, J, K0=10, tol=.10)

mrss.mod333(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
            rho2, rho3, r21=0, r22=0, r23=0,
            p=.50, q=NULL, n, J, K0=10, tol=.10)

mdes.bcra4f3(power=.80, alpha=.05, two.tailed=TRUE,
             rho2, rho3, p=.50, r21=0, r22=0, r23=0, g3=0,
             n, J, K, L)

power.bcra4f3(es=.25, alpha=.05, two.tailed=TRUE,
              rho2, rho3, p=.50, r21=0, r22=0, r23=0, g3=0,
              n, J, K, L)

mrss.bcra4f3(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
             n, J, L, K0=10, tol=.10,
             rho2, rho3, p=.50, g3=0, r21=0, r22=0, r23=0)
}

\arguments{
  \item{power}{statistical power \eqn{(1-\beta)}.}
  \item{es, esa, esB}{effect size for main/moderator effects, or for path coefficients a (treatment - mediator), or B (overall mediator - outcome) in the mediation model.}
  \item{alpha}{probability of type I error.}
  \item{two.tailed}{logical; \code{TRUE} for two-tailed hypothesis testing, \code{FALSE} for one-tailed hypothesis testing. }
  \item{rho2}{proportion of variance in the outcome between level 2 units (unconditional ICC2).}
  \item{rho3}{proportion of variance in the outcome between level 3 units (unconditional ICC3).}
  \item{rhom2}{proportion of variance in the mediator between level 2 units.}
  \item{rhom3}{proportion of variance in the mediator between level 3 units.}
  \item{omegam2}{ratio of the unconditional variance in the moderator effect that is between level 2 units to the residual variance between level 2 units in the null model.}
  \item{omegam3}{ratio of the unconditional variance in the moderator effect that is between level 3 units to the residual variance between level 3 units in the null model.}
  \item{p}{proportion of level 3 units randomly assigned to treatment.}
  \item{q}{proportion of level 1, level 2, or level 3 units in the moderator subgroup.}
  \item{g3}{number of covariates at level 3.}
  \item{gm3}{number of covariates at level 3 for the mediation model.}
  \item{r21}{proportion of level 1 variance in the outcome explained by level 1 covariates.}
  \item{r22}{proportion of level 2 variance in the outcome explained by level 2 covariates.}
  \item{r23}{proportion of level 3 variance in the outcome explained by level 3 covariates.}
  \item{r2m1}{proportion of mediator variance at level 1 explained by level 1 predictors.}
  \item{r2m2}{proportion of variance in the mediator explained by level 2 predictors.}
  \item{r2m3}{proportion of variance in the moderator effect that is explained by level 3 predictors. For the mediation model, proportion of variance in the mediator explained by level 3 predictors.}
  \item{n}{harmonic mean of level 1 units across level 2 units (or simple average).}
  \item{J}{harmonic mean of level 2 units across level 3 units (or simple average).}
  \item{K}{level 3 sample size.}
  \item{L}{level 4 sample size (blocks).}
  \item{K0}{starting value for \code{K}.}
  \item{tol}{tolerance to end iterative process for finding \code{K}.}
  \item{mc}{logical; \code{TRUE} for monte carlo simulation based power.}
  \item{nsims}{number of replications, if \code{mc = TRUE}.}
  \item{ndraws}{number of draws from the distribution of the path coefficients for each replication, if \code{mc = TRUE}.}
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
# cross-checks for the main effect
mdes.cra3(rho3=.06, rho2=.17, n=15, J=3, K=60)
power.cra3(es=.269, rho3=.06, rho2=.17, n=15, J=3, K=60)
mrss.cra3(es=.269, rho3=.06, rho2=.17, n=15, J=3)

# cross-checks for the randomly varying cont. L1 moderator effect
mdesd.mod331(power=.80, alpha=.05, two.tailed=TRUE,
            rho2=.17, rho3=.06, omegam2=.10, omegam3=.10,
            q=NULL, n=15, J=3, K=60)
power.mod331(es=0.1248, alpha=.05, two.tailed=TRUE,
            rho2=.17, rho3=.06, omegam2=.10, omegam3=.10,
            q=NULL, n=15, J=3, K=60)
mrss.mod331(es=0.1248, alpha=.05, two.tailed=TRUE,
            rho2=.17, rho3=.06, omegam2=.10, omegam3=.10,
            q=NULL, n=15, J=3)

# cross-checks for the non-randomly varying cont. L1 moderator effect
mdesd.mod331(power=.80, alpha=.05, two.tailed=TRUE,
            rho2=.17, rho3=.06, omegam2=0, omegam3=0,
            q=NULL, n=15, J=3, K=60)
power.mod331(es=.0946, alpha=.05, two.tailed=TRUE,
            rho2=.17, rho3=.06, omegam2=0, omegam3=0,
            q=NULL, n=15, J=3, K=60)
mrss.mod331(es=.0946, alpha=.05, two.tailed=TRUE,
            rho2=.17, rho3=.06, omegam2=0, omegam3=0,
            q=NULL, n=15, J=3)

# cross-checks for the randomly varying bin. L1 moderator effect
mdesd.mod331(power=.80, alpha=.05, two.tailed=TRUE,
            rho2=.17, rho3=.06, omegam2=.10, omegam3=.10,
            q=.50, n=15, J=3, K=60)
power.mod331(es=.2082, alpha=.05, two.tailed=TRUE,
            rho2=.17, rho3=.06, omegam2=.10, omegam3=.10,
            q=.50, n=15, J=3, K=60)
mrss.mod331(es=.2082, alpha=.05, two.tailed=TRUE,
            rho2=.17, rho3=.06, omegam2=.10, omegam3=.10,
            q=.50, n=15, J=3)

# cross-checks for the non-randomly varying bin. L1 moderator effect
mdesd.mod331(power=.80, alpha=.05, two.tailed=TRUE,
            rho2=.17, rho3=.06, omegam2=0, omegam3=0,
            q=.50, n=15, J=3, K=60)
power.mod331(es=.1893, alpha=.05, two.tailed=TRUE,
            rho2=.17, rho3=.06, omegam2=0, omegam3=0,
            q=.50, n=15, J=3, K=60)
mrss.mod331(es=.1893, alpha=.05, two.tailed=TRUE,
            rho2=.17, rho3=.06, omegam2=0, omegam3=0,
            q=.50, n=15, J=3)

# cross-checks for the randomly varying bin. L2 moderator effect
mdesd.mod332(rho3=.1, rho2=.1, omegam3=.05,
             q=.5, r21=.30, r22=.4, r2m3=0, n=20, J=4, K=60)
power.mod332(es=.2244, rho3=.1, rho2=.1, omegam3=.05,
             q=.5, r21=.30, r22=.4, r2m3=0, n=20, J=4, K=60)
mrss.mod332(es=.2244, rho3=.1, rho2=.1, omegam3=.05,
            q=.5, r21=.30, r22=.4, r2m3=0, n=20, J=4)

# cross-checks for the randomly varying cont. L2 moderator effect
mdesd.mod332(rho3=.1, rho2=.1, omegam3=.05,
            r21=.30, r22=.4, r2m3=0, n=20, J=4, K=60)
power.mod332(es=.1209, rho3=.1, rho2=.1, omegam3=.05,
            r21=.30, r22=.4, r2m3=0, n=20, J=4, K=60)
mrss.mod332(es=.1209, rho3=.1, rho2=.1, omegam3=.05,
            r21=.30, r22=.4, r2m3=0, n=20, J=4)

# cross-checks for the non-randomly varying bin. L2 moderator effect
mdesd.mod332(rho3=.1, rho2=.1, omegam3=0,
            q=.5, r21=.30, r22=.4, r2m3=0, n=20, J=4, K=60)
power.mod332(es=.2157, rho3=.1, rho2=.1, omegam3=0,
            q=.5, r21=.30, r22=.4, r2m3=0, n=20, J=4, K=60)
mrss.mod332(es=.2157, rho3=.1, rho2=.1, omegam3=0,
            q=.5, r21=.30, r22=.4, r2m3=0, n=20, J=4, K=60)

# cross-checks for the non-randomly varying cont. L2 moderator effect
mdesd.mod332(rho3=.1, rho2=.1, omegam3=0,
            r21=.30, r22=.4, r2m3=0, n=20, J=4, K=60)
power.mod332(es=.1079, rho3=.1, rho2=.1, omegam3=0,
             r21=.30, r22=.4, r2m3=0, n=20, J=4, K=60)
mrss.mod332(es=.1079, rho3=.1, rho2=.1, omegam3=0,
             r21=.30, r22=.4, r2m3=0, n=20, J=4, K=60)

# cross-checks for the randomly varying bin. L3 moderator effect
mdesd.mod333(rho3=.1, rho2=.1, q=.5,
            r21=.3, r22=.4, r23=.5, n=20, J=4, K=60)
power.mod333(es=.4128, rho3=.1, rho2=.1, q=.5,
             r21=.3, r22=.4, r23=.5, n=20, J=4, K=60)
mrss.mod333(es=.4128, rho3=.1, rho2=.1, q=.5,
             r21=.3, r22=.4, r23=.5, n=20, J=4, K=60)

# cross-checks for the randomly varying cont. L3 moderator effect
mdesd.mod333(rho3=.1, rho2=.1,
             r21=.3, r22=.4, r23=.5, n=20, J=4, K=60)
power.mod333(es=.2064, rho3=.1, rho2=.1,
             r21=.3, r22=.4, r23=.5, n=20, J=4, K=60)
mrss.mod333(es=.2064, rho3=.1, rho2=.1,
             r21=.3, r22=.4, r23=.5, n=20, J=4, K=60)

# 3-3-1 mediation
power.med331(esa= .50, esB = .30, rho2 = .15, rho3 = .15,
             r21 = .20, r22 = .20, g3 = 4,
             n = 20, J = 4, K = 80, p = .5)

# 3-2-1 mediation
power.med321(esa= .51, esB = .30, rhom3 = 0.27, rho2 = .15, rho3 = .19,
             r2m2 = .07, gm3 = 4, r2m3 = .16,
             r21 = .02, r22 = .41, g3 = 5, r23 = .38,
             p = .50, n = 20, J = 4, K = 60)

# 3-1-1 mediation
power.med311(esa= .49 , esB = .30,
             rhom2 = .05, rhom3 = .26, rho2 = .15, rho3 = .20,
             r2m1 = .10, r2m2 = .07, r2m3 = .17,
             r21 = .02, r22 = .41, r23 = .38,
             p = .50, n = 20, J = 4, K = 30)

# cross-checks for cluster-randomized block design
# treatment at level 3, with fixed effects across level 4 blocks
mdes.bcra4f3(rho3=.15, rho2=.15,
             n=10, J=4, K=23, L=15)
power.bcra4f3(es=0.137, rho3=.15, rho2=.15,
              n=10, J=4, K=33, L=15)
mrss.bcra4f3(es=0.137, rho3=.15, rho2=.15,
             n=10, J=4, L=15)
}

\references{
Spybrook, J., Kelcey, B., \& Dong, N. (2016). Power for detecting treatment by moderator effects in two-and three-level cluster randomized trials. \emph{Journal of Educational and Behavioral Statistics}, \emph{41}(6), 605-627. \doi{10.3102/1076998616655442}

Dong, N., Kelcey, B., \& Spybrook, J. (2018). Power analyses for moderator effects in three-level cluster randomized trials. \emph{The Journal of Experimental Education}, \emph{86}(3), 489-514. \doi{10.1080/00220973.2017.1315714}

Dong, N., \& Maynard, R. (2013). PowerUp!: A tool for calculating minimum detectable effect sizes and minimum required sample sizes for experimental and quasi-experimental design studies. \emph{Journal of Research on Educational Effectiveness}, \emph{6}(1), 24-67. \doi{10.1080/19345747.2012.673143}

Kelcey, B., Xie, Y., Spybrook, J., \& Dong, N. (2020). Power and sample size determination for multilevel mediation in three-Level cluster-randomized trials. \emph{Multivariate Behavioral Research}. Advance online publication. \doi{10.1080/00273171.2020.1738910}

Kelcey, B., Spybrook, J., Dong, N., \& Bai, F. (2020). Cross-level mediation in school-randomized studies of teacher development: Experimental design and power.  \emph{Journal of Research on Educational Effectiveness}. Advance online publication. \doi{10.1080/19345747.2020.1726540}
}

