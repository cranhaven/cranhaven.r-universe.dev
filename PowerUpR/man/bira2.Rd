\name{bira2}
\alias{bira2}
\alias{bira2r1}
\alias{bira2_pn}
\alias{mdes.bira2}
\alias{power.bira2}
\alias{mrss.bira2}
\alias{mdes.mod211}
\alias{mdesd.mod211}
\alias{power.mod211}
\alias{mrss.mod211}
\alias{mdes.mod212}
\alias{mdesd.mod212}
\alias{power.mod212}
\alias{mrss.mod212}
\alias{mdes.bira2r1}
\alias{power.bira2r1}
\alias{mrss.bira2r1}
\alias{mdes.bira2_pn}
\alias{power.bira2_pn}
\alias{mrss.bira2_pn}
\title{Two-Level Blocked Individual-level Random Assignment Design}

\description{
For two-level randomized block designs (treatment at level 1, with random effects across level 2 blocks), use \code{mdes.bira2()} to calculate the minimum detectable effect size, \code{power.bira2()} to calculate the statistical power, and \code{mrss.bira2()} to calculate the minimum required sample size (number of blocks).

For treatment effect moderated by level 1 moderator use \code{power.mod211()}, \code{mdesd.mod211()}, and \code{mrss.mod211()} functions. For treatment effect moderated by level 2 moderator, use \code{power.mod212()}, \code{mdesd.mod212()}, and \code{mrss.mod212()} functions.

For partially nested blocked individual-level random assignment designs (blocked randomized controlled trial with intervention clusters) use \code{mdes.bira2_pn()} to calculate the minimum detectable effect size, \code{power.bira2_pn()} to calculate the statistical power, and \code{mrss.bira2_pn()} to calculate the minimum required sample size (number of blocks).

}

\usage{
mdes.bira2(power=.80, alpha=.05, two.tailed=TRUE,
           rel1=1, rho2, esv2=NULL, omega2=esv2/rho2,
           g2=0, r21=0, r2t2=0, p=.50, n, J)

power.bira2(es=.25, alpha=.05, two.tailed=TRUE,
            rel1=1, rho2, esv2=NULL, omega2=esv2/rho2,
            g2=0, r21=0, r2t2=0, p=.50, n, J)

mrss.bira2(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
           rel1=1, rho2, esv2=NULL, omega2=esv2/rho2,
           r21=0, r2t2=0, J0=10, tol=.10, g2=0, p=.50, n)

power.mod211(es=.25, alpha=.05, two.tailed=TRUE,
             rho2, omega2tm, r21=0,
             p=.50, q=NULL, n, J)

mdesd.mod211(power=.80, alpha=.05, two.tailed=TRUE,
             rho2, omega2tm, g1=0, r21=0,
             p=.50, q=NULL, n, J)

mrss.mod211(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
            n, J0=10, tol=.10, rho2, omega2tm, r21=0,
            p=.50, q=NULL)

power.mod212(es=.25, alpha=.05, two.tailed=TRUE,
             rho2, omega2t, r21=0,
             p=.50, q=NULL, n, J)

mdesd.mod212(power=.80, alpha=.05, two.tailed=TRUE,
             rho2, omega2t, g1=0, r21=0,
             p=.50, q=NULL, n, J)

mrss.mod212(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
            n, J0=10, tol=.10, rho2, omega2t, r21=0,
            p=.50, q=NULL)

mdes.bira2_pn(power=.80, alpha=.05, two.tailed=TRUE, df=NULL,
              rho2_trt=.20, omega2=.50, rho_ic=0,
              p=.50, g2=0, r21=0, n, J, ic_size=1)

power.bira2_pn(es=.25,alpha=.05, two.tailed=TRUE, df=NULL,
               rho2_trt=.20, omega2=.50, rho_ic=0,
               p=.50, g2=0, r21=0, n, J, ic_size=1)

mrss.bira2_pn(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
              z.test=FALSE, rho2_trt=.20, omega2=.50, rho_ic=0,
              p=.50, g2=0, r21=0, n, ic_size=1, J0=10, tol=.10)
}

\arguments{
  \item{power}{statistical power \eqn{(1-\beta)}.}
  \item{es}{effect size.}
  \item{alpha}{probability of type I error.}
  \item{two.tailed}{logical; \code{TRUE} for two-tailed hypothesis testing, \code{FALSE} for one-tailed hypothesis testing. }
  \item{df}{degrees of freedom.}
  \item{rho_ic}{proportion of variance in the outcome (for treatment group) that is between intervention clusters.}
  \item{rho2_trt}{proportion of variance in the outcome (for treatment group) that is between level 2 units.}
  \item{rel1}{level 1 outcome reliability coefficient (see Cox \& Kelcey, 2019, p. 23).}
  \item{rho2}{proportion of variance in the outcome between level 2 units (unconditional ICC2). \code{rho} also works.}
  \item{esv2}{effect size variability as the ratio of the treatment effect variance between level 2 units to the total variance in the outcome (level 1 + level 2). \code{esv} also works. Ignored when \code{omega2} is specified.}
  \item{omega2}{treatment effect heterogeneity as the ratio of the treatment effect variance between level 2 units to the unconditional level 2 residual variance. \code{omega} also works.}
  \item{omega2t}{standardized treatment effect variability across sites in the model that is not conditional on Level 2 moderator (ratio of the treatment effect variance between level 2 units to the total variance in the outcome.)}
  \item{omega2tm}{standardized effect variability of the moderation across sites (ratio of the moderated treatment effect variance between level 2 units to the total variance in the outcome.)}
  \item{p}{average proportion of level 1 units randomly assigned to treatment within level 2 units.}
  \item{q}{proportion of level 1 (on average) or level 2 units in the moderator subgroup.}
  \item{g1}{number of covariates at level 1.}
  \item{g2}{number of covariates at level 2.}
  \item{r21}{proportion of level 1 variance in the outcome explained by level 1 covariates (applies to all levels in partially nested designs).}
  \item{r2t2}{proportion of treatment effect variance among level 2 units explained by level 2 covariates.}
  \item{n}{level 1 sample size per block (average or harmonic mean).}
  \item{J}{number of blocks.}
  \item{ic_size}{sample size for each intervention cluster.}
  \item{J0}{starting value for \code{J}.}
  \item{tol}{tolerance to end iterative process for finding \code{J}.}
  \item{z.test}{logical; \code{TRUE} for z-test.}
}

\value{
  \item{fun}{function name.}
  \item{parms}{list of parameters used in power calculation.}
  \item{df}{degrees of freedom.}
  \item{ncp}{noncentrality parameter.}
  \item{power}{statistical power \eqn{(1-\beta)}.}
  \item{mdes}{minimum detectable effect size.}
  \item{J}{number of level 2 units.}
}

\examples{
# cross-checks
mdes.bira2(rho2=.17, omega2=.50, n=15, J=20)
power.bira2(es=.366, rho2=.17, omega2=.50, n=15, J=20)
mrss.bira2(es=.366, rho2=.17, omega2=.50, n=15)

# cross-checks
power.mod211(es=.248, rho2=.247, omega2tm=.148, r21=.493, n=20, J=35)
mdes.mod211(power=.853, rho2=.247, omega2tm=.148, r21=.493, n=20, J=35)
mrss.mod211(es=.248, power = .853, rho2=.247, omega2tm=.148, r21=.493, n=20)

# cross-checks
power.mod212(es=.248, rho2=.247, omega2t=.148, r21=.493, n=20, J=20)
mdes.mod212(power=.739, rho2=.247, omega2t=.148, r21=.493, n=20, J=20)
mrss.mod212(es=.248, power=.739, rho2=.247, omega2t=.148, r21=.493, n=20)

# cross-checks
mdes.bira2_pn(n=20, J=15, rho_ic=.10, ic_size=5)
power.bira2_pn(es=.357, n=20, J=15, rho_ic=.10, ic_size=5)
mrss.bira2_pn(es=.357, n=20, rho_ic=.10, ic_size=5)
}

\references{
Cox, K., \& Kelcey, B. (2019). Optimal design of cluster-and multisite-randomized studies using fallible outcome measures. Evaluation Review, 43(3-4), 189-225. \doi{10.1177/0193841X19870878}

Dong, N., Kelcey, B., \& Spybrook, J. (2020). Design considerations in multisite randomized trials probing moderated treatment effects. \emph{Journal of Educational and Behavioral Statistics.} Advance online publication. \doi{10.3102/1076998620961492}

Dong, N., \& Maynard, R. (2013). PowerUp!: A tool for calculating minimum detectable effect sizes and minimum required sample sizes for experimental and quasi-experimental design studies. \emph{Journal of Research on Educational Effectiveness}, \emph{6}(1), 24-67. \doi{10.1080/19345747.2012.673143}

Lohr, S., Schochet, P. Z., \& Sanders, E. (2014). Partially nested randomized controlled trials in education research: A guide to design and analysis. NCER 2014-2000. National Center for Education Research. \url{https://ies.ed.gov/ncer/pubs/20142000/pdf/20142000.pdf}
}
