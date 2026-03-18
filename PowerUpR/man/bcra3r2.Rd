\name{bcra3r2}
\alias{bcra3r2}
\alias{bcra3r2_pn}
\alias{mdes.bcra3r2}
\alias{power.bcra3r2}
\alias{mrss.bcra3r2}
\alias{mdes.bcra3r2_pn}
\alias{power.bcra3r2_pn}
\alias{mrss.bcra3r2_pn}
\title{Three-Level Blocked Cluster-level Random Assignment Design, Treatment at Level 2}

\description{
For three-level cluster-randomized block designs (treatment at level 2, with random effects across level 3 blocks), use \code{mdes.bcra3r2()} to calculate the minimum detectable effect size, \code{power.bcra3r2()} to calculate the statistical power, and \code{mrss.bcra3r2()} to calculate the minimum required sample size.

For partially nested blocked cluster randomized trials (interventions clusters in treatment groups) use \code{mdes.bcra3r2_pn()} to calculate the minimum detectable effect size, \code{power.bcra3r2_pn()} to calculate the statistical power, and \code{mrss.bcra3r2_pn()} to calculate the minimum required sample size (number of blocks).
}

\usage{
mdes.bcra3r2(power=.80, alpha=.05, two.tailed=TRUE,
             rho2, rho3, esv3=NULL, omega3=esv3/rho3,
             p=.50, g3=0, r21=0, r22=0, r2t3=0,
             n, J, K)

power.bcra3r2(es=.25, alpha=.05, two.tailed=TRUE,
              rho2, rho3, esv3=NULL, omega3=esv3/rho3,
              p=.50, g3=0, r21=0, r22=0, r2t3=0,
              n, J, K)

mrss.bcra3r2(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
             n, J, K0=10, tol=.10,
             rho2, rho3, esv3=NULL, omega3=esv3/rho3,
             p=.50, g3=0, r21=0, r22=0, r2t3=0)

mdes.bcra3r2_pn(power=.80, alpha=.05, two.tailed=TRUE, df=NULL,
                rho3_trt=.10, omega3=.50, rho2_trt=.20, rho_ic=0,
                p=.50, r21=0, g3=0, n, J, K, ic_size=1)

power.bcra3r2_pn(es=.25,alpha=.05, two.tailed=TRUE, df=NULL,
                 rho3_trt=.10, omega3=.50, rho2_trt=.20, rho_ic=0,
                 p=.50, r21=0, g3=0, n, J, K, ic_size=1)

mrss.bcra3r2_pn(es=.25, power=.80, alpha=.05, two.tailed=TRUE, z.test=FALSE,
                rho3_trt=.10, omega3 = .50, rho2_trt=.20, rho_ic=0,
                p=.50, r21=0, g3=0, n, J, ic_size=1, K0=10, tol=.10)
}

\arguments{
  \item{power}{statistical power \eqn{(1-\beta)}.}
  \item{es}{effect size.}
  \item{alpha}{probability of type I error.}
  \item{two.tailed}{logical; \code{TRUE} for two-tailed hypothesis testing, \code{FALSE} for one-tailed hypothesis testing.}
  \item{df}{degrees of freedom.}
  \item{rho_ic}{proportion of variance in the outcome that is between intervention clusters.}
  \item{rho2_trt}{proportion of variance in the outcome (for treatment group) that is between level 2 units.}
  \item{rho3_trt}{proportion of variance in the outcome (for treatment group) that is between level 3 units.}
  \item{rho2}{proportion of variance in the outcome between level 2 units (unconditional ICC2).}
  \item{rho3}{proportion of variance in the outcome between level 3 units (unconditional ICC3).}
  \item{esv3}{effect size variability as the ratio of the treatment effect variance between level 3 units to the total variance in the outcome (level 1 + level 2 + level 3). \code{esv} also works. Ignored when \code{omega3} is specified.}
  \item{omega3}{treatment effect heterogeneity as ratio of treatment effect variance among level 3 units to the residual variance at level 3.}
  \item{p}{average proportion of level 2 units randomly assigned to treatment within level 3 units.}
  \item{g3}{number of covariates at level 3.}
   \item{r21}{proportion of level 1 variance in the outcome explained by level 1 covariates (applies to all levels in partially nested designs).}
  \item{r22}{proportion of level 2 variance in the outcome explained by level 2 covariates.}
  \item{r2t3}{proportion of treatment effect variance among level 3 units explained by level 3 covariates.}
  \item{ic_size}{sample size for each intervention cluster.}
  \item{n}{harmonic mean of level 1 units across level 2 units (or simple average).}
  \item{J}{harmonic mean of level 2 units across level 3 units (or simple average).}
  \item{K}{number of level 3 units.}
  \item{K0}{starting value for \code{K}.}
  \item{tol}{tolerance to end iterative process for finding \code{K}.}
  \item{z.test}{logical; \code{TRUE} for z-test.}
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
mdes.bcra3r2(rho3=.13, rho2=.10, omega3=.4,
             n=10, J=6, K=24)
power.bcra3r2(es = .246, rho3=.13, rho2=.10, omega3=.4,
              n=10, J=6, K=24)
mrss.bcra3r2(es = .246, rho3=.13, rho2=.10, omega3=.4,
             n=10, J=6)

# cross-checks
mdes.bcra3r2_pn(rho3_trt=.10, omega3=.50,
                rho2_trt=.15, rho_ic=.20,
                n=40, J=60, K=6, ic_size=10)
power.bcra3r2_pn(es=.399, rho3_trt=.10, omega3=.50,
                rho2_trt=.15, rho_ic=.20,
                n=40, J=60, K=6, ic_size=10)
mrss.bcra3r2_pn(es=.399, rho3_trt=.10, omega3=.50,
                 rho2_trt=.15, rho_ic=.20,
                 n=40, J=60, ic_size=10)
}

\references{
Dong, N., & Maynard, R. (2013). PowerUp!: A tool for calculating minimum detectable effect sizes and minimum required sample sizes for experimental and quasi-experimental design studies. \emph{Journal of Research on Educational Effectiveness}, \emph{6}(1), 24-67. \doi{10.1080/19345747.2012.673143}

Lohr, S., Schochet, P. Z., & Sanders, E. (2014). Partially Nested Randomized Controlled Trials in Education Research: A Guide to Design and Analysis. NCER 2014-2000. National Center for Education Research. \url{https://ies.ed.gov/ncer/pubs/20142000/pdf/20142000.pdf}
}
