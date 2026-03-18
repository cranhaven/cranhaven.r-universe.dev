\name{replication}
\alias{rep}
\alias{mdh}
\alias{power.rep}
\alias{mdh.rep}
\alias{mrns.rep}
\alias{replication}
\title{Unambiguous Test of Replication for Ensemble of Studies}

\description{
Use \code{power.rep()} for the statistical power, \code{mdh.rep()} for the minimum detectable heterogeneity, and \code{mrns.rep()} for the minimum required number of studies. Functions implement methods designed to conduct unambiguous test of replication for ensemble of studies (Hedges & Schauer, 2019). \code{mdh} argument is the effect heterogeneity above and beyond sampling variability. An \code{mdh = 0} specification means effects are same across subgroups or moderator levels in the population. Effects will vary from each other solely due to sampling error. In this case, with large samples, heterogeneity detected after ensample of studies are conducted will be equal to unity.
}

\usage{
power.rep(k = 2L, mdh = 1/4, mdh.null = 0, alpha = .05)

mdh.rep(k = 2L, mdh.max = 15, alpha = .05, power = 0.80,
        mdh.null = 0, step = .001, plot = FALSE)

mrns.rep(power = .80, mdh = 1/4, mdh.null = 0, alpha = .05,
         tol = .001)
}

\arguments{
  \item{k}{number of replications.}
  \item{power}{statistical power \eqn{(1-\beta)}.}
  \item{alpha}{probability of type I error.}
  \item{mdh}{minimum detectable heterogeneity (MDH).}
  \item{mdh.null}{MDH for null hypothesis.}
  \item{mdh.max}{maximum of possible MDH values for grid search.}
  \item{step}{step size to generate possible MDH values.}
  \item{plot}{logical; if TRUE plots MDH - power curve.}
  \item{tol}{tolerance to end iterative process for finding \code{k}}
}

\value{
  \item{fun}{function name.}
  \item{parms}{list of parameters used in the calculation.}
  \item{df}{degrees of freedom.}
  \item{power}{statistical power \eqn{(1-\beta)}.}
  \item{mdh}{minimum detectable heterogeneity (MDH).}
  \item{k}{minimum required number of studies.}
  \item{df}{degrees of freedom.}
}

\examples{
# cross-checks
power.rep(k = 20L, mdh = 0.50)
mdh.rep(k = 20L, power = .39)
mrns.rep(power = .39, mdh = .50)
}

\references{
Hedges, L. V., & Schauer, J. (2019). Statistical analyses for studying replication: Meta-analytic perspectives. Psychological Methods, 24(5), 557-570. http://dx.doi.org/10.1037/met0000189
}
