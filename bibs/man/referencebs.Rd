 \name{referencebs}
\alias{referencebs}
\title{Computing the Bayesian estimators of the Birnbaum-Saunders (BS) distribution.}
\description{Computing the Bayesian estimators of the BS distribution using reference prior proposed by Berger and Bernardo(1989). The joint distribution of the priors is
\eqn{\pi(\alpha,\beta)=1/(\alpha,\beta)}.
}
\usage{referencebs(x, CI = 0.95, M0 = 800, M = 1000)}
\arguments{
\item{x}{Vector of observations.}
\item{CI}{Confidence level for constructing percentile and asymptotic confidence intervals. That is 0.95 by default.}
\item{M0}{The number of sampler runs considered as burn-in.}
\item{M}{The number of total sampler runs.}
}
\value{A list including summary statistics of a Gibbs sampler for Bayesian inference including point estimation for the parameter, its standard error, and the corresponding \eqn{100(1-\alpha)\%} credible interval, goodness-of-fit measures, asymptotic \eqn{100(1-\alpha)\%} confidence interval (CI) and corresponding standard errors, and Fisher information matix.
}
\references{
J. O. Berger and J. M. Bernardo 1989. Estimating a product of means: Bayesian analysis with reference priors. \emph{Journal of the
 American Statistical Association}, 84(405), 200-207.
 }
\author{Mahdi Teimouri}
\examples{
data(fatigue)
x <- fatigue
referencebs(x, CI = 0.95, M0 = 800, M = 1000)
}
