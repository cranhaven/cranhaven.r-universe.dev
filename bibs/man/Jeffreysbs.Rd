\name{Jeffreysbs}
\alias{Jeffreysbs}
\title{Computing the Bayesian estimators of the Birnbaum-Saunders (BS) distribution.}
\description{Computing the Bayesian estimators of the BS distribution based on approximated Jeffreys prior proposed by Achcar (1993). The approximated Jeffreys piors is
\eqn{\pi_{j}(\alpha,\beta)\propto\frac{1}{\alpha\beta}\sqrt{\frac{1}{\alpha^2}+\frac{1}{4}}}.
}
\usage{Jeffreysbs(x, CI = 0.95, M0 = 800, M = 1000)}
\arguments{
\item{x}{Vector of observations.}
\item{CI}{Confidence level for constructing percentile and asymptotic confidence intervals. That is 0.95 by default.}
\item{M0}{The number of sampler runs considered as burn-in.}
\item{M}{The number of total sampler runs.}
}
\value{A list including summary statistics of a Gibbs sampler for the Bayesian inference including point estimation for the parameter, its standard error, and the corresponding \eqn{100(1-\alpha)\%} credible interval, goodness-of-fit measures, asymptotic \eqn{100(1-\alpha)\%} confidence interval (CI) and corresponding standard errors, and Fisher information matix.
}
\references{
J. A. Achcar 1993. Inferences for the Birnbaum-Saunders fatigue life model using Bayesian methods, \emph{Computational Statistics \& Data Analysis}, 15 (4), 367-380.
}
\author{Mahdi Teimouri}
\examples{
data(fatigue)
x <- fatigue
Jeffreysbs(x, CI = 0.95, M0 = 800, M = 1000)
}
