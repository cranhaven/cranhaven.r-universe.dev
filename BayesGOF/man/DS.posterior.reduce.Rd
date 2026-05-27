\name{DS.posterior.reduce}
\alias{DS.posterior.reduce}
\title{
Posterior Expectation and Modes of DS object
}
\description{ A function that determines the posterior expectations \eqn{E(\theta_0 | y_0)} and posterior modes for a set of observed data.}
\usage{
DS.posterior.reduce(DS.GF.obj, exposure)
}
\arguments{
   \item{DS.GF.obj}{Object resulting from running DS.prior function on a data set.}
   \item{exposure}{In the case of the Poisson family with exposure, represents the exposure values for the count data.}

}

\value{ Returns \eqn{k \times 4} matrix with the columns indicating PEB mean, DS mean, PEB mode, and DS modes for \eqn{k} observations in the data set.}

\references{
Mukhopadhyay, S. and Fletcher, D., 2018. "Generalized Empirical Bayes via Frequentist Goodness of Fit," \emph{Nature Scientific Reports}, 8(1), p.9983, \url{https://www.nature.com/articles/s41598-018-28130-5 }.}
\author{Doug Fletcher
}
\examples{
data(rat)
rat.start <- gMLE.bb(rat$y, rat$n)$estimate
rat.ds <- DS.prior(rat, max.m = 4, rat.start, family = "Binomial")
DS.posterior.reduce(rat.ds)
}

