\name{waic_hetop}
\alias{waic_hetop}

\title{WAIC for FH-HETOP model}

\description{
  Computes the Watanabe-Akaike information criterion (WAIC)
  for the FH-HETOP model using the data and posterior samples of the
  group means, group standard deviations and cutpoints.
}

\usage{
waic_hetop(ngk, samps)
}

\arguments{
  \item{ngk}{
    Numeric matrix of dimension \code{G x K} in which column \code{k} of
    row \code{g} indicates the number of units from group \code{g}
    falling into category \code{k}.
  }

  \item{samps}{
    A matrix of posterior samples that includes at least the
    group means, group standard deviations and the cutpoints.  Column
    names for these three collections of parameters must contain the
    strings 'mu', 'sigma' and 'cuts', respectively.
  }
}

\details{
  Although this function can be called directly by the user, it is
  primarily intended to be used to compute WAIC as part of the function
  \code{fh_hetop}.  Details on the WAIC calculation are provided by
  Vehtari and Gelman (2017).
}

\value{
  A list with the following components:

  \item{lpd_hat}{Part 1 of the WAIC calculation: the estimated log
  pointwise predictive density, summed across groups.}

  \item{phat_waic}{Part 2 of the WAIC calculation: the effective number
  of parameters.}

  \item{waic}{The WAIC criterion: -2 times (lpd_hat - phat_waic).}
}

\references{
  Lockwood J.R., Castellano K.E. and Shear B.R. (2018).
  \dQuote{Flexible Bayesian models for inferences from coarsened,
    group-level achievement data,}
  \emph{Journal of Educational and Behavioral Statistics.} 43(6):663--692.

  Vehtari A., Gelman A. and Gabry J. (2017).
  \dQuote{Practical Bayesian model evaluation using leave-one-out
    cross-validation and WAIC,}
  \emph{Statistics and Computing.} 27(5):1413--1432.
}

\author{
  J.R. Lockwood \email{jrlockwood@ets.org}
}

\examples{
## example call using data 'ngk' and FH-HETOP model object 'm'
## (demonstrated in examples for fh_hetop):
##
## waic_hetop(ngk, m$BUGSoutput$sims.matrix)
}

\keyword{utilities}
