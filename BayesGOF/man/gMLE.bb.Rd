\name{gMLE.bb}
\alias{gMLE.bb}
\alias{mom.est}

\title{ Beta-Binomial Parameter Estimation }
\description{
Computes type-II Maximum likelihood estimates \eqn{\hat{\alpha}} and \eqn{\hat{\beta}} for Beta prior \eqn{g\sim}Beta\eqn{(\alpha,\beta)}.  
}
\usage{
gMLE.bb(success, trials, start = NULL, optim.method = "default", 
        lower = 0, upper = Inf)
}

\arguments{
  \item{success}{Vector containing the number of successes.}
  \item{trials}{Vector containing the total number of trials that correspond to the successes.}
  \item{start}{initial parameters; default is NULL which allows function to determine MoM estimates as initial parameters.}
  \item{optim.method}{optimization method in \code{optim()}{stats}.}
  \item{lower}{ lower bound for parameters; default is 0.}
  \item{upper}{ upper bound for parameters; default is infinity.}
}

\value{
  \item{estimate}{MLE estimate for beta parameters.}
  \item{convergence }{Convergence code from \code{optim()};  0 means convergence.}
  \item{loglik}{Loglikelihood that corresponds with MLE estimated parameters.}
  \item{initial}{Initial parameters, either user-defined or determined from method of moments.}
  \item{hessian}{Estimated Hessian matrix at the given solution.}
  }
\references{\url{https://github.com/SupplyFrame/EmpiricalBayesR/blob/master/EmpiricalBayesEstimation.R
}
}

\author{Aleksandar Bradic}

\examples{
data(rat)
### MLE estimate of alpha and beta
rat.mle <- gMLE.bb(rat$y, rat$N)$estimate
rat.mle
### MoM estimate of alpha and beta
rat.mom <- gMLE.bb(rat$y, rat$N)$initial
rat.mom
}