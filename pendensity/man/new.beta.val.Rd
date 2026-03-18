\name{new.beta.val}
\alias{new.beta.val}

\title{Calculating the new parameter beta}
\description{
Calculating the direction of the Newton-Raphson step for the known beta and iterate a step size bisection to control the maximizing of the penalized likelihood.
}
\usage{
new.beta.val(llold, penden.env)
}

\arguments{
  \item{llold}{log likelihood of the algorithm one step before}
  \item{penden.env}{Containing all information, environment of pendensity()}
}
\details{
We terminate the search for the new beta, when the new log likelihood is smaller than the old likelihood and the step size is smaller or equal 1e-3. We calculate the direction of the Newton Raphson step for the known \eqn{beta_t} and iterate a step size bisection to control the maximizing of the penalized likelihood \deqn{l_p(\beta_t,\lambda_0)}{%
\eqn{l(beta,lambda0)}}. This means we set \deqn{\beta_{t+1}=\beta_t - 2^{-v} \{s_p(\beta_t,\lambda_0) \cdot (-J_p(\beta_t,\lambda_0))^{-1}\}}{%
\eqn{beta[t+1]=beta[t]-(2/v)*sp(beta,lambda0)*(-Jp(beta[t],lambda0))^-1}} with \eqn{s_p} as penalized first order derivative and \eqn{J_p} as penalized second order derivative. We begin with \eqn{v=0}. Not yielding a new maximum for a current v, we increase v step by step respectively bisect the step size. We terminate the iteration, if the step size is smaller than some reference value epsilon (eps=1e-3) without yielding a new maximum. We iterate for new parameter beta until the new log likelihood depending on the new estimated parameter beta differ less than 0.1 log-likelihood points from the log likelihood estimated before.\cr
}
\value{
  \item{Likelie}{corresponding log likelihood}
  \item{step}{used step size}
}

\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}

\references{Density Estimation with a Penalized Mixture Approach, Schellhase C. and Kauermann G. (2012), Computational Statistics 27 (4), p. 757-777.}
\keyword{nonparametric}

