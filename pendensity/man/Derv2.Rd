\name{Derv2}
\alias{Derv2}
\title{Calculating the second order derivative with and without penalty}
\description{
Calculating the second order derivative of the likelihood function of the pendensity approach w.r.t. the parameter beta. Thereby, for later use, the program returns the second order derivative with and without the penalty.
}
\usage{
Derv2(penden.env, lambda0)
}
\details{
We approximate the second order derivative in this approach with the negative fisher information. 
\deqn{J(\beta)= -  \frac{\partial^2 l(\beta)}{\partial \beta \ \partial \beta^T} \approx \sum_{i=1}^n s_i(\beta) s_i^T(\beta) .}{%
\eqn{J(beta)= partial^2 l(beta) / (partial(beta) partial(beta)) = sum(s[i](beta) s[i]^T(beta))}}
Therefore we construct the second order derivative of the i-th observation w.r.t. beta with the outer product of the matrix Derv1.cal and the i-th row of the matrix Derv1.cal.\cr
The penalty is computed as \deqn{\lambda D_m}{%
\eqn{ lambda Dm}}.
}
\arguments{
  \item{penden.env}{Containing all information, environment of pendensity()}
  \item{lambda0}{smoothing parameter lambda}
}

\value{
  \item{Derv2.pen}{second order derivative w.r.t. beta with penalty}
  \item{Derv2.cal}{second order derivative w.r.t. beta without penalty. Needed for calculating of e.g. AIC.}
}
\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}
\references{Density Estimation with a Penalized Mixture Approach, Schellhase C. and Kauermann G. (2012), Computational Statistics 27 (4), p. 757-777.}

\keyword{math}

