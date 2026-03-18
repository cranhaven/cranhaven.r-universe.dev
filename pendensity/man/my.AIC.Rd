\name{my.AIC}
\alias{my.AIC}

\title{Calculating the AIC value}
\description{
Calculating the AIC value of the density estimation. Therefore, we add the unpenalized log likelihood of the estimation and the degree of freedom, which are 	
}
\usage{
my.AIC(penden.env, lambda0, opt.Likelihood = NULL)
}

\arguments{
  \item{penden.env}{Containing all information, environment of pendensity()}
  \item{lambda0}{penalty parameter lambda}
  \item{opt.Likelihood}{optimal unpenalized likelihood of the density estimation}
}

\details{
AIC is calculated as
\eqn{AIC(\lambda)= - l(\hat{\beta}) + df(\lambda)}
}
\value{
  \item{myAIC}{sum of the negative unpenalized log likelihood and mytrace}
  \item{mytrace}{calculated mytrace as the sum of the diagonal matrix df, which results as the product of the inverse of the penalized second order derivative of the log likelihood with the unpenalized second order derivative of the log likelihood}
}
\references{Density Estimation with a Penalized Mixture Approach, Schellhase C. and Kauermann G. (2012), Computational Statistics 27 (4), p. 757-777.}
\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}
\keyword{math}

