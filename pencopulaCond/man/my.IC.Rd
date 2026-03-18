\name{my.IC}
\alias{my.IC}

\title{Calculating the AIC-value}

\description{
Calculating the AIC-value and cAIC-value of the copula density estimation.

}
\usage{
my.IC(penden.env,temp=FALSE)
}

\arguments{
  \item{penden.env}{Containing all information, environment of
    paircopula()}
  \item{temp}{Default=FALSE, if TRUE temporary values of AIC and cAIC are calculated.}
}

\details{
AIC is calculated as
\eqn{AIC(\lambda)= - 2*l({\bf u},\hat{\bf{b}}) + 2*df(\lambda)}

cAIC is calculated as
\eqn{cAIC(\lambda)= - 2*l({\bf u},\hat{\bf{b}}) + 2*df(\lambda) + \frac{2df(\lambda)(df(\lambda)+1)}{n-df(\lambda)-1} }

BIC is calculated as
\eqn{BIC(\lambda)= 2*l({\bf u},\hat{\bf{b}}) + 2*df(\lambda)*log(n)}
}

\value{
  \item{AIC}{sum of twice the negative non-penalized log likelihood and df(lambda)}
  \item{cAIC}{sum of twice the negative non-penalized log likelihood and df(lambda) and (2df(lambda)(df(lambda)+1))/(n-df(lambda)-1)}
  \item{BIC}{sum of twice the non-penalized log likelihood and log(n)*df(lambda)}
  All values are saved in the environment.
}

\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}
\keyword{math}
\references{Flexible Copula Density Estimation with Penalized
  Hierarchical B-Splines, Kauermann G., Schellhase C. and Ruppert, D. (2013), Scandinavian Journal of Statistics 40(4), 685-705.

Estimating Non-Simplified Vine Copulas Using Penalized Splines, Schellhase, C. and Spanhel, F. (2017), Statistics and Computing.}
