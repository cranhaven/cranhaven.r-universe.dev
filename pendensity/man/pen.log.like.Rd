\name{pen.log.like}
\alias{pen.log.like}
\title{Calculating the log likelihood}

\description{
Calculating the considered log likelihood. If one chooses lambda0=0, one gets the (actual) unpenalized log likelihood. Otherwise, one gets the penalized log likelihood for the used fitted values of the response y and the actual parameter set beta.
}

\usage{
pen.log.like(penden.env,lambda0,f.hat.val=NULL,beta.val=NULL)
}

\arguments{
  \item{penden.env}{Containing all information, environment of pendensity()}
  \item{lambda0}{penalty parameter lambda}
  \item{f.hat.val}{matrix contains the fitted values of the response, if NULL the matrix is caught in the environment}
  \item{beta.val}{actual parameter set beta, if NULL the vector is caught in the environment}
}
\details{
The calculation depends on the fitted values of the response as well as on the penalty parameter lambda and the penalty matrix Dm.\cr
\deqn{l(\beta)=\sum_{i=1}^{n} \left[ \log \{\sum_{k=-K}^K c_k(x_i,\beta) \boldsymbol\phi_k(y_i)\}  \right]- \frac 12 \lambda \beta^T D_m \beta}{%
 \eqn{l(\beta)=sum(log(sum(c_k(x_i,\beta) \phi_k(y_i))))-0.5*\lambda \beta^T D_m \beta}}.

The needed values are saved in the environment.
}

\value{
Returns the log likelihood depending on the penalty parameter lambda.
}
\references{Density Estimation with a Penalized Mixture Approach, Schellhase C. and Kauermann G. (2012), Computational Statistics 27 (4), p. 757-777.}
\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}

\keyword{nonparametric}

