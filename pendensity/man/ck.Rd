\name{ck}
\alias{ck}
\title{Calculating the actual weights ck}

\description{
Calculating the actual weights ck for each factor combination of the covariates combinations.
}

\usage{
ck(penden.env, beta.val)
}

\arguments{
  \item{penden.env}{Containing all information, environment of pendensity()}
  \item{beta.val}{actual parameter beta}
}

\details{
The weights in depending of the covariate 'x' are calculated as follows.

\eqn{ c_k(x,\beta)=\frac{\exp(Z(x)\beta_k)}{\sum_{j=-K}^{K}\exp(Z(x)\beta_j)}}

For estimations without covariates, Z doesn't appear in calculations.

\eqn{c_k(\beta)=\frac{\exp(\beta_k)}{\sum_{k=-K}^{K}\exp(\beta_k)}}

Starting density calculation, the groupings of the covariates are
indexed in the main program. The groupings are saved in 'x.factor', the index which response
belongs to which group is saved in 'Z.index'. Therefore, one can link to
the rows in 'x.factor' to calculate the weights 'ck'. 

The needed values are saved in the environment.
  }

\value{
Returning the actual weights ck, depending on the actual parameter beta in a matrix with rows.
}
\references{Density Estimation with a Penalized Mixture Approach, Schellhase C. and Kauermann G. (2012), Computational Statistics 27 (4), p. 757-777.}
\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}

\keyword{nonparametric}
