\name{gMLE.pg}
\alias{gMLE.pg}


\title{ Negative-Binomial Parameter Estimation }
\description{
Computes Type-II Maximum likelihood estimates \eqn{\hat{\alpha}} and \eqn{\hat{\beta}} for gamma prior \eqn{g\sim} Gamma\eqn{(\alpha, \beta)}.  
}
\usage{
gMLE.pg(cnt.vec, exposure = NULL, start.par = c(1,1))

}

\arguments{
  \item{cnt.vec}{Vector containing Poisson counts.}
  \item{exposure}{Vector containing exposures for each count.  The default is no exposure, thus \code{exposure = NULL}.}
  \item{start.par}{Initial values that will pass to \code{optim}.}
  
}

\value{
  Returns a vector where the first component is \eqn{\alpha} and the second component is the scale parameter \eqn{\beta} for the gamma distribution: \eqn{\frac{1}{\Gamma(\alpha)\beta^\alpha} \theta^{\alpha-1}e^{-\frac{\theta}{\beta}}.}
}  
\references{
Koenker, R. and Gu, J., 2017. "REBayes: An R Package for Empirical Bayes Mixture Methods," \emph{Journal of Statistical Software, Articles, 82}(8), pp. 1-26.
}

\author{Doug Fletcher
}

\examples{
### without exposure
data(ChildIll)
ill.start <- gMLE.pg(ChildIll)
ill.start
### with exposure
data(NorbergIns)
X <- NorbergIns$deaths
E <- NorbergIns$exposure/344
norb.start <- gMLE.pg(X, exposure = E)
norb.start
}