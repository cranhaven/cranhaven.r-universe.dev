\name{Generalized Extreme Value Distribution}
\alias{rgev}
\alias{dgev}
\alias{qgev}
\alias{pgev}

\title{The Generalized Extreme Value Distribution}
\description{Density, distribution function, quantile function and random
  generation for the GP distribution with location equal to 'loc',
  scale equal to 'scale' and shape equal to 'shape'.}

\usage{
rgev(n, loc = 0, scale = 1, shape = 0)
pgev(q, loc = 0, scale = 1, shape = 0, lower.tail = TRUE)
qgev(p, loc = 0, scale = 1, shape = 0, lower.tail = TRUE)
dgev(x, loc = 0, scale = 1, shape = 0, log = FALSE)
}

\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations.}
  \item{loc}{vector of the location parameters.}
  \item{scale}{vector of the scale parameters.}
  \item{shape}{a numeric of the shape parameter.}
  \item{lower.tail}{logical; if TRUE (default), probabilities are \eqn{\Pr[ X
      \le x]}{Pr[ X <= x]}, otherwise, \eqn{\Pr[X > x]}{Pr[X > x]}.}
  \item{log}{logical; if TRUE, probabilities p are given as log(p).}
}
\value{
  If 'loc', 'scale' and 'shape' are not specified they assume the default
  values of '0', '1' and '0', respectively.
  
  The GEV distribution function for loc = \eqn{u}{u}, scale =
  \eqn{\sigma}{\sigma} and shape = \eqn{\xi}{\xi} is
  
  \deqn{G(x) = \exp\left[-\left\{1 + \xi \frac{x - u}{\sigma}
    \right\}^{-1 / \xi} \right]}{%
    G(z) = exp[-{1 + \xi (x - u) / \sigma}^(-1/\xi)]}  
  for \eqn{1 + \xi ( x - u ) / \sigma > 0}{1 + \xi (x - u) / \sigma > 0}
    and \eqn{x > u}{x > u}, where \eqn{\sigma > 0}{\sigma > 0}. If
    \eqn{\xi = 0}{\xi =  0}, the distribution is defined by continuity
    corresponding to the Gumbel distribution.
}
\examples{
dgev(0.1)
rgev(100, 1, 2, 0.2)
qgev(seq(0.1, 0.9, 0.1), 1, 0.5, -0.2)
pgev(12.6, 2, 0.5, 0.1)
}
\keyword{distribution}
