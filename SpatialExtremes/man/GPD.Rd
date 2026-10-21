\name{Generalized Pareto Distribution}
\alias{rgpd}
\alias{dgpd}
\alias{qgpd}
\alias{pgpd}

\title{The Generalized Pareto Distribution}
\description{Density, distribution function, quantile function and random
  generation for the GP distribution with location equal to 'loc',
  scale equal to 'scale' and shape equal to 'shape'.}

\usage{
rgpd(n, loc = 0, scale = 1, shape = 0)
pgpd(q, loc = 0, scale = 1, shape = 0, lower.tail = TRUE, lambda = 0)
qgpd(p, loc = 0, scale = 1, shape = 0, lower.tail = TRUE, lambda = 0)
dgpd(x, loc = 0, scale = 1, shape = 0, log = FALSE)
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
  \item{lambda}{a single probability - see the "value" section.}
}
\value{
  If 'loc', 'scale' and 'shape' are not specified they assume the default
  values of '0', '1' and '0', respectively.
  
  The GP distribution function for loc = \eqn{u}{u}, scale =
  \eqn{\sigma}{\sigma} and shape = \eqn{\xi}{\xi} is
  
  \deqn{G(x) = 1 - \left[ 1 + \frac{\xi (x - u )}{ \sigma } \right] ^ { - 1 /
      \xi}}{%
    G(z) = 1 - [ 1 + \xi ( x - u ) / \sigma ]^(-1/\xi)}
  
  for \eqn{1 + \xi ( x - u ) / \sigma > 0}{1 + \xi (x - u) / \sigma > 0}
    and \eqn{x > u}{x > u}, where \eqn{\sigma > 0}{\sigma > 0}. If
    \eqn{\xi = 0}{\xi = 0}, the distribution is defined by continuity
    corresponding to the exponential distribution.

  By definition, the GP distribution models exceedances above a
  threshold. In particular, the \eqn{G}{G} function is a suited
  candidate to model

  \deqn{\Pr\left[ X \geq x | X > u \right] = 1 - G(x)}{Pr[ X >= x | X >
    u ] = 1 - G(x)}
  for \eqn{u}{u} large enough.

  However, it may be usefull to model the "non conditional" quantiles,
  that is the ones related to \eqn{\Pr[ X \leq x]}{Pr[X <= x]}. Using
  the conditional probability definition, one have :

  \deqn{\Pr\left[ X \geq x \right] = \left(1 - \lambda\right) \left( 1 +
    \xi \frac{x - u}{\sigma}\right)^{-1/\xi}}{(1 - \lambda) ( 1 + \xi (x
    - u) /\sigma)_+^(-1/\xi)}
  where \eqn{\lambda = \Pr[ X \leq u]}{\lambda = Pr[X <= u]}.

  When \eqn{\lambda = 0}{\lambda = 0}, the "conditional" distribution
  is equivalent to the "non conditional" distribution. 
}
\examples{
dgpd(0.1)
rgpd(100, 1, 2, 0.2)
qgpd(seq(0.1, 0.9, 0.1), 1, 0.5, -0.2)
pgpd(12.6, 2, 0.5, 0.1)
##for non conditional quantiles
qgpd(seq(0.9, 0.99, 0.01), 1, 0.5, -0.2, lambda = 0.9)
pgpd(2.6, 2, 2.5, 0.25, lambda = 0.5)
}
\keyword{distribution}
