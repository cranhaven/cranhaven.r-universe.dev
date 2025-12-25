\name{gumbel}
\alias{dgumbel}
\alias{pgumbel}
\alias{qgumbel}
\alias{rgumbel}
\title{The Gumbel Distribution and Derivatives}
\description{
  Density function, distribution function, quantile function and
  random generation, and their gradient functions 
  for the Gumbel distribution with location and scale parameters. 
}
\usage{
dgumbel(x, location=0, scale=1, log = FALSE, grad=FALSE) 
pgumbel(q, location=0, scale=1, lower.tail = TRUE, log.p = FALSE, grad=FALSE) 
qgumbel(p, location=0, scale=1, lower.tail = TRUE, grad=FALSE)
rgumbel(n, location=0, scale=1)
}
\arguments{
  \item{x, q}{Vector of quantiles.}
  \item{p}{Vector of probabilities.}
  \item{n}{Number of observations.}
  \item{location, scale}{Location and scale parameters.}
  \item{log, log.p}{Logical; if \code{TRUE}, probabilities p are given as log(p).}
  \item{lower.tail}{Logical; if \code{TRUE} (default), probabilities
    are P[X <= x], otherwise, P[X > x]}
  \item{grad}{Logical; if \code{TRUE}, the gradient w.r.t. parameters location and scale is given instead of function value.}
}
\details{
  The Gumbel distribution function with parameters
  \eqn{\code{location} = a} and \eqn{\code{scale} = b} is
  \deqn{G(z) = \exp\left\{-\exp\left[-\left(\frac{z-a}{b}\right)
    \right]\right\}}{G(x) = exp{-exp[-(z-a)/b]}}
  for all real \eqn{z}, where \eqn{b > 0}.
  Gradients are exact numerical derivatives implemented using automatic differentiation.
  \code{dgumbel} builds on the \code{Eigen} linear algebra library, \code{Adept} for automatic differentiation and \code{RcppEigen} for bindings to \code{R} and loading \code{Eigen}.
}
\value{
  \code{dgumbel} gives the density function, \code{pgumbel} gives
  the distribution function, \code{qgumbel} gives the quantile
  function, and \code{rgumbel} generates random deviates. 
  If \code{grad=TRUE} is supplied, then the gradient is returned instead of the objective function.
}

\examples{
dgumbel(-1:2, -1, 0.5)
pgumbel(-1:2, -1, 0.5)
qgumbel(seq(0.9, 0.6, -0.1), 2, 0.5)
rgumbel(6, -1, 0.5)
p <- (1:9)/10
pgumbel(qgumbel(p, -1, 2), -1, 2)
## [1] 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9

## Random number generation
loc = .5
scale = 3.2
n <- 1000
x <- rgumbel(n, loc, scale)

## The density
hist(x, freq=FALSE)
xs <- sort(x)
fx <- dgumbel(xs, loc, scale)
points(xs,fx, type="l", col=2, lwd=2)

## The distribution
edf <- sapply(xs, function(x){sum(xs<=x)/n})
plot(xs, edf)
Fx <- pgumbel(xs, loc, scale)
points(xs, Fx, type="l", col=2, lwd=2) 

## The quantile function
q <- qgumbel(0.6, loc, scale)
polygon(c(xs[xs <= q], q), c(Fx[xs<=q], 0), col=3)

## Negative log likelihood: Objective and gradient
nll <- function(par, data) -sum(dgumbel(data, par[1], par[2], log=TRUE))
dnll <- function(par, data) -rowSums(dgumbel(data, par[1], par[2], log=TRUE, grad=TRUE))

## Parameter estimation
par_start <- c(3,1)
opt <- nlminb(par_start, objective=nll, gradient=dnll, data=x, control = list(trace=5))
opt$convergence
opt$par

}
\keyword{distribution}