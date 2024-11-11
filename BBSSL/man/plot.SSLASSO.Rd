\name{plot.SSLASSO}
\alias{plot.SSLASSO}
\title{Plot coefficients from a "SSLASSO" object}
\description{Produces a plot of the coefficient paths for a fitted
  \code{"SSLASSO"} object.}
\usage{
\method{plot}{SSLASSO}(x, ...)
}
\arguments{
  \item{x}{Fitted \code{"SSLASSO"} model.}
  \item{...}{Other graphical parameters to plot.}
}
\references{Rockova, V. and George, E.I. (2018) The Spike-and-Slab LASSO. Journal of the American Statistical Association.}

\seealso{\code{\link{SSLASSO_2}}}
\examples{

## Linear regression, where p>n

n = 100; p = 1000;
truth.beta = c(2, 3, -3, 4); # high-dimensional case
truth.sigma = 1
data = Generate_data(truth.beta, p, n, truth.sigma = 1, rho = 0.6,"all", 4)
y = data$y; X = data$X; beta = data$beta

# --------------- set parameters -----------------
lambda0 = 50; lambda1 = 0.05; lambda = c(lambda0, lambda1)
a = 1; b = p #beta prior for theta


# Separable penalty with fixed theta

result<-SSLASSO_2(X, y,penalty="separable", variance = "fixed",
lambda1 = lambda1, lambda0 = seq(from=lambda1,to=lambda0,length.out=50),
theta = 4/p,initial.beta = rep(0,p))

plot(result)

}
\value{A plot of the coefficient paths for a fitted "SSLASSO" object.}
\keyword{models}
\keyword{regression}
