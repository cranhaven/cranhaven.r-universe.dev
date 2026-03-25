\name{loglinll}
\alias{loglinll}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Log of the likelihood of the log-linear model%%  ~~function to do ... ~~
}
\description{
This function provides a direct computation of the logarithm of the likelihood
of a standard log-linear model, as defined in Chapter 4.
}
\usage{
loglinll(beta, y, X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{beta}{
coefficient of the logit model%%     ~~Describe \code{beta} here~~
}
  \item{y}{
vector of binary response variables%%     ~~Describe \code{y} here~~
}
  \item{X}{
covariate matrix%%     ~~Describe \code{X} here~~
}
}
\value{
%%  ~Describe the value returned
returns the logarithmic value of the logit likelihood for the data \code{y},
covariate matrix \code{X} and parameter vector \code{beta}
}
\examples{
X=matrix(rnorm(20*3),ncol=3)
beta=c(3,-2,1)
y=rpois(20,exp(X\%*\%beta))
loglinll(beta, y, X)
}
\keyword{log-linear model}
\keyword{generalised linear model}
