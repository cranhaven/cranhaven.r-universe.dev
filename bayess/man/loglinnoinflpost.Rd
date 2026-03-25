\name{loglinnoinflpost}
\alias{loglinnoinflpost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Log of the posterior density for the log-linear model under a noninformative prior%%  ~~function to do ... ~~
}
\description{
This function computes the logarithm of the posterior density associated
with a log-linear model and the noninformative prior used in Chapter 4.
}
\usage{
loglinnoinflpost(beta, y, X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{beta}{
parameter of the log-linear model%%     ~~Describe \code{beta} here~~
}
  \item{y}{
binary response variable%%     ~~Describe \code{y} here~~
}
  \item{X}{
covariate matrix%%     ~~Describe \code{X} here~~
}
}
\value{
%%  ~Describe the value returned
returns the logarithm of the logit posterior density for the data \code{y},
covariate matrix \code{X} and parameter vector \code{beta}
}
\details{
This function does not test for coherence between the lengths of \code{y},
\code{X} and \code{beta}, hence may return an error message in case of
incoherence.
}
\examples{
X=matrix(rnorm(20*3),ncol=3)
beta=c(3,-2,1)
y=rpois(20,exp(X\%*\%beta))
loglinnoinflpost(beta, y, X) 
}
\keyword{log-linear model}
\keyword{generalised linear model}
\keyword{noninformative prior}

