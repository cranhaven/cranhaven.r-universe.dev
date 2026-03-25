\name{logitnoinflpost}
\alias{logitnoinflpost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Log of the posterior distribution for the probit model under a noninformative prior%%  ~~function to do ... ~~
}
\description{
This function computes the logarithm of the posterior density associated
with a logit model and the noninformative prior used in Chapter 4.
}
\usage{
logitnoinflpost(beta, y, X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{beta}{
parameter of the logit model%%     ~~Describe \code{beta} here~~
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
returns the logarithm of the logit likelihood for the data \code{y},
covariate matrix \code{X} and parameter \code{beta}
}
\seealso{
\code{\link{probitnoinflpost}}%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(bank)
y=bank[,5]
X=as.matrix(bank[,-5])
logitnoinflpost(runif(4),y,X)
}
\keyword{logit model}
\keyword{generalised linear model}
\keyword{noninformative prior}
