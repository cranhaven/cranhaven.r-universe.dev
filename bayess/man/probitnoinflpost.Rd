\name{probitnoinflpost}
\alias{probitnoinflpost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Log of the posterior density for the probit model under a non-informative model%%  ~~function to do ... ~~
}
\description{
This function computes the logarithm of the posterior density associated
with a probit model and the non-informative prior used in Chapter 4.
}
\usage{
probitnoinflpost(beta, y, X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{beta}{
parameter of the probit model%%     ~~Describe \code{beta} here~~
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
returns the logarithm of the posterior density associated with a
logit model for the data \code{y},
covariate matrix \code{X} and parameter \code{beta}
}
\seealso{
\code{\link{logitnoinflpost}}%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(bank)
y=bank[,5]
X=as.matrix(bank[,-5])
probitnoinflpost(runif(4),y,X)
}
\keyword{probit model}
\keyword{generalised linear model}
\keyword{non-informative prior}
