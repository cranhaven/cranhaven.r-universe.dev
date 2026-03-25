\name{probitll}
\alias{probitll}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Log-likelihood of the probit model%%  ~~function to do ... ~~
}
\description{
This function implements a direct computation of the logarithm of the likelihood
of a standard probit model \deqn{P(y=1|X,\beta)=
\Phi(\beta^{T}X).}{P(y=1|X,beta)=Phi(X beta).}
}
\usage{
probitll(beta, y, X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{beta}{
coefficient of the probit model%%     ~~Describe \code{beta} here~~
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
returns the logarithm of the probit likelihood for the data \code{y},
covariate matrix \code{X} and parameter vector \code{beta}
}
\seealso{
\code{\link{logitll}}%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(bank)
y=bank[,5]
X=as.matrix(bank[,-5])
probitll(runif(4),y,X)
}
\keyword{probit model}
\keyword{generalised linear model}
