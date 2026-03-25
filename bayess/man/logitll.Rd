\name{logitll}
\alias{logitll}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Log-likelihood of the logit model%%  ~~function to do ... ~~
}
\description{
Direct computation of the logarithm of the likelihood
of a standard logit model (Chapter 4) \deqn{P(y=1|X,\beta)=
\{1+\exp(-\beta^{T}X)\}^{-1}.}{P(y=1|X,beta)=
(1+\exp(-beta X))^(-1).}%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
logitll(beta, y, X)
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
returns the logarithm of the logit likelihood for the data \code{y}, 
covariate matrix \code{X} and parameter vector \code{beta}
}
\seealso{
\code{\link{probitll}}%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(bank)
y=bank[,5]
X=as.matrix(bank[,-5])
logitll(runif(4),y,X)
}
\keyword{logit model}
\keyword{generalised linear model}
