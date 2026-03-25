\name{hmnoinflogit}
\alias{hmnoinflogit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Metropolis-Hastings for the logit model under a noninformative prior%%  ~~function to do ... ~~
}
\description{
This function runs a Metropolis-Hastings algorithm that produces a sample from the
posterior distribution for the logit model (Chapter 4) coefficient \eqn{\beta}{beta}
associated with a noninformative prior defined in the book.
}
\usage{
hmnoinflogit(niter, y, X, scale)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{niter}{
number of iterations%%     ~~Describe \code{niter} here~~
}
  \item{y}{
binary response variable%%     ~~Describe \code{y} here~~
}
  \item{X}{
matrix of covariates with the same number of rows as \code{y}%%     ~~Describe \code{X} here~~
}
  \item{scale}{
scale of the random walk%%     ~~Describe \code{scale} here~~
}
}
\value{
%%  ~Describe the value returned
sample of \eqn{\beta}{beta}'s as a matrix of size \code{niter} x p, where p is the number
of covariates
}
\seealso{
\code{\link{hmnoinfprobit}}%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(bank)
bank=as.matrix(bank)
y=bank[,5]
X=bank[,1:4]
noinflogit=hmnoinflogit(1000,y,X,1)
par(mfrow=c(1,3),mar=1+c(1.5,1.5,1.5,1.5))
plot(noinflogit[,1],type="l",xlab="Iterations",ylab=expression(beta[1]))
hist(noinflogit[101:1000,1],nclass=50,prob=TRUE,main="",xlab=expression(beta[1]))
acf(noinflogit[101:1000,1],lag=10,main="",ylab="Autocorrelation",ci=FALSE)
}
\keyword{logit model}
\keyword{Metropolis-Hastings algorithm}
\keyword{random walk proposal}
\keyword{noninformative prior}
