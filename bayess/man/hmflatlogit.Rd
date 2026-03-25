\name{hmflatlogit}
\alias{hmflatlogit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Metropolis-Hastings for the logit model under a flat prior%%  ~~function to do ... ~~
}
\description{
Under the assumption that the posterior distribution is well-defined,
this Metropolis-Hastings algorithm produces a sample from the
posterior distribution on the logit model coefficient \eqn{\beta}{beta}
under a flat prior. %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hmflatlogit(niter, y, X, scale)
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
scale of the Metropolis-Hastings random walk
}
}
\value{
%%  ~Describe the value returned
The function produces a sample of \eqn{\beta}{beta}'s as a matrix of size \code{niter} x \code{p},
where \code{p} is the number of covariates.
}
\seealso{
\code{\link{hmflatprobit}}%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(bank)
bank=as.matrix(bank)
y=bank[,5]
X=bank[,1:4]
flatlogit=hmflatlogit(1000,y,X,1)
par(mfrow=c(1,3),mar=1+c(1.5,1.5,1.5,1.5))
plot(flatlogit[,1],type="l",xlab="Iterations",ylab=expression(beta[1]))
hist(flatlogit[101:1000,1],nclass=50,prob=TRUE,main="",xlab=expression(beta[1]))
acf(flatlogit[101:1000,1],lag=10,main="",ylab="Autocorrelation",ci=FALSE)
}
\keyword{logit model}
\keyword{Metropolis-Hastings algorithm}
\keyword{random walk proposal}
\keyword{flat prior}
