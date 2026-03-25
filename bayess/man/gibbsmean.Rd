\name{gibbsmean}
\alias{gibbsmean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gibbs sampler on a mixture posterior distribution with unknown means%%  ~~function to do ... ~~
}
\description{
This function implements a Gibbs sampler for a toy mixture problem (Chapter 6) with
two Gaussian components and only the means unknown, so that
likelihood and posterior surfaces can be drawn.%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gibbsmean(p, datha, niter = 10^4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
first component weight%%     ~~Describe \code{p} here~~
}
  \item{datha}{
dataset to be modelled as a mixture%%     ~~Describe \code{datha} here~~
}
  \item{niter}{
number of Gibbs iterations%%     ~~Describe \code{niter} here~~
}
}
\value{
Sample of \eqn{\mu}{mu}'s as a matrix of size \code{niter} x 2
}
\seealso{
\code{\link{plotmix}}%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
dat=plotmix(plottin=FALSE)$sample
simu=gibbsmean(0.7,dat,niter=100)
plot(simu,pch=19,cex=.5,col="sienna",xlab=expression(mu[1]),ylab=expression(mu[2]))
}
% R documentation directory.
\keyword{Gibbs}
\keyword{mixture of distributions}
