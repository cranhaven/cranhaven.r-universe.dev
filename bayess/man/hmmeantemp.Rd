\name{hmmeantemp}
\alias{hmmeantemp}
\title{
Metropolis-Hastings with tempering steps for the mean mixture posterior model
}
\description{
This function provides another toy illustration of the capabilities
of a tempered random walk Metropolis-Hastings algorithm applied
to the posterior distribution associated with a two-component normal mixture with only its means
unknown (Chapter 7). It shows how a decrease in the temperature leads to a proper exploration of
the target density surface, despite the existence of two well-separated modes.
}
\usage{
hmmeantemp(dat, niter, var = 1, alpha = 1)
}
\arguments{
  \item{dat}{set to be modelled as a mixture}
  \item{niter}{number of iterations}
  \item{var}{variance of the random walk}
  \item{alpha}{temperature, expressed as power of the likelihood}
}
\details{
When \eqn{\alpha=1}{alpha=1} the function operates (and can be used) as a regular Metropolis-Hastings algorithm.
}
\value{
sample of \eqn{\mu_i}{mu}'s as a matrix of size \code{niter} x 2
}
\examples{
dat=plotmix(plot=FALSE)$sample
simu=hmmeantemp(dat,1000)
plot(simu,pch=19,cex=.5,col="sienna",xlab=expression(mu[1]),ylab=expression(mu[2]))
}
\keyword{Metropolis-Hastings}
\keyword{GLM}
\keyword{mixture of distributions}
