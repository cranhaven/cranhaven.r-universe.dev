\name{gibbscap1}
\alias{gibbscap1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gibbs sampler for the two-stage open population capture-recapture
model%%  ~~function to do ... ~~
}
\description{
This function implements a regular Gibbs sampler associated with Chapter 5 for a two-stage
capture recapture model with open populations, accounting for the possibility
that some individuals vanish between two successive capture experiments.
}
\usage{
gibbscap1(nsimu, n1, c2, c3, N0 = n1/runif(1), r10, r20)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nsimu}{
number of simulated values in the sample%%     ~~Describe \code{nsimu} here~~
}
  \item{n1}{
first capture population size%%     ~~Describe \code{n1} here~~
}
  \item{c2}{
number of individuals recaptured during the second experiment%%     ~~Describe \code{c2} here~~
}
  \item{c3}{
number of individuals recaptured during the third experiment%%     ~~Describe \code{c2} here~~
}
  \item{N0}{
starting value for the population size%%     ~~Describe \code{N0} here~~
}
  \item{r10}{
starting value for the number of individuals who vanished between the first and second experiments%%     ~~Describe \code{r10} here~~
}
  \item{r20}{
starting value for the number of individuals who vanished between the second and third experiments%%     ~~Describe \code{r20} here~~
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{N }{Gibbs sample of the simulated population size}
  \item{p }{Gibbs sample of the probability of capture}
  \item{q }{Gibbs sample of the probability of leaving the population}
  \item{r1 }{Gibbs sample of the number of individuals who vanished between the first and second experiments}
  \item{r2 }{Gibbs sample of the number of individuals who vanished between the second and third experiments}
%% ...
}
\examples{
res=gibbscap1(100,32,21,15,200,10,5)
plot(res$p,type="l",col="steelblue3",xlab="iterations",ylab="p")
}
\keyword{Gibbs}
\keyword{capture-recapture}
\keyword{open population}
