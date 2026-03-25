\name{pottshm}
\alias{pottshm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Metropolis-Hastings sampler for a Potts model with \code{ncol} classes%%  ~~function to do ... ~~
}
\description{
This function returns a simulation of a \code{n} by \code{m} grid
from a Potts distribution with \code{ncol} colours and a four neighbour
structure, using a Metropolis-Hastings step that avoids proposing
a value identical to the current state of the Markov chain.%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pottshm(ncol=2,niter=10^4,n,m=n,beta=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ncol}{
number of colors%%%%     ~~Describe \code{niter} here~~
}
  \item{niter}{
number of Metropolis-Hastings iterations%%%%     ~~Describe \code{niter} here~~
}
  \item{n}{
number of rows in the image
}
  \item{m}{
number of columns in the image
}
  \item{beta}{
parameter of the Potts model%%%%     ~~Describe \code{beta} here~~
}
}
\value{
returns a random realisation from the Potts model
}
\seealso{
\code{\link{pottsgibbs}}%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
ex=pottshm(niter=50,n=15,beta=.4)
hist(ex,prob=TRUE,col="steelblue",main="pottshm()")
}
\keyword{Potts model}
\keyword{Gibbs sampling}
\keyword{Metropolis-Hastings algorithm}
