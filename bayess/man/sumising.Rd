\name{sumising}
\alias{sumising}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Approximation by path sampling of the normalising constant 
for the Ising model%%  ~~function to do ... ~~
}
\description{
This function implements a path sampling approximation of the 
normalising constant of an Ising model with a four neighbour relation.
}
\usage{
sumising(niter = 10^3, numb, beta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{niter}{
number of iterations%%     ~~Describe \code{niter} here~~
}
  \item{numb}{
size of the square grid for the Ising model%%     ~~Describe \code{numb} here~~
}
  \item{beta}{
Ising model parameter%%     ~~Describe \code{beta} here~~
}
}
\value{
%%  ~Describe the value returned
returns a vector of 21 values for \eqn{Z(\beta)} corresponding to a regular sequence
of \eqn{\beta}{beta}'s between 0 and 2
}
\seealso{
\code{\link{isingibbs}},\code{\link{isinghm}}
}
\examples{
Z=seq(0,2,length=21)
for (i in 1:21)
  Z[i]=sumising(5,numb=24,beta=Z[i])
lrcst=approxfun(seq(0,2,length=21),Z)
plot(seq(0,2,length=21),Z,xlab="",ylab="")
curve(lrcst,0,2,add=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Ising model}
\keyword{path sampling}
