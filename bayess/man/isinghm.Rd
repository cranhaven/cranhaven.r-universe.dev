\name{isinghm}
\alias{isinghm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Metropolis-Hastings for the Ising model%%  ~~function to do ... ~~
}
\description{
This is the Metropolis-Hastings version of the original Gibbs algorithm
on the Ising model (Chapter 8). Its basic step only proposes changes of values at selected
pixels, avoiding the inefficient updates that do not modify the current value
of \code{x}.%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
isinghm(niter, n, m=n,beta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{niter}{
number of iterations of the algorithm%%     ~~Describe \code{niter} here~~
}
  \item{n}{
number of rows in the grid%%     ~~Describe \code{n} here~~
}
  \item{m}{
number of columns in the grid%%     ~~Describe \code{n} here~~
}
  \item{beta}{
Ising parameter%%     ~~Describe \code{beta} here~~
}
}
\value{
\code{x}, a realisation from the Ising distribution as a \code{n} x \code{m} matrix
of 0's and 1's
}
\seealso{
\code{\link{isingibbs}}%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
prepa=runif(1,0,2)
prop=isinghm(10,24,24,prepa)
image(1:24,1:24,prop)
}
\keyword{Ising model}
\keyword{random walk}
\keyword{Metropolis-Hastings}
