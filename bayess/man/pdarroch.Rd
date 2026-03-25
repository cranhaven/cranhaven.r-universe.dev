\name{pdarroch}
\alias{pdarroch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Posterior probabilities for the Darroch model%%  ~~function to do ... ~~
}
\description{
This function computes the posterior expectation of the population size for a
two-stage Darroch capture-recapture experiment (Chapter 5) under a uniform prior on 
the range (0,400).
}
\usage{
pdarroch(n1, n2, m2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n1}{
size of the first capture experiment%%     ~~Describe \code{n1} here~~
}
  \item{n2}{
size of the second capture experiment%%     ~~Describe \code{n2} here~~
}
  \item{m2}{
number of recaptured individuals%%     ~~Describe \code{m2} here~~
}
}
\details{
This model can be seen as a conditional version of the two-stage model when
conditioning on both sample sizes \eqn{n_1}{n1} and \eqn{n_2}{n2}.%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
numerical value of the posterior expectation
}
\seealso{
\code{\link{pcapture}}%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
for (i in 6:16) print(round(sum(pdarroch(22,43,i)*1:400)))
}
\keyword{Darroch model}
\keyword{posterior expectation}
\keyword{uniform prior}
