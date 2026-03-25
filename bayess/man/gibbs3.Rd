\name{gibbscap2}
\alias{gibbscap2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gibbs sampling for the Arnason-Schwarz capture-recapture model%%  ~~function to do ... ~~
}
\description{
In the Arnason-Schwarz capture-recapture model (see Chapter 5), individual histories
are observed and missing steps can be inferred upon. For the dataset \code{eurodip},
the moves between regions can be reconstituted. This is the first instance
of a hidden Markov model met in the book.
}
\usage{
gibbscap2(nsimu, z)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nsimu}{
numbed of simulation steps in the Gibbs sampler%%     ~~Describe \code{nsimu} here~~
}
  \item{z}{
data, capture history of each individual, with \code{0} coding non-capture%%     ~~Describe \code{x} here~~
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{p}{Gibbs sample of capture probabilities across time}
  \item{phi}{Gibbs sample of survival probabilities across time and locations}
  \item{psi}{Gibbs sample of interstata movement probabilities across time and locations}
  \item{late}{Gibbs averages of completed histories}
%% ...
}
\examples{
data(eurodip)
res=gibbscap2(10,eurodip[1:100,])
plot(res$p,type="l",col="steelblue3",xlab="iterations",ylab="p")
}
\keyword{Arnason-Schwarz}
\keyword{capture-recapture}
\keyword{hidden Markov model}
\keyword{Gibbs sampling}
