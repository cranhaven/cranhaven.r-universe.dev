\name{pbino}
\alias{pbino}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Posterior expectation for the binomial capture-recapture model%%  ~~function to do ... ~~
}
\description{
This function provides an estimation of the number of dippers by a posterior expectation,
based on a uniform prior and the \code{eurodip} dataset, as described in Chapter 5.
}
\usage{
pbino(nplus)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nplus}{
number of different dippers captured%%     ~~Describe \code{nplus} here~~
}
}
\value{
%%  ~Describe the value returned
returns a numerical value that estimates the number of dippers
in the population
}
\seealso{
\code{\link{eurodip}}
}
\examples{
data(eurodip)
year81=eurodip[,1]
nplus=sum(year81>0)
sum((1:400)*pbino(nplus))
}
\keyword{capture-recapture models} 
\keyword{binomial probability}
\keyword{posterior expectation}
