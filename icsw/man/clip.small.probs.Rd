\name{clip.small.probs}
\alias{clip.small.probs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Replace probabilities below threshold with threshold value
}
\description{
Return the provided vector with values smaller than the provided
threshold replaced with that threshold (i.e., clip the probabilities
below a certain value). If the threshold is chosen to match an empirical quantile
then this can be used to implement Winsorizing probabilities from below.
If no threshold is provided, the smallest value greater than zero is used.
}
\usage{
clip.small.probs(x, min.prob = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Vector of probabilities.
}
  \item{min.prob}{
Threshold. Values smaller than \code{min.prob} are replaced with
\code{min.prob}. If not provided, set to the smallest value in
\code{x} greater than 0.
}
}
\details{
Used to avoid small probabilities blowing up in inverse probability weighting.

Produces warnings whenever values are actually replaced.
}
\value{
Vector the same length as \code{x} with, possibly, some entries replaced.
}

\author{
Peter M. Aronow <peter.aronow@yale.edu>, Dean Eckles <icsw@deaneckles.com>, Kyle Peyton <kyle.peyton@yale.edu>
}
\examples{

probs <- seq(0, .01, by = .001)
min(clip.small.probs(probs, .05))

# without min.prob, uses smallest value > 0
min(clip.small.probs(probs))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
