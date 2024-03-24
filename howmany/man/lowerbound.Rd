\name{lowerbound}
\alias{lowerbound}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ The lower bound for the number of correct rejections }
\description{
  Extracts the lower bound for the number of false rejections (as a
  function of the total number of made rejections) from an object of
  class 'howmany'.
}
\usage{
lowerbound(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ an object of class 'howmany' }
}

\value{
  A numerical vector, giving the lower bound for the number of correct
  rejections as a function of the number of made rejections.
}
\author{ Nicolai Meinshausen, \email{nicolai@stat.math.ethz.ch} }

\seealso{  \code{\link{howmany}} and \code{\link{howmany_dependent}} for
  multiple tests with independent and dependent test statistics respectively.
}

\keyword{htest}

