\name{plot.howmany}
\alias{plot.howmany}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plot of an object of class 'howmany' }
\description{
  Displays information stored in objects of class \code{howmany}
}
\usage{
\method{plot}{howmany}(x,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{howmany} }
  \item{...}{ additional arguments, currently un-implemented }
}
\details{
  There are four plots.

  The upper-left plot displays the empirical
  distribution function of p-values along with the linear
  function and the
  bounding function at the chosen level alpha (empirical distributions
  functions, where only true null hypotheses are present, are below the
  bounding function with high probability 1-alpha).

  The upper-right plot displays the same information as the upper-left
  plot, yet with the linear component subtracted.

  The lower-left plot shows the lower bound for the number of correct
  rejections as a function of the number of rejected hypotheses.

  The lower-right plot shows the lower bound for the number of correct
  rejections divided by the number of made rejections.
  
}

\seealso{ \code{\link{howmany}} and \code{\link{howmany_dependent}};
  \code{\link{summary.howmany}} }

\keyword{htest}
