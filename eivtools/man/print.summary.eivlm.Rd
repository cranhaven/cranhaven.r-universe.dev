\name{print.summary.eivlm}
\alias{print.summary.eivlm}

\title{\code{print} method for objects of class \code{summary.eivlm}.}

\description{ Similar to \code{print} method for summaries of
  \code{\link{lm}} objects, but provides additional information specific
  to the EIV regression.  The \code{summary} method for objects of class
  \code{eivlm} returns an object of class \code{summary.eivlm}.}

\usage{
\method{print}{summary.eivlm}(x,
digits = max(3L, getOption("digits") - 3L),
symbolic.cor = x$symbolic.cor,
signif.stars = getOption("show.signif.stars"),
...)
}

\arguments{
  \item{x}{
    A model object of class \code{summary.eivlm}.
  }
  
  \item{digits, symbolic.cor, signif.stars, \dots}{
    See help for \code{\link{summary.lm}}.
  }

}

\seealso{
  See help for \code{\link{summary.eivlm}} for description of quantities
  relevant to summarizing \code{eivlm} objects.
}

\keyword{methods}

