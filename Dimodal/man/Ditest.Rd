
\newcommand{\opt}{\ifelse{latex}{\code{"#1"}}{\verb{"#1"}}}
\newcommand{\nl}{\ifelse{latex}{ }{\ifelse{html}{ }{ \cr}}}

\name{Ditest}
\alias{Ditest}
\alias{print.Ditest}
\alias{summary.Ditest}
\title{
Class Methods for Ditest Objects
}
\description{
Common functions to handle \opt{Ditest} class results from the Dimodal
feature tests.
}

\usage{
\method{print}{Ditest}(x, \dots)
\method{summary}{Ditest}(object, \dots)
}

\arguments{
\item{x}{an object of class \opt{Ditest}}
\item{object}{an object of class \opt{Ditest}}
\item{\dots}{extra arguments, ignored for all methods}
}

\details{
The values of the individual tests specify the contents of the object.
These functions provide a common formatting of the result.  The print
methods show the test applied and all parameters, as well as the result.
The summary methods show the tested statistic and the resulting probability.
}

\value{
the argument \code{x} or \code{object}, invisible
}

\seealso{
  \code{\link{Dipeak.test}},
  \code{\link{Diflat.test}},
  \code{\link{Dinrun.test}},
  \code{\link{Dirunlen.test}},
  \code{\link{Diexcurht.test}},
  \code{\link{Dipermht.test}}
}

\examples{
ptst <- Dipeak.test(1:4, 250, 0.15, 'bartlett')
ptst
## Summary results.
summary(ptst)
}

\keyword{Ditest}
