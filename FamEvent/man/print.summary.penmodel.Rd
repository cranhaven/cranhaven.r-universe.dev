\name{print.summary.penmodel}
\alias{print.summary.penmodel}

\title{
  Print method for \code{summary.penmodel} of a fitted penetrance model.
}
\description{
  Prints a short summary of parameter and penetrance estimates of a \code{'summary.penmodel'} object.
}
\usage{
\method{print}{summary.penmodel}(x, digits = max(3, getOption("digits") - 3),
  signif.stars=TRUE, \ldots)
}
\arguments{
  \item{x}{
    An object class of \code{'summary.penmodel'}, a result of a call to \code{\link{summary.penmodel}}.}
  \item{digits}{Number of significant digits to use when printing.}
  \item{signif.stars}{Logical; if \code{TRUE}, provides stars to hightlight significant p-values. Default is \code{TRUE}.}
  \item{...}{Further arguments passed to or from other methods.}
}

\value{
Prints a summary of parameter estimates, their standard errors, \eqn{t}-statistics and corresponding two-sided \eqn{p}-values and additionally indicates significance stars if \code{signif.stars} is \code{TRUE}.

Also prints penetrance estimates by age 70 specific to gender and mutation-status subgroups along with their standard errors and 95\% confidence intervals.

Returns an object of class \code{'summary.penmodel'}.

}
\author{
  Yun-Hee Choi	
}

\seealso{
    \code{\link{penmodel}, \link{penmodelEM}, \link{print.penmodel}, \link{summary.penmodel}}
}

\keyword{ print }
