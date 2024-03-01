\name{print.penmodel}
\alias{print.penmodel}

\title{
Print method for \code{penmodel}.
}
\description{
 Prints a summary of parameter estimates of a fitted penetrance model.
}
\usage{
\method{print}{penmodel}(x, digits = max(3, getOption("digits") - 3), \ldots)
}
\arguments{
  \item{x}{
An object class of \code{'penmodel'}, a fitted model by \code{\link{penmodel}} or \code{\link{penmodelEM}} functions.
}
 \item{digits}{Number of significant digits to use when printing.}
 \item{...}{Further arguments passed to or from other methods.}
}
\value{
Prints a short summary of the model and model fit.

Returns an object of class \code{'penmodel'}.
}

\author{
Yun-Hee Choi	
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{penmodel}, \link{penmodelEM}, \link{summary.penmodel}, \link{print.summary.penmodel}, \link{plot.penmodel}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }
