\name{print.penmodel_c}
\alias{print.penmodel_c}

\title{
Print method for \code{penmodel_c}.
}
\description{
 Prints a summary of parameter estimates of a fitted competing risk penetrance model.
}
\usage{
\method{print}{penmodel_c}(x, digits = max(3, getOption("digits") - 3), \ldots)
}
\arguments{
  \item{x}{
An object class of \code{'penmodel_c'}, a fitted model by \code{\link{penmodel_c}} function.
}
 \item{digits}{Number of significant digits to use when printing.}
 \item{...}{Further arguments passed to or from other methods.}
}
\value{
Prints a short summary of the model and model fit.

Returns an object of class \code{'penmodel_c'}.
}

\author{
Yun-Hee Choi	
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{penmodel_c}, \link{summary.penmodel_c}, \link{print.summary.penmodel_c}, \link{plot.penmodel_c}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }
