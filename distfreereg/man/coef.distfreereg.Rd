\name{coef.distfreereg}
\alias{coef.distfreereg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Extract Estimated Parameters from \code{distfreereg} Objects
}
\description{
	This is a \code{\link{coef}} method for objects of class \code{distfreereg}. It extracts the estimated parameters from a model in a \code{distfreereg} object.
}
\usage{
	\method{coef}{distfreereg}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	Object of class \code{distfreereg}.
}
  \item{\dots}{
	Additional parameters passed to or from other methods. Currently ignored.
}
}
\value{
	Numeric vector of estimated model parameters.
}
\author{
	Jesse Miller
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link{distfreereg}}, \code{\link{vcov.distfreereg}}, \code{\link{confint.distfreereg}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
