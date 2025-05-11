\name{formula.distfreereg}
\alias{formula.distfreereg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Extract Formulas from \code{distfreereg} Objects
}
\description{
	This is a \code{\link{formula}} method for objects of class \code{distfreereg}. It extracts the formula from a model in a \code{distfreereg} object.
}
\usage{
	\method{formula}{distfreereg}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	Object of class \code{distfreereg}.
}
  \item{\dots}{
	Additional parameters passed to or from other methods. Currently ignored.
}
}
\value{
	Formula extracted from \code{x$test_mean}, or \code{NULL} if such a formula cannot be extracted.
}
\author{
	Jesse Miller
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link{distfreereg}}
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
