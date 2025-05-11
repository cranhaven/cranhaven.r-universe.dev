\name{residuals.distfreereg}
\alias{residuals.distfreereg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Extract Residuals from \code{distfreereg} Objects
}
\description{
	This is a \code{\link{residuals}} method for objects of class \code{distfreereg}. It can extract any of the three available types of residuals.
}
\usage{
	\method{residuals}{distfreereg}(object, ..., type = "raw")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	Object of class \code{distfreereg}.
}
  \item{\dots}{
	Additional parameters passed to or from other methods. Currently ignored.
}
  \item{type}{
	Character string specifying the type of residuals to return. Must be one of
	"\code{raw}", "\code{sphered}", and "\code{transformed}".
}
}
\value{
	Numeric vector of residuals.
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
