\name{confint.distfreereg}
\alias{confint.distfreereg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Calculate Confidence Intervals with a \code{distfreereg} Object
}
\description{
	This is a \code{\link{confint}} method for objects of class \code{distfreereg}. It calculates confidence intervals for the estimated parameters of a model in a \code{distfreereg} object.
}
\usage{
	\method{confint}{distfreereg}(object, parm, level = 0.95, ..., digits = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	Object of class \code{distfreereg}.
}
  \item{parm}{
	Numeric or character vector; specifies which parameters are to be given confidence intervals. If missing, all parameters are considered.
}
  \item{level}{
	Numeric vector of length one; specifies the confidence level.
}
  \item{\dots}{
	Additional parameters passed to other methods. Currently ignored.
}
  \item{digits}{
	Numeric vector of length one; used to format percentage labels. Silently converted to an integer.
}
}
\details{
	This is a slight reworking of \code{\link{confint.default}}. The primary difference is that when \code{object$test_mean} is a function, the return value from \code{\link{vcov.distfreereg}} is included in the output, since its calculation can be computationally expensive and this prevents users from needing to call \code{\link{vcov}} separately for its output.
}
\note{
	If \code{object} was created by calling \code{\link{distfreereg.default}} directly, there is no estimated parameter vector, and therefore \code{confint.distfreereg} does not apply.
}
\value{
	If \code{object$test_mean} is not a function, then the output is a named numeric matrix each row of which gives the endpoints of the requested confidence interval of its corresponding parameter. If \code{object$test_mean} is a function, then the output is a named list with the previously defined matrix as its first element and the output of \code{vcov(object)} as its second.
}
\author{
	Jesse Miller
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link{distfreereg}}, \code{\link{vcov.distfreereg}}
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
