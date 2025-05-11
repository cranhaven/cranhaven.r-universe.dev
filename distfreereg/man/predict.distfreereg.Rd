\name{predict.distfreereg}
\alias{predict.distfreereg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Generate Predicted Values from \code{distfreereg} Objects
}
\description{
	This is a \code{\link{predict}} method for objects of class \code{distfreereg}.
}
\usage{
	\method{predict}{distfreereg}(object, ..., newdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	Object of class \code{distfreereg}.
}
  \item{\dots}{
	Additional parameters affecting the predictions produced. Currently ignored.
}
  \item{newdata}{
	Optional matrix or data frame of new covariate values. If missing, the fitted values are returned.
}
}
\details{
	When \code{object$test_mean} is of class "\code{lm}" or "\code{nls}", \code{object$test_mean} is sent to \code{\link{predict}} for method dispatch. When \code{object$test_mean} is of class "\code{formula}", \code{object$model} is sent to \code{\link{predict}}.
}
\value{
	Numeric vector of predicted values.
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
