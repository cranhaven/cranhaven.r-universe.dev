\name{vcov.distfreereg}
\alias{vcov.distfreereg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Calculate Covariance Matrices from \code{distfreereg} Objects
}
\description{
	This is a \code{\link{vcov}} method for objects of class \code{distfreereg}. It calculates an estimated covariance matrix of the estimated parameters in a model from a \code{distfreereg} object.
}
\usage{
	\method{vcov}{distfreereg}(object, ..., jacobian_args, hessian_args)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	Object of class \code{distfreereg}.
}
  \item{\dots}{
	Additional parameters passed to other methods when \code{test_mean} element of \code{object} is not of class \code{function}.
}
  \item{jacobian_args, hessian_args}{
	Lists of additional arguments to pass to \code{\link[numDeriv]{jacobian}} and \code{\link[numDeriv]{hessian}}.
}
}
\details{
	When the \code{test_mean} element of \code{object} is of class \code{function}, the covariance matrix is estimated using the method described in section 5.3 of \cite{Van der Vaart (1998)}. Otherwise, \code{test_mean} is of a class that has its own method for \code{\link{vcov}}, which is used to calculate the output.
}
\note{
	If \code{object} was created by calling \code{\link{distfreereg.default}} directly, there is no estimated parameter vector, and therefore \code{vcov.distfreereg} does not apply.
}
\section{Warning}{
	This calculation can be computationally intensive when the sample size is large and \code{object$test_mean} is a function.
}
\value{
	Named numeric matrix equal to the estimated covariance matrix of the parameter estimates from \code{object}.
}
\references{
	Vaart, A. W. \strong{Asymptotic statistics}, 2007, \emph{Cambridge series on statistical and probabilistic mathematics}, Cambridge University Press.
}
\author{
	Jesse Miller
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link{distfreereg}}, \code{\link{confint.distfreereg}}
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
