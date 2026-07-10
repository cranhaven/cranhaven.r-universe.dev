\name{lm.pval}
\alias{lm.pval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to calculate p-values for ordinary multiple linear regression.}
\description{Calculates (classical) p-values for an ordinary multiple linear
  regression in the n > p situation.}
\usage{
lm.pval(x, y, exact = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Design matrix (without intercept).}
  \item{y}{Response vector.}
  \item{exact}{Logical. TRUE if p-values based on t-distribution should
    be calculated. FALSE if normal distribution should be used as
    approximation.}
  \item{...}{Additional arguments to be passed to \code{\link{lm}}.}
}
\details{A model with intercept is fitted but the p-value of the intercept
  is not reported in the output.}
\value{Vector of p-values (not including the intercept).}
\author{Lukas Meier}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{hdi}}}
\examples{
x <- matrix(rnorm(100 * 5), nrow = 100, ncol = 5)
y <- x[,1] * 2 + x[,2] * 2.5 + rnorm(100)
pval <- lm.pval(x, y)
pval
}

\keyword{models}
\keyword{regression}% __ONLY ONE__ keyword per line
