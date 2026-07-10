\name{lm.ci}
\alias{lm.ci}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to calculate confidence intervals for ordinary multiple
  linear regression.}
\description{Calculates (classical) confidence intervals for an ordinary
  multiple linear regression model in the n > p situation.}
\usage{
lm.ci(x, y, level = 0.95, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Design matrix (without intercept).}
  \item{y}{Response vector.}
  \item{level}{Coverage level.}
  \item{...}{Additional arguments to be passed to \code{\link{lm}}.}
}
\details{A model with intercept is fitted but the p-value of the intercept
  is not reported in the output.}
\value{Matrix of confidence interval bounds (not including the intercept).}
\author{Lukas Meier}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{hdi}}}
\examples{
x <- matrix(rnorm(100 * 5), nrow = 100, ncol = 5)
y <- x[,1] * 2 + x[,2] * 2.5 + rnorm(100)
ci <- lm.ci(x, y)
ci
}

\keyword{models}
\keyword{regression}% __ONLY ONE__ keyword per line
