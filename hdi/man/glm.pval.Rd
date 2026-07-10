\name{glm.pval}
\alias{glm.pval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to calculate p-values for a generalized linear model.}
\description{Calculates (classical) p-values for an ordinary generalized
  linear model in the n > p situation.}
\usage{
glm.pval(x, y, family = "binomial", verbose = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Design matrix (without intercept).}
  \item{y}{Response vector.}
  \item{family}{As in \code{\link{glm}}.}
  \item{verbose}{Logical. Should information be printed out if algorithm
    did not converge?}
  \item{...}{Additional arguments to be passed to \code{\link{glm}}.}
}
\details{A model with intercept is fitted but the p-value of the intercept
  is not reported in the output.}
\value{Vector of p-values (not including the intercept).}
\author{Lukas Meier}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{hdi}}}
\examples{
## ...
}

\keyword{models}
\keyword{regression}% __ONLY ONE__ keyword per line
