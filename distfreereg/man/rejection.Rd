\name{rejection}
\alias{rejection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Compute Rejection Rates of a Distribution-Free Test
}
\description{
	Compute the rejection rates of the tests simulated in a \code{compare} object. Specifically, this function estimates the rejection rates of the tests conducted with specified statistics of the hypothesis that the mean function is \code{test_mean} when the true mean function is \code{true_mean}.
}
\usage{
	rejection(object, alpha = 0.05, stat = names(object[["observed_stats"]]), ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	Object of class \code{compare}.
}
  \item{alpha}{
	Numeric vector; specifies the \eqn{\alpha}-levels to use. Passed as \code{probs} argument to \code{\link{quantile}}.
}
  \item{stat}{
	Character vector; specifies the names of the statistics to use. The default value computes the rejection rate associated with every statistic in \code{object}.
}
  \item{...}{
	Additional arguments to pass to \code{\link{quantile}} to estimate the \eqn{1-\alpha} quantiles of the distribution of simulated statistics.
}
}
\value{
	Data frame containing estimated rejection rates and associated Monte Carlo standard errors, with one row for each combination of \code{stat} and \code{alpha} elements.
}
\author{
	Jesse Miller
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\section{Warning}{
  The reported Monte Carlo standard error does not account for the uncertainty of the estimation of the \eqn{1-\alpha} quantiles of the distribution of simulated statistics. The number of Monte Carlo simulations should be large enough to make this estimate sufficiently accurate that it can be considered known for practical purposes. The standard errors of estimated quantiles can be calculated using the \pkg{mcmcse} package.
}

\seealso{
	\code{\link{distfreereg}}, \code{\link{compare}}
}
\examples{
# In practice, set "reps" larger than 200.
set.seed(20240201)
n <- 100
func <- function(X, theta) theta[1] + theta[2]*X
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5)
X <- matrix(rexp(n, rate = 1))
cdfr <- compare(true_mean = func, true_X = X, true_covariance = list(Sigma = Sig),
                test_mean = func, X = X, covariance = list(Sigma = Sig), reps = 200,
                prog = Inf, theta = theta, theta_init = rep(1, length(theta)))

rejection(cdfr)
rejection(cdfr, stat = "CvM")
rejection(cdfr, alpha = c(0.1, 0.2))
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
