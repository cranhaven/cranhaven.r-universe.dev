\name{ks.test.compare}
\alias{ks.test.compare}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Formally Compare Observed and Simulated Statistics
}
\description{
	This is a \code{\link{ks.test}} method for objects of class \code{compare}. It performs a two-sample Kolmogorov--Smirnov test to compare the observed and simulated statistics in an object of class \code{compare}.
}
\usage{
	\method{ks.test}{compare}(x, ..., stat = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	Object of class \code{compare}.
}
  \item{...}{
	Additional parameters passed to \code{ks.test}.
}
  \item{stat}{
	Character string specifying the statistic on which to run the test.
}
}
\details{
	When \code{stat} is \code{NULL}, the default value is the first statistic appearing in the \code{observed_stats} element of \code{object}.
}
\value{
	A list of the form specified in \code{\link{ks.test}}.
}
\author{
	Jesse Miller
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link{compare}}, \code{\link{distfreereg}}, \code{\link{ks.test}}
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

ks.test(cdfr)
ks.test(cdfr, stat = "CvM")
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
