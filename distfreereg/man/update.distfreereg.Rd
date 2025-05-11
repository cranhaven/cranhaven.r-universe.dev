\name{update.distfreereg}
\alias{update.distfreereg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Update \code{distfreereg} Objects
}
\description{
	This is an \code{\link{update}} method for objects of class \code{distfreereg}. The method takes advantage of the \code{override} argument of \code{\link{distfreereg}} to prevent unnecessary recalculation of potentially computationally expensive objects.
}
\usage{
	\method{update}{distfreereg}(object, ..., smart = TRUE, envir = parent.frame())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	Object of class \code{distfreereg}.
}
  \item{\dots}{
	Additional named parameters to pass to \code{\link{distfreereg}}.
}
  \item{smart}{
	Logical. If \code{TRUE}, then saved values from \code{object} are passed to \code{\link{distfreereg}} using the \code{override} argument, when they need not themselves be updated. See details.
}
  \item{envir}{
	Environment passed to \code{\link{eval}} when evaluating modified call.
}
}
\details{
	This function updates an object of class \code{distfreereg}. By default, it does so "intelligently" in the sense that it does not unnecessarily recompute elements that are already saved in \code{object}. For example, if a new value for \code{covariance} is not included in \code{...}, then the value of \code{covariance} saved in \code{object} is automatically passed to the new call, preventing recalculating \code{Q}. If a new value of \code{covariance} is specified, then all objects dependent on that (e.g., \eqn{\hat\theta}) are recomputed.
	
	In particular, the simulated samples depend on the data and function only through the number of observations, the covariates (if any), and the dimension of the parameter space of the function. If none of these change, then the updated object reuses the simulated samples from the supplied \code{object}.
	
	The price we pay for this efficiency is a potentially "large" value of \code{call} in the updated object.
}
\value{
	An updated object of class \code{distfreereg}.
}
\author{
	Jesse Miller
}
\note{
  The default behavior of \code{\link{update}} is to create an updated call and then evaluate that call. This means, among other things, that a call to \code{\link{update}} made after one of the arguments in the original call is modified will use the modified version of that argument. This is not always true for \code{update.distfreereg}. Values for the override arguments are drawn from the \code{distfreereg} object itself, not from any objects used as values for the original call to \code{\link{distfreereg}}.

	In general, an object created by \code{update.distfreereg} will therefore not be \code{\link{identical}} to the object created by \code{\link{distfreereg}} using corresponding arguments, because the \code{call} values will differ.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link{distfreereg}}
}
\examples{
set.seed(20240218)
n <- 1e2
func <- function(X, theta) X[,1]^theta[1] + theta[2]*X[,2]
Sig <- runif(n, min = 1, max = 3)
theta <- c(2,5)
X <- matrix(runif(2*n, min = 1, max = 5), nrow = n)
Y <- X[,1]^theta[1] + theta[2]*X[,2] + rnorm(n, sd = sqrt(Sig))
dfr_1 <- distfreereg(Y = Y, X = X, test_mean = func,
                     covariance = list(Sigma = Sig),
                     theta_init = c(1,1))

func_updated <- function(X, theta) X[,1]^theta[1] + theta[2]*X[,2]^2
dfr_2 <- update(dfr_1, test_mean = func_updated)

# The following are identical, since the Monte Carlo simulation did not need to
# be rerun for dfr_2, and was therefore copied from dfr_1.
identical(dfr_1$mcsim_stats, dfr_2$mcsim_stats)
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
