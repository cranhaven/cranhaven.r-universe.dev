\name{plot.pdmod}
\alias{plot.pdmod}
\title{Plot model}
\usage{
  \method{plot}{pdmod} (x, actual, n, ...)
}
\arguments{
  \item{x}{Object of class \code{pdmod}}

  \item{actual}{Actual rewards received}

  \item{n}{(optional) Only plot the last n values}

  \item{...}{Other arguments to \code{\link{plot}}}
}
\description{
  Plots a \code{pdmod} class (what's returned from
  \code{\link{computeModel}} with \code{verbose = TRUE}).
  The plots show the proximal and distal estimates, their
  corresponding uncertainties and weights, as well as the
  overall mean estimate.
}
\examples{
# Create 5 sessions of 20 rewarded trials,
# then 2 sessions of 20 unrewarded trials
trialTime = as.vector(sapply(0:6, function(x) 1:20 + x * TV_DAY))
trials =  TimedVector(c(rep(1, 5*20), rep(0, 2*20)), trialTime)

estimates = computeModel(trials, mFast = 0.7, mSlow = 0.1, n = 0.05,
						 g = 500, h = 0.2, verbose = TRUE)
plot(estimates, trials)
}
\author{
  Chloe Bracis
}

