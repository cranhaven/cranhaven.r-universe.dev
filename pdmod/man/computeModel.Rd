\name{computeModel}
\alias{computeModel}
\title{Calculates proximal/distal model}
\usage{
  computeModel(x, mFast, mSlow, n, g = 0, h,
    tau = 1/TV_DAY, threshold = 0, verbose = TRUE)
}
\arguments{
  \item{x}{Object of class \code{\link{TimedVector}}
  specifying trials including whether signal was
  rewarded/unrewarded and times}

  \item{mFast}{Learning rate of proximal memory estimates}

  \item{mSlow}{Learning rate of distal memory estimates}

  \item{n}{Learning rate of uncertainty estimates}

  \item{h}{Decay rate of distal memory uncertainty
  estimator as time passes between trials}

  \item{g}{Association learning speed parameter}

  \item{tau}{Temporal scaling coefficient to translate time
  differences in \code{x} to fractional days. Defaults to
  \code{1/TV_DAY} assuming that the times in \code{x} are
  expressed in minutes.}

  \item{threshold}{Difference in real time that must pass
  before deflation kicks in (used for testing)}

  \item{verbose}{true to include supporting estimates,
  weights, etc.}
}
\value{
  Series of estimates
}
\description{
  Calulates a realization of a proximal/distal model for a
  specified sequence of trials and paramter values. Use the
  \code{verbose} parameter to include underlying model
  components (distal and proximal estimates, weights,
  uncertainties and signal-reward association) in addition
  to the mean estimate.
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
\seealso{
  \code{\link{calculateResponse}},
  \code{\link{averageBySession}}
}

