\name{TimedVector}
\alias{TimedVector}
\title{Create a TimedVector}
\usage{
  TimedVector(x, t)
}
\arguments{
  \item{x}{Series of values in event time}

  \item{t}{(optional) Cooresponding real time of events in
  minutes.  Default is an event every minute.}
}
\value{
  TimedVector
}
\description{
  The class TimedVector contains a vector of values in
  event time, as well as when in real time those events
  took place.
}
\examples{
# One session of 20 rewarded trials every minute
TimedVector(rep(1, 20), 1:20)

# Three sessions of rewarded trials, then one session of non-rewarded trials,
# with trials every 2 min and sessions every day
trialTime = as.vector(sapply(0:3, function(x) seq(2, 20, 2) + x * TV_DAY))
TimedVector(c(rep(1, 30), rep(0, 10)), trialTime)

# The above schedule of sessions, but 50\% probability of reward
TimedVector(sample(0:1, 40, replace = TRUE), trialTime)
}
\author{
  Chloe Bracis
}
\seealso{
  \code{\link{Constants}}, \code{\link{isTimedVector}},
  \code{\link{verifyTimedVector}}
}

