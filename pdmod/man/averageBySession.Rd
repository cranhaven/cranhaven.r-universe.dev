\name{averageBySession}
\alias{averageBySession}
\title{Average by session}
\usage{
  averageBySession(estimate, sessionBoundaries)
}
\arguments{
  \item{estimate}{Series of estimates in event time}

  \item{sessionBoundaries}{Vector of the starting indices
  for each session (which means to include the end, the
  last value should be length(estimate) + 1)}
}
\value{
  Vector of average estimate for each session
}
\description{
  Calculates the average estimate per session or block of
  trials
}
\examples{
# Create vector of values (i.e. estimates, respones, etc.)
values = runif(100)
# Specify sessions, here a group of 10 trials
sessionBoundaries = seq(1, 101, 10)
valuesBySession = averageBySession(values, sessionBoundaries)
}
\author{
  Chloe Bracis
}

