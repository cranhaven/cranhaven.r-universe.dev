\name{fitModel}
\alias{fitModel}
\title{Fit model parameters}
\usage{
  fitModel(dataX, dataResponse,
    responseFunction = calculateResponse,
    sessionBoundaries = NA, fitG = TRUE)
}
\arguments{
  \item{dataX}{Object of class \code{\link{TimedVector}}
  specifying trials including whether signal was
  rewarded/unrewarded and times}

  \item{dataResponse}{Corresponding observations of
  subject's response to signal}

  \item{responseFunction}{The function to use to transform
  the mean estimate into a response}

  \item{sessionBoundaries}{(optional) Vector defining how
  to group the trials into sessions where the items are the
  starting indicies for each session (so the last value can
  be the index after the last trial) and \code{NA}s are
  used for gaps between sessions}

  \item{fitG}{\code{TRUE} (default) to estimate g, or
  \code{FALSE} to fix g at 0}
}
\value{
  Model fit
}
\description{
  Estimates parameters for proximal/distal model using
  multi-criteria estimation (\code{\link[=nsga2]{mco}})
}
\author{
  Chloe Bracis
}
\seealso{
  \code{\link{computeModel}}
}

