\name{modelObjectiveFunction}
\alias{modelObjectiveFunction}
\title{Objective function to fit model parameters}
\usage{
  modelObjectiveFunction(pars, dimension, dataX,
    dataResponse, responseFunction = calculateResponse,
    sessionBoundaries = NA, fitG = TRUE)
}
\arguments{
  \item{pars}{Vector of parameters mFast, mSlow, n, hSlow,
  and r}

  \item{dimension}{What dimension to return error in, 1 for
  single criteria optimization, or number of columns of
  data for multicriteria optimization}

  \item{dataX}{List of observations of process x(i) (with
  real time)}

  \item{dataResponse}{Corresponding list of observations of
  subject's response to x(i), i.e. ~x(i)}

  \item{responseFunction}{The function to use to transform
  the forecast into a response}

  \item{sessionBoundaries}{(option) Vector defining how to
  group the trials into sessions where the items are the
  starting indicies for each session (so the last value can
  be the index after the last trial) and \code{NA}s are
  used for gaps between sessions}

  \item{fitG}{\code{TRUE} to estimate g, or \code{FALSE} to
  fix g at 0}
}
\value{
  Error between \code{dataRespones} and what would have
  been estimated for \code{dataX} based on parameters pars
}
\description{
  Function passed to optimization routine to minimize to
  estimate parameters. Uses mean squared error to calculate
  difference between \code{dataResponse} and what
  \code{\link{computeModel}}) would forcast for
  \code{dataX} using parameters \code{pars}.
}
\author{
  Chloe Bracis
}
\seealso{
  \code{\link{computeModel}}, \code{\link{fitModel}}
}

