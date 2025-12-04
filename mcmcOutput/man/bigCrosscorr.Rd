\name{bigCrosscorr}
\alias{bigCrosscorr}
\encoding{UTF-8}
\title{
Cross-correlations for MCMC output
}
\description{
When MCMC output has hundreds of monitored nodes, the full cross-correlation matrix produced by \code{cor} is of little use. \code{bigCrossCorr} extracts and reports only those values greater than a given threshold.
}
\usage{
bigCrosscorr(x, big = 0.6, digits = 3)
}
\arguments{
  \item{x}{
an object of any class with MCMC output that can be coerced to class \code{mcmcOutput}.
}
  \item{big}{
only values below -big or above +big will be returned
}
  \item{digits}{
the number of decimal places to return
}
}
\value{
A data frame with 2 columns for the names of parameters and a 3rd column with the cross-correlation, sorted in order of decreasing absolute cross-correlation.
}
\author{
Mike Meredith
}

\seealso{
\code{\link{crosscorrPlot}}.
}
