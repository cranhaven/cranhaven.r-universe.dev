\name{mcmcOutput-class}
\alias{mcmcOutput-class}
\alias{mcmcOutput}
\alias{mcmcOutput.default}
\alias{mcmcOutput.mcmc.list}
\alias{mcmcOutput.mcmc}
\alias{mcmcOutput.jagsUI}
\alias{mcmcOutput.bugs}
\alias{mcmcOutput.rjags}
\alias{mcmcOutput.runjags}
\alias{mcmcOutput.matrix}
\alias{mcmcOutput.data.frame}

\title{
Conversion to class \code{mcmcOutput}
}
\description{
Convert output containing MCMC chains to the class \code{mcmcOutput}. The function is generic, with methods for a range of input objects.

\code{print}, \code{\link[mcmcOutput:summary.mcmcOutput]{summary}}, \code{\link[mcmcOutput:plot.mcmcOutput]{plot}} and \code{\link[mcmcOutput:window.mcmcOutput]{window}} methods are available for the class. See also \code{\link{postPlot}}, \code{\link{discrepancyPlot}}, \code{\link{crosscorrPlot}}, \code{\link{postPriorOverlap}}.
}
\usage{
mcmcOutput(object, ...)

\method{mcmcOutput}{default}(object, ...)

\method{mcmcOutput}{mcmc.list}(object, header, ...)

\method{mcmcOutput}{mcmc}(object, header, ...)

\method{mcmcOutput}{jagsUI}(object, header, ...)

\method{mcmcOutput}{bugs}(object, header, ...)

\method{mcmcOutput}{rjags}(object, header, ...)

\method{mcmcOutput}{runjags}(object, header, ...)

\method{mcmcOutput}{matrix}(object, nChains=1, header, ...)

\method{mcmcOutput}{data.frame}(object, nChains=1, header, ...)

}

\arguments{
  \item{object}{
an object containing the MCMC chains; see Details.
}
  \item{header}{
text to use as the header by \code{print} and \code{summary} methods.
}
  \item{nChains}{
the number of chains.
}
  \item{\dots}{
named arguments to be passed to other methods (currently not used).
}
}

\details{
\code{mcmcOutput} objects store the output from MCMC estimation runs in a compact and easily accessible format. Several customised extraction methods are available:

  \tabular{ll}{
  \code{$} \tab  : extracts arrays for individual parameters in the same way as a \code{sims.list}. \cr
  \code{[} with 1 index \tab  : returns a new \code{mcmcOutput} object with the selected node(s). \cr
  \code{[} with 2 indices \tab  : returns the selected row(s) and columns(s). \cr
  \code{[} with 3 indices \tab  : behaves as an iterations x chains x nodes array.}
}

\value{
An object of class \code{mcmcOutput}. This is a matrix with a column for the MCMC chain for each node monitored. The first 2 attributes in the list below must be present, the rest are optional but may be used by \code{print} or \code{summary} methods:

  \tabular{ll}{
  \code{nChains} \tab  the number of chains. \cr
  \code{simsList} \tab  a list specifying which columns correspond to each parameter. \cr
  \code{header} \tab text to be displayed as the first line when the object is printed. \cr
  \code{call} \tab the original function call. \cr
  \code{modelFile} \tab the name of the original model file. \cr
  \code{timeTaken} \tab the time taken in seconds for the MCMC run. \cr
  \code{runDate} \tab an object of class \code{POSIXct} with the date of the MCMC run. }
}

\author{
Mike Meredith.
}

\keyword{methods}

