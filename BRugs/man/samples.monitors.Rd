\name{samplesMonitors}
\alias{samplesMonitors}
\title{Names of monitored scalar variables}
\description{This function returns names of monitored scalar variables.}
\usage{
samplesMonitors(node)
}
\arguments{
  \item{node}{Character vector of length 1, name of a variable in the model, or simply \sQuote{\code{*}}.
      \code{node} can be a vector quantity with sub ranges given to indices (e.g. \code{samplesMonitors("node[3:5]")}).}
}
\value{
A list of names that are monitored.
If sampling a vector of parameters of \code{node}, all elements are printed, e.g.:
\code{"node[beg]", ..., "node[end]"}.
}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}

