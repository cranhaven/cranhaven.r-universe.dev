\name{setValues}
\alias{setValues}
\title{Setting current values}
\description{This function sets the current values of a variable for future
  iterations. Only stochastic nodes can be set using this facility, and
  logical nodes are then updated if necessary. }
\usage{
setValues(nodeLabel, values)
}
\arguments{
  \item{nodeLabel}{Character vector of length 1, name of a node in the model.}
  \item{values}{The values to be set, generated, e.g., by \code{\link{infoNodeValues}}.}
}
\details{
Current values of a node can be stored to be used later as initial values.
}
\value{The number of values set.}
\seealso{\code{\link{infoNodeValues}}, \code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}


