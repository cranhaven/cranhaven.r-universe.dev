\name{infoNode}
\alias{infoNodeValues}
\alias{infoNodeMethods}
\alias{infoNodeTypes}
\title{Node information}
\description{List current values, data types and samplers corresponding to a variable.}
\usage{
infoNodeValues(nodeLabel)
infoNodeMethods(nodeLabel)
infoNodeTypes(nodeLabel)
}
\arguments{
  \item{nodeLabel}{Character vector of length 1, name of a variable in the model.}
}
\value{
  \code{infoNodeValues} returns a vector of the current (last sampled)
  values of a variable.

  \code{infoNodeMethods} returns a data frame listing the method used
  internally by OpenBUGS to sample values from the full conditional
  distribution of the node. 

  \code{infoNodeTypes} returns a data frame listing the OpenBUGS data
  type which represents each node internally. For example, stochastic
  nodes with normal priors are of type \code{GraphNormal.StdNode}. 
}
\seealso{\code{\link{setValues}}, \code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
