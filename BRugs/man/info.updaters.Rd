\name{infoUpdaters}
\alias{infoUpdatersbyName}
\alias{infoUpdatersbyDepth}
\title{Information on MCMC updaters}
\description{List the MCMC sampling algorithms in use by the current model.}
\usage{
infoUpdatersbyName()
infoUpdatersbyDepth()
}
\value{
  A data frame listing the MCMC updating algorithms chosen for each
  stochastic node in the model after the model has been compiled.

  For block updating algorithms, the first component in the block is
  shown followed by the other components of the block in angle brackets.
  For vector nodes, only the first element is shown.

  \code{infoUpdatersbyName} sorts the nodes alphabetically.

  \code{infoUpdatersbyDepth} sorts the nodes in their reverse topological
  order in the graphical model. Nodes which are forward sampled have a
  negative depth.
}
\seealso{\code{\link{infoNodeMethods}},\code{\link{BRugs}},
\code{\link{help.WinBUGS}}} \keyword{interface}
