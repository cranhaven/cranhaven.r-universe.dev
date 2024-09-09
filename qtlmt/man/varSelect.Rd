\name{varSelect}
\alias{varSelect}
\title{
Variable selection
}
\description{
Select a specific number of variables via variable selection that are optimal for multivariate analysis.
}
\usage{
varSelect(x, group, scope, nv, direction=c("backward","forward"))
}
\arguments{
  \item{x}{
a data matrix/frame. Columns are variables to select from.
}
  \item{group}{
a grouping indicator of observations.
}
  \item{scope}{
which variables (i.e., columns of \code{x}) to select from.
}
  \item{nv}{
how many variables to be selected.
}
  \item{direction}{
forward selection or backward elimination.
}
}

\value{
variables selected in the model.
}
\seealso{
\code{\link{varGroup}} and \code{\link{varStep}}
}

\examples{
data(etrait)
g55<- varGroup(traits, mdat, zscope=55, method="best")
idx<- sample(1:nrow(traits), replace=FALSE)
varSelect(traits[idx,], group=mdat[,55], scope=1:ncol(traits[idx,]),
   nv=length(g55[[1]]))
}

