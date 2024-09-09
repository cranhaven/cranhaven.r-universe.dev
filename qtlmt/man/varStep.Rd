\name{varStep}
\alias{varAdd1}
\alias{varDrop1}
\alias{varStep}
\title{
Variable selection
}
\description{
Add a variable, drop a variable, or select a subset of variables via variable selection that are optimal for multivariate analysis.
}
\usage{
varAdd1(x, group, vin=NULL, scope=1:ncol(x), k=0)

varDrop1(x, group, vin=1:ncol(x), k=0)

varStep(x, group, scope, k, kf=k/2, direction=c("both",
   "forward","backward"))
}
\arguments{
  \item{x}{
a data matrix/frame. Columns are variables to select from.
}
  \item{group}{
a grouping indicator of observations.
}
  \item{vin}{
which variables (i.e., columns of \code{x}) already in model. It defines the initial model.
}
  \item{scope}{
which variables (i.e., columns of \code{x}) to select from.
}
  \item{k}{
entry/stay value in backward stepwise.
}
  \item{kf}{
entry/stay value in forward stepwise.
}
  \item{direction}{
forward selection, backward elimination or both stepwise.
}
}

\value{
which variable to add (add1), which variable to drop (drop1), or a subset of variables in the final model (step).
}

\seealso{
\code{\link{varSelect}}
}
\examples{
data(etrait)
varAdd1(traits, group=mdat[,42], vin=10, scope=1:ncol(traits))
varStep(traits, group=mdat[,42], k=12, scope=1:ncol(traits),
   direction="back")
}

