\name{AICtable}
\alias{AICtable}
\title{
Make a table for AIC or other criterion
}
\description{
Takes the output from a call to AIC or AICc returns a data frame with model likelihoods and model weights.
}
\usage{
AICtable(x, digits=3, sort)
}
\arguments{
  \item{x}{
A data frame with the second column being AIC-type values, as returned by \code{AIC} or \code{AICc}.
}
  \item{digits}{
integer indicating the number of decimal places to retain.
}
  \item{sort}{
logical indicating whether the table should be sorted by AIC; if missing, the rows are sorted if the table has row names.
}
}
\value{
Returns the original data frame with the following extra columns
  \item{Delta }{The difference between each value in x and min(x)}
  \item{ModelLik }{The model likelihood}
  \item{ModelWt }{The model weight}

If sort = TRUE, the rows will be sorted by increasing values of AIC/AICc.
}
\references{
Burnham and Anderson (2002) \emph{Model selection and multimodel inference: a practical information-theoretic approach}, 2 edn. Springer-Verlag.
}
\author{
Mike Meredith
}

\examples{
data(KanhaTigers)
mods <- NULL
mods$M0 <- closedCapM0(KanhaTigers)
mods$Mh2 <- closedCapMh2(KanhaTigers)
mods$MhJK <- closedCapMhJK(KanhaTigers)
mods$Mt <- closedCapMt(KanhaTigers)
AICc <- sapply(mods, AICc)
AICtable(AICc)
# MhJK does not use likelihood maximisation
}

