\name{toves}
\alias{toves}
\docType{data}
\title{
Simulated detection/non-detection data for a fictitious species
}
\description{
A data set for single-season occupancy modelling together with habitat covariates. See \code{\link{predictAvg}} for an example of its use.
}
\usage{data("toves")}
\format{
  A data frame with detection (1) vs non-detection data for 2 species at 160 sites on three occasions.
  \describe{
    \item{y1, y2, y3, y4}{ detection histories for 4 occasions}
    \item{x1, x2, x3}{ simulated habitat covariates}
  }
}
\source{
Simulated data
}
\examples{
data(toves)
str(toves)

# Extract detection histories
DH <- toves[, 1:4]

# Fit some models
m.1 <- occSS(DH, psi ~ x1, data=toves)
m.12 <- occSS(DH, psi ~ x1 + x2, data=toves)
m.13 <- occSS(DH, psi ~ x1 + x3, data=toves)
m.123 <- occSS(DH, psi ~ x1 + x2 + x3, data=toves)
m.23 <- occSS(DH, psi ~ x2 + x3, data=toves)
AICtable(AIC(m.1, m.12, m.13, m.123, m.23))
}
\keyword{datasets}
