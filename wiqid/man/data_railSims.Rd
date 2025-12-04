\name{railSims}
\alias{railSims}
\docType{data}
\title{
Simulated detection/non-detection data for two species of rails
}
\description{
A data set for single-season two-species occupancy modelling. See \code{\link{occ2sps}} for details of these kinds of models.
}
\usage{data("railSims")}
\format{
  A data frame with detection (1) vs non-detection data for 2 species at 160 sites on three occasions.
  \describe{
    \item{A1, A2, A3}{detection histories for the dominant species on 3 occasions}
    \item{B1, B2, B3}{detection histories for the subordinate species on 3 occasions}
    \item{logArea}{a continuous site covariate, standardised to mean 0, sd 1}
    \item{reeds}{a logical site covariate.}
  }
}
\details{
The data come from a simulated scenario with the following parameters:
  \tabular{lll}{
  psiA  \tab = plogis(0 + 2*logArea) \tab = probability of occupancy of species A \cr
  psiBa \tab = 0.77 \tab = probability of occupancy of B if A is absent \cr
  psiBA \tab = plogis(-1 + 2*reeds) \tab = probability of occupancy of B if A is present \cr
  pA \tab = 0.75 \tab = probability of detection of species A if B is absent \cr
  rA \tab = pA \tab = probability of detection of species A if both are present \cr
  pB \tab = 0.80 \tab = probability of detection of species B if A is absent \cr
  rBa \tab = pB \tab = probability of detection of species B if both are present but A was not detected \cr
  rBA \tab = 0.40 \tab = probability of detection of species B if both are present and A was detected}
}
\source{
Simulated data
}
\references{
Richmond, O.M.W., Hines, J.E., & Beissinger, S.R. (2010) Two-species occupancy models: a new parameterization applied to co-occurrence of secretive rails. Ecological Applications, 20, 2036-2046.
}
\examples{
data(railSims)
# Separate the two detection histories
DHA <- railSims[, 1:3]
DHB <- railSims[, 4:6]

# Default model (no interaction)
occ2sps(DHA, DHB)

\donttest{
# Model with full interaction
occ2sps(DHA, DHB, list(psiBA ~ 1, rA ~ 1, rBa ~ 1, rBA ~ 1))

# Model corresponding to the data generation model
occ2sps(DHA, DHB, list(psiA ~ logArea, psiBA ~ reeds, rBA ~ 1), data=railSims)
}
}
\keyword{datasets}
