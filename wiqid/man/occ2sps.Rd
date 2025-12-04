\name{occ2sps}
\alias{occ2sps}

\title{
Single-season two-species occupancy estimation
}

\description{
Estimates occupancy and probability of detection for two species, where one (dominant) species affects the occupancy or detection of the other (subordinate) species (see Richmond et al, 2010). The model has the following parameters:
  \tabular{ll}{
  psiA \tab probability of occupancy of species A \cr
  psiBa \tab probability of occupancy of B if A is absent \cr
  psiBA \tab probability of occupancy of B if A is present \cr
  pA \tab probability of detection of species A if B is absent \cr
  rA \tab probability of detection of species A if both are present \cr
  pB \tab probability of detection of species B if A is absent \cr
  rBa \tab probability of detection of species B if both are present but A was not detected \cr
  rBA \tab probability of detection of species B if both are present and A was detected} }
\usage{
occ2sps(DHA, DHB, model=NULL, data=NULL, ci = 0.95, verify=TRUE)
}
\arguments{
  \item{DHA}{
a 1/0/NA matrix (or data frame) of detection histories, sites x occasions, for the dominant species.
}
  \item{DHB}{
detection histories for the subordinate species in the same format as DHA.
}
  \item{model}{
a list of formulae symbolically defining a linear predictor for any of the parameters in the model. The default, NULL, is equivalent to \code{list(psiA~1, psiBa~1, pA~1, pB~1)}; parameters not included in the list are given the following values: \code{psiBA <- psiBa, rA <- pA, rBa <- pB, rBA <- rBa}.
}
  \item{data}{
a data frame containing the variables in the model. If \code{data = NULL}, a faster algorithm is used, and any covariates in the model will be ignored.
}
  \item{ci}{
the confidence interval to use.
}
  \item{verify}{
if TRUE, the data provided will be checked.
}
}

\value{
Returns an object of class \code{wiqid}, see \link{wiqid-class} for details.
}

\section{Benchmarks}{Output has been checked against output from PRESENCE (Hines 2006) v.5.5 for the \code{\link{railSims}} data set. Real values are  the same to 4 decimal places, and AICs are the same.
}

\references{

Richmond, Hines, Beissinger (2010) Two-species occupancy models: a new parameterization applied to co-occurrence of secretive rails. \emph{Ecological Applications} 20(7):2036-2046

MacKenzie, D I; J D Nichols; A J Royle; K H Pollock; L L Bailey; J E Hines 2006. \emph{Occupancy Estimation and Modeling : Inferring Patterns and Dynamics of Species Occurrence}. Elsevier Publishing.
}
\author{
Mike Meredith
}

\seealso{
See the example data set \code{\link{railSims}}. See \code{\link{occSS}} for single-season single-species occupancy estimation.
}
\examples{
data(railSims)
# Extract the two detection histories
DHA <- railSims[, 1:3]
DHB <- railSims[, 4:6]

# Default model (no interaction)
occ2sps(DHA, DHB)

\donttest{  %%% these can be slow
# Add a submodel for psiBA, so that psiBA and psiBa are separated:
occ2sps(DHA, DHB, model = psiBA ~ 1)

# Add covariates for psiA and psiBA; only display beta coefficients:
occ2sps(DHA, DHB, model = list(psiA ~ logArea, psiBA ~ reeds), data=railSims)$beta

# Model corresponding to the data generation model
occ2sps(DHA, DHB, list(psiA ~ logArea, psiBA ~ reeds, rBA ~ 1), data=railSims)$beta
}
}

