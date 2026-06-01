\name{lip}
\alias{lip}
\docType{data}
\title{Scottish Lip Cancer Data}
\description{
Clayton and Kaldor (1987) analyzed observed and expected numbers
of lip cancer cases in the 56 administrative areas of Scotland
with a view to produce a map that would display regional variations
in cancer incidence and yet avoid the presentation of unstable rates for
the smaller areas. The expected numbers had been calculated allowing
for the different age distributions in the areas by using a fixed-effects
multiplicative model; these were regarded for the purpose of analysis as
(Intercept)s based on an external set of standard rates. Presumably the
spatial aggregation is due in large part to the effects of environmental
risk factors. Data were available on the percentage of the work force in
each area employed in agriculture, fishing, or forestry. This covariate
exhibits spatial aggregation paralleling that for lip cancer itself. Because
all three occupations involve outdoor work, exposure to sunlight, the
principal known risk factor for lip cancer, might be the explanation.
}
\usage{data("lip")}
\format{
  A data frame with 56 observations on the following 4 variables.
  \describe{
    \item{\code{y}}{observed number of lip cancer}
    \item{\code{n}}{expected number of lip cancer}
    \item{\code{x}}{percentage of the work force in each area employed in agriculture, fishing, or forestry}
    \item{\code{county}}{county number for 56 areas}
  }
}
\references{
Clayton, D.G. and Kaldor, J. (1987). Empirical bayes estimates of agestandardized
relative risks for use in disease mapping. Biometrics, 43, 671--681.
}

