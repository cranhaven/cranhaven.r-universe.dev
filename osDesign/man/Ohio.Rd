\name{Ohio}
\Rdversion{1.7}
\alias{Ohio}
\docType{data}
\title{
Ohio lung cancer data
}
\description{
Population estimates and lung cancer death counts for the state of Ohio in 1988, among 55-84 year olds. Counts are stratified by age, sex and race.
}
\usage{data(Ohio)}
\format{
  A data frame consisting of 12 observations, with the following columns:
  \describe{
    \item{\code{Age}}{A 3-level categorical level variable; 0=55-64 years; 1=65-74 years; 2=75-84 years.}
    \item{\code{Sex}}{A binary variable; 0=male; 1=female.}
    \item{\code{Race}}{A binary variable; 0=white; 1=non-white.}
    \item{\code{N}}{A numeric vector of estimated population counts.}
    \item{\code{Death}}{A numeric vector of lung cancer death counts.}
  }
}
\details{
The data were obtained from the National Center for Heatlh Statistics Compressed Mortality File and correspond to a population of 2,220,177 individuals with 5,533 lung cancer deaths. A more comprehensive dataset, providing counts further stratified by county as well as for the years 1968 to 1988, is described by Xia and Carlin (1998).
}
\source{
Xia H, Carlin B (1998). Spatio-temporal models with errors in covariates: Mapping Ohio lung cancer mortality. Statistics in Medicine, 17, 2025-2043.

Haneuse, S. and Saegusa, T. and Lumley, T. (2011) "osDesign: An R Package for the Analysis, Evaluation, and Design of Two-Phase and Case-Control Studies." Journal of Statistical Software, 43(11), 1-29.
}
\keyword{datasets}
