\name{BZdata}
\alias{BZdata}
\docType{data}
\title{Brazilian data}

\description{ The data set is selected by a 10\% random sampling of households in each area from a test demographic census completed in one municipality in Brazil consisting of 140 enumeration districts. This data set includes two centered auxiliary covariates, sampling means, sampling variances and true means for all areas. The contained information is available only at the area level.}
   
\usage{data("BZdata")}
\format{
  A data frame with 140 observations on the following 6 variables.
  \describe{
    \item{\code{ni}:}{ sample size for each district.}
    \item{\code{X1}:}{ respective small area population means of the educational attainment of the head of                          household, centered auxiliary covariate.}
    \item{\code{X2}:}{ respective average number of rooms in households, centered auxiliary covariate}
    \item{\code{S2}:}{ respective sampling variances income of head of the household.}
    \item{\code{y}:}{ respective average mean income of head of the household.}
    \item{\code{truemean}:}{ respective true mean income of head of the household.}
  }
}

\keyword{datasets}