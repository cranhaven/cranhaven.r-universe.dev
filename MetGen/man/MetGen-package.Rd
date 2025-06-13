\name{MetGen-package}
\alias{MetGen-package}
\alias{MetGen}
\docType{package}
\title{
\packageTitle{MetGen}
}

\description{
An adaptation of the multi-variable stochastic weather generator (SWG) proposed in Rglimclim to perform gap-filling and temporal extension at sub-daily resolution. Simulation is performed based on large scale variables and climatic observation data that could be generated from different gauged stations having geographical proximity. SWG relies on ERA5 reanalyses. Multivariable dependance is taking into account by using the decomposition of the product rule (in statistics) into conditional probabilities. See Farhani et al 2020 https://hal.archives-ouvertes.fr/hal-02554676 
}

\details{
The DESCRIPTION file:
\packageDESCRIPTION{MetGen}
The SWG is based on generalized linear models (GLMs) for each hydro-meteorological variable with a suitable probability distribution (Normal, Gamma or Binomial) and appropriate covariates. Covariates are considered in the GLMs to account for temporal and spatial variability. The inter-variable dependencies are taken into account by including a subset of hydro-meteorological variables (excluding the one being modelled) in the covariates of the GLMs. Large-scale atmospheric variables and deterministic effects (seasonal and diurnal cycles, geographical information and temporal persistence) are also included in the covariates.

\packageIndices{MetGen}
1- This package helps to define the probability distribution and covariates for each hydro-meteorological variables using \code{\link{fit.lasso}} function. 

2- The package can be used either in a gap filling mode in which missing values in observation period are imputed using \code{\link{imputation.lagged}} function, or in a projection mode in which the generator simulates values on a period with no observations to perform temporal extension using \code{\link{projection.lagged}} function. 
}
\author{
\packageAuthor{MetGen}

Maintainer: \packageMaintainer{MetGen}
}

\references{
Chandler, R. 2015. A multisite, multivariate daily weather generator based on Generalized Linear
9 Models. User guide:R package.

Farhani N, Carreau J, Kassouk Z, Mougenot B, Le Page M, et al.. Sub-daily stochastic weather generator based on reanalyses for water stress retrieval in central Tunisia. 2020. (hal-02554676)
}

\seealso{\code{\link{fit.glm}} 
}
