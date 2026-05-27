#################################
# Ramussen's latent variable daily rainfall model
#################################

#' @include default_parameters.R

modelInfoList[["P-ann-latent"]] <- list(
  simVar = "P",
  timeStep = "1 day",
  simPriority = 1,
  npars = 4,
  parNam = c("alpha", "sigma", "mu", "lambda"),
  minBound = c(0, 0.001, -15, 0.5),
  maxBound = c(0.999, 10, 5, 4)
)

modelInfoList[["P-seas-latent"]] <- list(
  simVar = "P",
  timeStep = "1 day",
  simPriority = 1,
  npars = 16,
  parNam = c(
    "alpha.SON", "alpha.DJF", "alpha.MAM", "alpha.JJA",
    "sigma.SON", "sigma.DJF", "sigma.MAM", "sigma.JJA",
    "mu.SON", "mu.DJF", "mu.MAM", "mu.JJA",
    "lambda.SON", "lambda.DJF", "lambda.MAM", "lambda.JJA"
  ),
  minBound = c(
    0.1, 0.1, 0.1, 0.1,
    0.5, 0.5, 0.5, 0.5,
    -8, -8, -8, -8,
    1, 1, 1, 1
  ),
  maxBound = c(
    0.9, 0.9, 0.9, 0.9,
    8, 8, 8, 8,
    2, 2, 2, 2,
    5, 5, 5, 5
  )
)

modelInfoList[["P-har-latent"]] <- list(
  simVar = "P",
  timeStep = "1 day",
  simPriority = 1,
  npars = 12,
  parNam = c(
    "alpha.m", "alpha.amp", "alpha.ang",
    "sigma.m", "sigma.amp", "sigma.ang",
    "mu.m", "mu.amp", "mu.ang",
    "lambda.m", "lambda.amp", "lambda.ang"
  ),
  minBound = c(
    0, 0, 0,
    0.001, 0, 0,
    -15, 0, 0,
    1, 0, 0
  ),
  maxBound = c(
    0.999, 0, 0,
    10, 5, 15,
    0, 8, 15,
    2, 0, 0
  )
)

# #################################

#' @exportS3Method parManager latent
parManager.latent <- function(parS, SWGparameterization, datInd, auxInfo = NULL) {
  parNamesSWG <- c("alpha", "sigma", "mu", "lambda")

  if (SWGparameterization == "ann") {
    parTS <- assignAnnualParameters(parNamesSWG = parNamesSWG, parS = parS, datInd = datInd)
  } else if (SWGparameterization == "seas") {
    parTS <- assignSeasonalParameters(parNamesSWG = parNamesSWG, parS = parS, datInd = datInd)
  } else if (SWGparameterization == "har") {
    parTS <- assignHarmonicDailyParameters(parNamesSWG = parNamesSWG, parS = parS, datInd = datInd)
  }
  return(parTS)
}

#################################

#' @exportS3Method SWGsim latent
#' @import Rcpp
SWGsim.latent <- function(SWGpar,
                          nTimes,
                          randomTerm,
                          auxInfo = NULL) {
  if (length(SWGpar[["alpha"]]) != nTimes) {
    stop("length alpha != nTimes")
  }
  if (length(SWGpar[["sigma"]]) != nTimes) {
    stop("length sigma != nTimes")
  }
  if (length(SWGpar[["mu"]]) != nTimes) {
    stop("length mu != nTimes")
  }
  if (length(SWGpar[["lambda"]]) != nTimes) {
    stop("length lambda != nTimes")
  }

  if (!is.null(randomTerm$randomUnitNormalVector)) {
    randomUnitNormalVector <- randomTerm$randomUnitNormalVector
  } else {
    randomUnitNormalVector <- stats::qnorm(randomTerm$randomVector)
  }

  if (length(randomUnitNormalVector) != nTimes) {
    stop("length randomUnitNormalVector != nTimes")
  }

  # Calculate latent variable - latentX
  epsilonT <- randomUnitNormalVector * SWGpar$sigma
  X <- latentX_calc_cpp(SWGpar$alpha, epsilonT, nTimes)
  X <- X + SWGpar$mu

  rain <- rep(0, nTimes)
  rain[X > 0] <- X[X > 0]^SWGpar$lambda[X > 0]

  return(rain)
}

#################################
