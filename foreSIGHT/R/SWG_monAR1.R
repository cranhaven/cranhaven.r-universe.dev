#################################
# Monthly rainfall model based on transformed AR1 model.
# Very similar to Rasmussen's model for daily rainfall (should consolidate later)
#################################

#' @include default_parameters.R

modelInfoList[["P-ann-monAR1"]] <- list(
  simVar = "P",
  timeStep = "1 month",
  simPriority = 1,
  npars = 4,
  parNam = c("mu", "sigma", "phi", "lambda"),
  minBound = c(-1e2, 0.01, -0.7, 0.5),
  maxBound = c(5e2, 5e2, 0.9, 2)
)

modelInfoList[["P-seas-monAR1"]] <- list(
  simVar = "P",
  timeStep = "1 month",
  simPriority = 1,
  npars = 16,
  parNam = c(
    "mu.SON", "mu.DJF", "mu.MAM", "mu.JJA",
    "sigma.SON", "sigma.DJF", "sigma.MAM", "sigma.JJA",
    "phi.SON", "phi.DJF", "phi.MAM", "phi.JJA",
    "lambda.SON", "lambda.DJF", "lambda.MAM", "lambda.JJA"
  ),
  minBound = rep(c(-1e2, 0.01, -0.5, 0.5), each = 4),
  maxBound = rep(c(5e2, 1e3, 0.9, 2), each = 4)
)

modelInfoList[["P-seas1-monAR1"]] <- list(
  simVar = "P",
  timeStep = "1 month",
  simPriority = 1,
  npars = 13,
  parNam = c(
    "mu.SON", "mu.DJF", "mu.MAM", "mu.JJA",
    "sigma.SON", "sigma.DJF", "sigma.MAM", "sigma.JJA",
    "lambda.SON", "lambda.DJF", "lambda.MAM", "lambda.JJA",
    "phi"
  ),
  minBound = c(
    -5e2, -5e2, -5e2, -5e2,
    0.01, 0.01, 0.01, 0.01,
    0.2, 0.2, 0.2, 0.2,
    -0.5
  ),
  maxBound = c(
    5e3, 5e3, 5e3, 5e3,
    5e3, 5e3, 5e3, 5e3,
    1.5, 1.5, 1.5, 1.5,
    0.9
  )
)

modelInfoList[["P-seas2-monAR1"]] <- list(
  simVar = "P",
  timeStep = "1 month",
  simPriority = 1,
  npars = 10,
  parNam = c(
    "mu.SON", "mu.DJF", "mu.MAM", "mu.JJA",
    "sigma.SON", "sigma.DJF", "sigma.MAM", "sigma.JJA",
    "phi","lambda"
  ),
  minBound = c(
    -1e2, -1e2, -1e2, -1e2,
    0.01, 0.01, 0.01, 0.01,
    -0.5,0.2
  ),
  maxBound = c(
    5e3, 5e3, 5e3, 5e3,
    5e3, 5e3, 5e3, 5e3,
    0.9,1.5
  )
)
modelInfoList[["P-har-monAR1"]] <- list(
  simVar = "P",
  timeStep = "1 month",
  simPriority = 1,
  npars = 12,
  parNam = c(
    "mu.m", "mu.amp", "mu.ang",
    "sigma.m", "sigma.amp", "sigma.ang",
    "phi.m", "phi.amp", "phi.ang",
    "lambda.m", "lambda.amp", "lambda.ang"
  ),
  minBound = c(
    -1e2, 0, 0,
    0.01, 0, 0,
    -0.5, 0, 0,
    0.5, 0, 0
  ),
  maxBound = c(
    2e2, 2e2, 6.28,
    2e2, 2e2, 6.28,
    0.9, 1, 6.28,
    2, 1, 6.28
  )
)

# #################################

#' @exportS3Method parManager monAR1
parManager.monAR1 <- function(parS, SWGparameterization, datInd, auxInfo = NULL) {
  parNamesSWG <- c("mu", "sigma", "lambda", "phi")
  if (SWGparameterization == "ann") {
    parTS <- assignAnnualParameters(parNamesSWG = parNamesSWG, parS = parS, datInd = datInd)
  } else if (SWGparameterization == "seas") {
    parTS <- assignSeasonalParameters(parNamesSWG = parNamesSWG, parS = parS, datInd = datInd)
  } else if (SWGparameterization == "seas1") {
    parTS <- assignSeasonalParameters(parNamesSWG = c("mu", "sigma", "lambda"), parS = parS, datInd = datInd)
    parTS <- assignAnnualParameters(parNamesSWG = c("phi"), parS = parS, datInd = datInd, parTS = parTS)
  } else if (SWGparameterization == "seas2") {
    parTS <- assignSeasonalParameters(parNamesSWG = c("mu", "sigma"), parS = parS, datInd = datInd)
    parTS <- assignAnnualParameters(parNamesSWG = c("phi","lambda"), parS = parS, datInd = datInd, parTS = parTS)
  } else if (SWGparameterization == "har") {
    parTS <- assignHarmonicMonthlyParameters(parNamesSWG = parNamesSWG, parS = parS, datInd = datInd)
  }
  parTS$sigma[parTS$sigma < 0] <- 0.
  parTS$phi[parTS$phi < -0.9] <- -0.9
  parTS$phi[parTS$phi > 0.9] <- 0.9

  return(parTS)
}

#################################

#' @import Rcpp
#' @exportS3Method SWGsim monAR1
SWGsim.monAR1 <- function(SWGpar,
                          nTimes,
                          randomTerm,
                          auxInfo = NULL) {
  if (length(SWGpar[["mu"]]) != nTimes) {
    stop("length mu != nTimes")
  }
  if (length(SWGpar[["sigma"]]) != nTimes) {
    stop("length sigma != nTimes")
  }
  if (length(SWGpar[["phi"]]) != nTimes) {
    stop("length phi != nTimes")
  }
  if (length(SWGpar[["lambda"]]) != nTimes) {
    stop("length lambda != nTimes")
  }
  if (length(randomTerm$randomVector) != nTimes) {
    stop("length randomTerm != nTimes")
  }

  sigma <- SWGpar[["sigma"]] * sqrt(1 - SWGpar[["phi"]]^2)
  epsilonT <- stats::qnorm(randomTerm$randomVector) * sigma
  X <- latentX_calc_cpp(SWGpar[["phi"]], epsilonT, nTimes)

  X <- X + SWGpar[["mu"]]

  P <- rep(0, nTimes)
  P[X > 0] <- X[X > 0]^SWGpar$lambda[X > 0]

  if (is.na(sum(P))) {
    browser()
  }

  return(P)
}

#################################
