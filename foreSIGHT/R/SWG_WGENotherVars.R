# #################################
# WGEN type model for variables other than rainfall (e.g. temp, PET)
# Various parameterisations, including conditioning parameters on wet/dry day
# #################################

#' @include default_parameters.R

##########

modelInfoList[["Temp-ann-wgenO"]] <- list(
  simVar = "Temp",
  timeStep = "1 day",
  simPriority = 2,
  npars = 3,
  parNam = c("cor0", "mu", "sigma"),
  minBound = c(0.45, -10, 1),
  maxBound = c(0.9, 40, 20),
  WDcondition = FALSE
)

modelInfoList[["Temp-seas-wgenO"]] <- list(
  simVar = "Temp",
  timeStep = "1 day",
  simPriority = 2,
  npars = 12,
  parNam = c(
    "cor0.SON", "cor0.DJF", "cor0.MAM", "cor0.JJA",
    "mu.SON", "mu.DJF", "mu.MAM", "mu.JJA",
    "sigma.SON", "sigma.DJF", "sigma.MAM", "sigma.JJA"
  ),
  minBound = c(
    0.45, 0.45, 0.45, 0.45,
    -10, -10, -10, -10,
    1, 1, 1, 1
  ),
  maxBound = c(
    0.9, 0.9, 0.9, 0.9,
    40, 40, 40, 40,
    20, 20, 20, 20
  ),
  WDcondition = FALSE
)

modelInfoList[["Temp-har-wgenO"]] <- list(
  simVar = "Temp",
  timeStep = "1 day",
  simPriority = 2,
  npars = 7,
  parNam = c(
    "cor0",
    "mu.m", "mu.amp", "mu.ang",
    "sigma.m", "sigma.amp", "sigma.ang"
  ),
  minBound = c(0.45, 7.0, 1.0, -0.05, 0.9, 0.1, -1.6),
  maxBound = c(0.9, 28.0, 9.0, 0.81, 4.9, 1.4, 3.15),
  WDcondition = FALSE,
  ncycle = 1, nperiod = 26
)

##########

# modelInfoList[["Temp-annWD-wgenO"]] <- list(
#   simVar = "Temp",
#   timeStep = "1 day",
#   simPriority = 2,
#   npars = 3,
#   parNam = c("cor0", "mu.W", "sigma.W", "mu.D", "sigma.D"),
#   minBound = c(0.45, -10, 0.1, -10, 1),
#   maxBound = c(0.95, 40, 20, 40, 20),
#   WDcondition = TRUE, WDthresh = 0
# )

modelInfoList[["Temp-seasWD-wgenO"]] <- list(
  simVar = "Temp",
  timeStep = "1 day",
  simPriority = 2,
  npars = 20,
  parNam = c(
    "cor0.SON", "cor0.DJF", "cor0.MAM", "cor0.JJA",
    "mu.W.SON", "mu.W.DJF", "mu.W.MAM", "mu.W.JJA",
    "sigma.W.SON", "sigma.W.DJF", "sigma.W.MAM", "sigma.W.JJA",
    "mu.D.SON", "mu.D.DJF", "mu.D.MAM", "mu.D.JJA",
    "sigma.D.SON", "sigma.D.DJF", "sigma.D.MAM", "sigma.D.JJA"
  ),
  minBound = c(
    0.45, 0.45, 0.45, 0.45,
    -10, -10, -10, -10,
    0.1, 0.1, 0.1, 0.1,
    10, 10, 10, 10,
    1, 1, 1, 1
  ),
  maxBound = c(
    0.95, 0.95, 0.95, 0.95,
    40, 40, 40, 40,
    20, 20, 20, 20,
    40, 40, 40, 40,
    20, 20, 20, 20
  ),
  WDcondition = TRUE, WDthresh = 0
)

modelInfoList[["Temp-harWD-wgenO"]] <- list(
  simVar = "Temp",
  timeStep = "1 day",
  simPriority = 2,
  npars = 13,
  strat = list(cor0 = "ann", mu = c("har", "wet"), sigma = c("har", "dry")),
  parNam = c(
    "cor0",
    "mu.W.m", "mu.W.amp", "mu.W.ang",
    "sigma.W.m", "sigma.W.amp", "sigma.W.ang",
    "mu.D.m", "mu.D.amp", "mu.D.ang",
    "sigma.D.m", "sigma.D.amp", "sigma.D.ang"
  ),
  minBound = c(
    0.1,
    -10, 0, -3.14,
    0, 0, -3.14,
    10, 0, -3.14,
    0, 0, -3.14
  ),
  maxBound = c(
    0.95,
    40, 20, 3.14,
    20, 20, 3.14,
    40, 20, 3.14,
    20, 20, 3.14
  ),
  WDcondition = TRUE, WDthresh = 0,
  ncycle = 1, nperiod = 26, wdCycle = "All"
)


##########

modelInfoList[["PET-seas-wgenO"]] <- list(
  simVar = "PET",
  timeStep = "1 day",
  simPriority = 2,
  npars = 12,
  parNam = c(
    "cor0.SON", "cor0.DJF", "cor0.MAM", "cor0.JJA",
    "mu.SON", "mu.DJF", "mu.MAM", "mu.JJA",
    "sigma.SON", "sigma.DJF", "sigma.MAM", "sigma.JJA"
  ),
  minBound = c(
    0.45, 0.45, 0.45, 0.45,
    0, 0, 0, 0,
    0.1, 0.1, 0.1, 0.1
  ),
  maxBound = c(
    0.9, 0.9, 0.9, 0.9,
    10, 10, 10, 10,
    2, 2, 2, 2
  ),
  WDcondition = FALSE,
  minVal = 0
)


modelInfoList[["PET-har-wgenO"]] <- list(
  simVar = "PET",
  timeStep = "1 day",
  simPriority = 2,
  npars = 7,
  parNam = c(
    "cor0",
    "mu.m", "mu.amp", "mu.ang",
    "sigma.m", "sigma.amp", "sigma.ang"
  ),
  minBound = c(
    0.1,
    0, 0, 0,
    0, 0, 0
  ),
  maxBound = c(
    0.95,
    10, 5, 15,
    2, 1, 15
  ),
  WDcondition = FALSE,
  minVal = 0,
  ncycle = 1, nperiod = 26
)


##########

modelInfoList[["PET-seasWD-wgenO"]] <- list(
  simVar = "PET",
  timeStep = "1 day",
  simPriority = 2,
  npars = 20,
  parNam = c(
    "cor0.SON", "cor0.DJF", "cor0.MAM", "cor0.JJA",
    "mu.W.SON", "mu.W.DJF", "mu.W.MAM", "mu.W.JJA",
    "sigma.W.SON", "sigma.W.DJF", "sigma.W.MAM", "sigma.W.JJA",
    "mu.D.SON", "mu.D.DJF", "mu.D.MAM", "mu.D.JJA",
    "sigma.D.SON", "sigma.D.DJF", "sigma.D.MAM", "sigma.D.JJA"
  ),
  minBound = c(
    0.45, 0.45, 0.45, 0.45,
    0, 0, 0, 0,
    0.1, 0.1, 0.1, 0.1,
    0, 0, 0, 0,
    0.1, 0.1, 0.1, 0.1
  ),
  maxBound = c(
    0.95, 0.95, 0.95, 0.95,
    10, 10, 10, 10,
    2, 2, 2, 2,
    10, 10, 10, 10,
    2, 2, 2, 2
  ),
  WDcondition = TRUE, WDthresh = 0,
  minVal = 0
)

modelInfoList[["PET-harWD-wgenO"]] <- list(
  simVar = "PET",
  timeStep = "1 day",
  simPriority = 2,
  npars = 13,
  parNam = c(
    "cor0",
    "mu.W.m", "mu.W.amp", "mu.W.ang",
    "sigma.W.m", "sigma.W.amp", "sigma.W.ang",
    "mu.D.m", "mu.D.amp", "mu.D.ang",
    "sigma.D.m", "sigma.D.amp", "sigma.D.ang"
  ),
  minBound = c(
    0.1,
    0, 0, -3.14,
    0, 0, -3.14,
    0, 0, -3.14,
    0, 0, -3.14
  ),
  maxBound = c(
    0.95,
    10, 5, 3.14,
    2, 1, 3.14,
    10, 5, 3.14,
    5, 2, 3.14
  ),
  WDcondition = TRUE, WDthresh = 0,
  minVal = 0,
  ncycle = 1, nperiod = 26, wdCycle = "All"
)


# #################################

#' @exportS3Method parManager wgenO
parManager.wgenO <- function(parS, SWGparameterization, datInd, auxInfo = NULL) {
  if (SWGparameterization == "ann") {
    parTS <- assignAnnualParameters(
      parNamesSWG = c("cor0", "mu", "sigma"), parS = parS,
      datInd = datInd
    )
  } else if (SWGparameterization == "har") {
    parTS <- assignAnnualParameters(parNamesSWG = c("cor0"), parS = parS, datInd = datInd)
    parTS <- assignHarmonicDailyParameters(
      parNamesSWG = c("mu", "sigma"), parS = parS,
      datInd = datInd, parTS = parTS
    )
  } else if (SWGparameterization == "seas") {
    parTS <- assignSeasonalParameters(parNamesSWG = c("cor0", "mu", "sigma"), parS = parS, datInd = datInd)
  # } else if (SWGparameterization == "annWD") {
  #   parTStmp <- assignAnnualParameters(
  #     parNamesSWG = c("cor0", "mu.D", "mu.W", "sigma.D", "sigma.W"), parS = parS,
  #     datInd = datInd
  #   )
  #   # select parameters mu and sigma based on wet-dry days
  #   mu <- parTStmp$muD
  #   mu[auxInfo$wdStatus] <- parTStmp$muW[auxInfo$wdStatus]
  #   sigma <- parTStmp$sigmaD
  #   sigma[auxInfo$wdStatus] <- parTStmp$sigmaW[auxInfo$wdStatus]
  #   parTS <- list(cor0 = parTStmp$cor0, mu = mu, sigma = sigma)
  } else if (SWGparameterization == "seasWD") {
    # setup seasonally varying parameter time series for all 5 parameters (including separate wet-dry day params)
    parTStmp <- assignSeasonalParameters(
      parNamesSWG = c("cor0", "mu.D", "mu.W", "sigma.D", "sigma.W"), parS = parS,
      datInd = datInd
    )
    # change correlation to 0 when switching between wet and dry days
    cor0 <- parTStmp$cor0
    if (is.null(auxInfo$wdStatus)) {
      stop("auxInfo$wdStatus is null")
    }
    # change=c(1,which(abs(diff(auxInfo$wdStatus))==1)+1)
    # cor0[change] = 0
    # select parameters mu and sigma based on wet-dry days
    if (is.null(auxInfo$wdStatus)) {
      stop("auxInfo$wdStatus is null")
    }
    mu <- parTStmp$muD
    mu[auxInfo$wdStatus] <- parTStmp$muW[auxInfo$wdStatus]
    sigma <- parTStmp$sigmaD
    sigma[auxInfo$wdStatus] <- parTStmp$sigmaW[auxInfo$wdStatus]
    parTS <- list(cor0 = cor0, mu = mu, sigma = sigma)
  } else if (SWGparameterization == "harWD") {
    # setup annual parameter for cor0
    parTStmp <- assignAnnualParameters(parNamesSWG = c("cor0"), parS = parS, datInd = datInd)
    # setup harmonic parameter for other
    parTStmp <- assignHarmonicDailyParameters(
      parNamesSWG = c("mu.D", "mu.W", "sigma.D", "sigma.W"), parS = parS,
      datInd = datInd, parTS = parTStmp
    )

    # change correlation to 0 when switching between wet and dry days
    cor0 <- parTStmp$cor0
    if (is.null(auxInfo$wdStatus)) {
      stop("auxInfo$wdStatus is null")
    }
    # change=c(1,which(abs(diff(auxInfo$wdStatus))==1)+1)
    # cor0[change] = 0
    # select parameters mu and sigma based on wet-dry days
    mu <- parTStmp$mu.D
    mu[auxInfo$wdStatus] <- parTStmp$mu.W[auxInfo$wdStatus]

    sigma <- parTStmp$sigma.D
    sigma[auxInfo$wdStatus] <- parTStmp$sigma.W[auxInfo$wdStatus]

    parTS <- list(cor0 = cor0, mu = mu, sigma = sigma)
  }
  return(parTS)
}

#################################

#' @import Rcpp
#' @exportS3Method SWGsim wgenO
SWGsim.wgenO <- function(SWGpar,
                         nTimes,
                         randomTerm,
                         auxInfo = NULL) {
  if (!is.null(randomTerm$randomUnitNormalVector)) {
    randomUnitNormalVector <- randomTerm$randomUnitNormalVector
  } else {
    randomUnitNormalVector <- stats::qnorm(randomTerm$randomVector)
  }

  epsilonT <- randomUnitNormalVector * SWGpar$sigma * sqrt(1 - SWGpar$cor0^2)
  X <- latentX_calc_cpp(SWGpar$cor0, epsilonT, nTimes)
  X <- X + SWGpar$mu

  sim <- X

  minVal <- auxInfo$modelInfo$minVal
  if (!is.null(minVal)) {
    sim[sim < minVal] <- minVal
  }

  return(sim)
}

#################################
