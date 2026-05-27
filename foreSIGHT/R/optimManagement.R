#######################################
##   OPTIMIZER MANAGER FUNCTIONS     ##
#######################################

#-----------------------------------------------
# setup environment for saving optimization info 
foreSIGHT_optimizationDiagnosticsEnv <- new.env(parent = emptyenv())

#-----------------------------------------------
# Main optimization called by simulateTarget
# Performs multiple calls to optimization routine to improve performance
multiStartOptim <- function(optimArgs,
                            modelInfo,
                            target,
                            parSuggest = NULL,
                            iRepTarg = NULL,
                            ...) {
  timeStart <- Sys.time()

  rownames(target) <- NULL # make sure that if same targets are used, then we don't perform optimization each time

  ##################################

  xLo <- modelInfo$minBound
  xHi <- modelInfo$maxBound

  fixedPars <- findFixedPars(xLo, xHi)

  fBest <- 9e9

  if (!is.null(parSuggest)) {
    nSugg <- nrow(parSuggest)
  } else {
    nSugg <- 0
  }

  fMulti <- timeMulti <- callsMulti <- convergedMulti <- convergenceCodeMulti <- messageMulti <- c()
  parsMulti <- onBoundsMulti <- matrix(nrow = optimArgs$nMultiStart, ncol = length(xLo))
  fTraceMulti <- callsTraceMulti <- list()


  if (!is.null(optimArgs$seed)) {
    seed1 <- optimArgs$seed
  } else {
    seed1 <- 1
  }

  for (r in 1:optimArgs$nMultiStart) {
    # print(r)

    # optim_num = foreSIGHT_optimizationSeedTrackerEnv$optim_num + 1
    # assign("optim_num",optim_num,envir = foreSIGHT_optimizationSeedTrackerEnv)

    time1 <- Sys.time()
    assign("WG_calls", 0, envir = foreSIGHT_optimizationDiagnosticsEnv)
    assign("fTrace", c(), envir = foreSIGHT_optimizationDiagnosticsEnv)

    if (optimArgs$use_different_seeds) {
      seed <- iRepTarg + (optimArgs$nMultiStart - 1) * 1000
    } else {
      seed <- seed1 + r - 1
    }

    print(paste0("multistart seed = ", seed))

    set.seed(seed) # set the random seed for selecting initial parameter values. note same set of seeds will be used for each target/replicate.

    if (r <= nSugg) {
      x0 <- parSuggest[r, ]
    } else {
      x0 <- xLo + stats::runif(length(xLo)) * (xHi - xLo)
    }

    optInput <- list(
      fixedPars = fixedPars,
      par = x0[fixedPars$fitParLoc],
      seed = seed,
      upper = xHi[fixedPars$fitParLoc],
      lower = xLo[fixedPars$fitParLoc],
      optimArgs = optimArgs,
      modelInfo = modelInfo,
      target = target,
      suggestions = parSuggest
    )

    # call optimization routine for a single multistart
    optOutput <- singleOptim(optInput, ...)


    time2 <- Sys.time()
    timeSingle <- time2 - time1 # optimisation runtime

    onBoundsSingle <- (abs(optOutput$parsSingle - xLo) < 1e-6) | (abs(optOutput$parsSingle - xHi) < 1e-6)

    # calculate best OF value after each function call
    fTrace <- foreSIGHT_optimizationDiagnosticsEnv$fTrace
    if (!is.null(fTrace)) {
      nTrace <- length(fTrace)
      fTraceTmp <- c()
      fTraceBest <- -999
      callsTraceTmp <- 1:nTrace
      for (i in 1:nTrace) {
        if (!is.na(fTrace[i]) & (fTrace[i] > fTraceBest)) {
          fTraceBest <- fTrace[i]
        }
        fTraceTmp[i] <- fTraceBest
      }
      # remove best OF values when they are repeated
      fTraceTrim <- c(fTraceTmp[1])
      callsTraceTrim <- c(callsTraceTmp[1])
      for (i in 2:(nTrace - 1)) {
        if ((fTraceTmp[i] != fTraceTmp[i - 1]) | (fTraceTmp[i] != fTraceTmp[i + 1])) {
          fTraceTrim <- c(fTraceTrim, fTraceTmp[i])
          callsTraceTrim <- c(callsTraceTrim, callsTraceTmp[i])
        }
      }
    } else {
      fTraceTrim <- callsTraceTrim <- NULL
    }

    print(optOutput$fSingle)
    fMulti[r] <- optOutput$fSingle
    parsMulti[r, ] <- optOutput$parsSingle
    timeMulti[r] <- timeSingle
    callsMulti[r] <- foreSIGHT_optimizationDiagnosticsEnv$WG_calls
    onBoundsMulti[r, ] <- onBoundsSingle
    convergedMulti[r] <- optOutput$convergedSingle
    convergenceCodeMulti[r] <- optOutput$convergenceCodeSingle
    messageMulti[r] <- optOutput$messageSingle
    if (optOutput$fSingle < fBest) {
      fBest <- optOutput$fSingle
      parsBest <- optOutput$parsSingle
      optFunctionOutput <- optOutput$outTmp
    }
    fTraceMulti[[r]] <- fTraceTrim
    callsTraceMulti[[r]] <- callsTraceTrim
    if (fBest < optimArgs$OFtol) {
      break()
    }
  }

  timeFin <- Sys.time()
  timeRun <- timeFin - timeStart # optimisation runtime

  noBoundsTol <- 1e-4
  onBounds <- ((abs(as.vector(parsBest) - xLo) < noBoundsTol) |
    (abs(as.vector(parsBest) - xHi) < noBoundsTol)) & (xLo != xHi)

  par <- as.vector(parsBest)
  names(par) <- modelInfo$parNam

  out <- list(
    par = par,
    fitness = as.numeric(fBest),
    seed1 = seed1,
    opt = optFunctionOutput,
    runtime = timeRun,
    onBounds = onBounds,
    fMulti = fMulti,
    parsMulti = parsMulti,
    onBoundsMulti = onBoundsMulti,
    callsMulti = callsMulti,
    timeMulti = timeMulti,
    convergedMulti = convergedMulti,
    convergenceCodeMulti = convergenceCodeMulti,
    messageMulti = messageMulti,
    fTraceMulti = fTraceMulti,
    callsTraceMulti = callsTraceMulti
  )

  return(out)
}

#-----------------------------------------------
# this funciton checks to see if this exact optimization has been performed already 
# (which can occur when multiple variables/models are used)

singleOptim <- function(optInput, ...) {
  optInputAll <- c(optInput, list(...))

  #  foreSIGHT_optimizationInputOutputEnv

  # assign("fTrace",c(foreSIGHT_optimizationInputOutputEnv$fTrace,
  #                   target),
  #        envir = foreSIGHT_optimizationInputOutputEnv)

  IOlist <- foreSIGHT_optimizationInputOutputEnv$IOlist

  n <- length(IOlist)
  if (n > 0) {
    for (i in 1:n) {
      if (identical(optInputAll, IOlist[[i]]$optInputAll)) {
        print("re-using output from identical optimization")
        return(IOlist[[i]]$optOutput)
      }
    }
  }

  IOlist[[n + 1]] <- list(optInputAll = optInputAll)

  fixedPars <- optInput$fixedPars
  par <- optInput$par
  seed <- optInput$seed
  upper <- optInput$upper
  lower <- optInput$lower
  optimArgs <- optInput$optimArgs
  modelInfo <- optInput$modelInfo
  target <- optInput$target
  suggestions <- optInput$suggestions

  convergenceCodeSingle <- messageSingle <- NA

  if (optimArgs$optimizer == "RGN") {
    outTmp <- RGN::rgn(
      simFunc = targetFinderFixPars,
      fixedPars = fixedPars,
      par = par,
      upper = upper,
      lower = lower,
      simTarget = rep(0., length(target)),
      control = optimArgs$RGN.control,
      #                       lambda.mult=optimArgs$lambda.mult,
      modelInfo = modelInfo,
      target = target,
      returnThis = "resid",
      ...
    )

    fSingle <- sqrt(2 * outTmp$value)
    parsSingle <- calcParFixedPars(outTmp$par, fixedPars)
    convergedSingle <- (outTmp$convergence)

    # } else if (optimArgs$optimizer=='CMAES') {
    #
    #   set.seed(seed)
    #   outTmp <- cmaes::cma_es(fn=targetFinderFixPars,
    #                           par = par,
    #                           fixedPars=fixedPars,
    #                           modelInfo=modelInfo,
    #                           target=target,
    #                           # lambda.mult=optimArgs$lambda.mult,
    #                           obj.func=optimArgs$obj.func,
    #                           ...,
    #                           lower = lower,
    #                           upper = upper,
    #                           control=optimArgs$CMAES.control)
    #
    #   fSingle = -outTmp$value
    #   if (is.null(outTmp$par)){
    #     fSingle=9e9
    #     parSingle = NULL
    #   } else {
    #     parsSingle = calcParFixedPars(outTmp$par,fixedPars)
    #   }
    #
    #   convergedSingle = (outTmp$convergence==0)
    #   convergenceCodeSingle = outTmp$convergence
    #
  } else if (optimArgs$optimizer == "GA") {
    outTmp <- GA::ga(
      type = "real-valued",
      fitness = targetFinderFixPars,
      lower = lower,
      upper = upper,
      pcrossover = optimArgs$GA$pcrossover,
      pmutation = optimArgs$GA.args$pmutation,
      maxiter = optimArgs$GA.args$maxiter,
      popSize = optimArgs$GA.args$popSize,
      maxFitness = optimArgs$GA.args$maxFitness,
      run = optimArgs$GA.args$run,
      seed = seed,
      parallel = optimArgs$GA.args$parallel,
      keepBest = optimArgs$GA.args$keepBest,
      suggestions = suggestions,
      monitor = FALSE, # switchback
      fixedPars = fixedPars,
      target = target,
      # lambda.mult=optimArgs$lambda.mult,
      obj.func = optimArgs$obj.func,
      modelInfo = modelInfo,
      ...
    )

    fSingle <- -outTmp@fitnessValue
    parsSingle <- calcParFixedPars(outTmp@solution[1, ], fixedPars)

    convergedSingle <- NA
  } else if (optimArgs$optimizer == "SCE") {
    outTmp <- SoilHyP::SCEoptim(
      FUN = targetFinderFixPars,
      par = par,
      lower = lower,
      upper = upper,
      control = optimArgs$SCE.control,
      fixedPars = fixedPars,
      target = target,
      # lambda.mult=optimArgs$lambda.mult,
      obj.func = optimArgs$obj.func,
      modelInfo = modelInfo,
      ...
    )

    fSingle <- outTmp$value
    parsSingle <- calcParFixedPars(outTmp$par, fixedPars)
    convergedSingle <- (outTmp$convergence == 0)
  } else if (optimArgs$optimizer == "optim.LBFGSB") {
    outTmp <- stats::optim(
      par = par,
      fn = targetFinderFixPars,
      method = "L-BFGS-B",
      lower = lower - 1e-6,
      upper = upper + 1e-6,
      fixedPars = fixedPars,
      target = target,
      control = list(fnscale = -1),
      # lambda.mult=optimArgs$lambda.mult,
      obj.func = optimArgs$obj.func,
      modelInfo = modelInfo,
      ...
    )

    fSingle <- -outTmp$value
    parsSingle <- calcParFixedPars(outTmp$par, fixedPars)
    convergedSingle <- (outTmp$convergence == 0)
    messageSingle <- outTmp$message
    convergenceCodeSingle <- outTmp$convergence
  } else if (optimArgs$optimizer == "NM") {
    outTmp <- dfoptim::nmkb(
      par = par,
      fn = targetFinderFixPars,
      lower = lower,
      upper = upper,
      fixedPars = fixedPars,
      target = target,
      control = optimArgs$NM.control,
      # lambda.mult=optimArgs$lambda.mult,
      obj.func = optimArgs$obj.func,
      modelInfo = modelInfo,
      ...
    )

    fSingle <- -outTmp$value
    parsSingle <- calcParFixedPars(outTmp$par, fixedPars)
    convergedSingle <- (outTmp$convergence == 0)
    messageSingle <- outTmp$message
    convergenceCodeSingle <- outTmp$convergence
  }

  optOutput <- list(
    fSingle = fSingle,
    parsSingle = parsSingle,
    convergedSingle = convergedSingle,
    messageSingle = messageSingle,
    convergenceCodeSingle = convergenceCodeSingle
  )

  IOlist[[n + 1]]$optOutput <- optOutput

  assign("IOlist", IOlist,
    envir = foreSIGHT_optimizationInputOutputEnv
  )

  return(optOutput)
}

#-----------------------------------------------
# FUNCTION TO SCREEN SUGGESTED POPULATIONS (screen outside, screen once)
screenSuggest <- function(modelInfo = NULL,
                          modelTag = NULL,
                          parLoc = NULL, # which pars belong to which model parLoc[[mod]]=c(start,end)
                          suggest = NULL # suggested pars
) {
  nMod <- length(modelTag)
  ind <- NULL
  if (is.vector(suggest)) {
    suggest <- matrix(suggest, nrow = 1)
  }
  for (mod in 1:nMod) {
    parSel <- suggest[, (parLoc[[mod]][1]:parLoc[[mod]][2])] # grab par suggestions related to modelTag running
    if (is.vector(parSel)) {
      parSel <- matrix(parSel, nrow = 1)
    }
    tmpInd <- enforceBounds(
      suggest = parSel, # matrix of suggestions
      minBound = modelInfo[[modelTag[mod]]]$minBound,
      maxBound = modelInfo[[modelTag[mod]]]$maxBound
    )
    # JOIN THE INDICES TOGETHER
    ind <- c(ind, tmpInd)
  }

  # REMOVE INDICES DUPLICATES
  ind <- unique(ind)

  # REMOVE INAPPROPRIATE SUGGESTIONS
  suggest <- suggest[ind, ]
  return(suggest)
}

enforceBounds <- function(suggest = NULL, # matrix of suggestions
                          minBound = NULL,
                          maxBound = NULL) {
  nSuggest <- nrow(suggest)
  ind <- vapply(X = seq(1, nSuggest), FUN = testBound, matPar = suggest, minBound = minBound, maxBound = maxBound, FUN.VALUE = numeric(1))
  ind <- ind[which(!is.na(ind))]
  ind
}

testBound <- function(ind = NULL,
                      matPar = NULL,
                      minBound = NULL,
                      maxBound = NULL) {
  out <- outBound(ind = ind, inVector = matPar[ind, ], minBound = minBound, maxBound = maxBound)
  out
}
# matTest=c(1,2,3,4,4,4,5,5,5,10,11,12);matTest=matrix(matTest,ncol=3,nrow=4,byrow=TRUE)
# minBound=c(1,1,3); maxBound=c(6,8,13)

outBound <- function(ind = NULL, # index of pars being evaluated
                     inVector = NULL,
                     minBound = NULL,
                     maxBound = NULL) {
  npar <- length(minBound) # No. of pars that should be inside bound
  ntest <- length(which((inVector >= minBound) & (inVector <= maxBound)))
  if (ntest == npar) {
    ok <- ind
  } else {
    ok <- NA
  }
  return(ok)
}

#-----------------------------------------------

# Following functions get parameters/obj func in format required for different optimizers 

#-----------------------------------------------

findFixedPars <- function(xLo, xHi) {
  fixParLoc <- fixParVal <- fitParLoc <- c()
  for (i in 1:length(xLo)) {
    if (xLo[i] == xHi[i]) {
      fixParLoc <- c(fixParLoc, i)
      fixParVal <- c(fixParVal, xLo[i])
    } else {
      fitParLoc <- c(fitParLoc, i)
    }
  }
  return(list(fixParLoc = fixParLoc, fixParVal = fixParVal, fitParLoc = fitParLoc))
}

#-----------------------------------------------

calcParFixedPars <- function(x, fixedPars) {
  if (!is.null(fixedPars$fixParLoc)) {
    xAll <- c()
    xAll[fixedPars$fixParLoc] <- fixedPars$fixParVal
    xAll[fixedPars$fitParLoc] <- x
  } else {
    xAll <- x
  }
  #  browser()
  return(xAll)
}

#-----------------------------------------------

targetFinderFixPars <- function(x, fixedPars = NULL, returnThis = "objFunc", ...) {
  # deal with fixed and fitted pars
  xAll <- calcParFixedPars(x, fixedPars)
  target <- targetFinder(x = xAll, returnThis = returnThis, ...)
  # save obj func value to vector
  if (returnThis == "objFunc") {
    assign("fTrace", c(
      foreSIGHT_optimizationDiagnosticsEnv$fTrace,
      target
    ),
    envir = foreSIGHT_optimizationDiagnosticsEnv
    )
  } else if (returnThis == "resid") {
    assign("fTrace", c(
      foreSIGHT_optimizationDiagnosticsEnv$fTrace,
      -sqrt(sum(target^2))
    ),
    envir = foreSIGHT_optimizationDiagnosticsEnv
    )
  }
  return(target)
}

#-----------------------------------------------

negTargetFinder <- function(x, ...) {
  target <- -targetFinder(x = x, ...)
  return(target)
}

#-----------------------------------------------

negTargetFinderFixPars <- function(x, fixedPars = NULL, ...) {
  target <- -targetFinderFixPars(x = x, fixedPars = fixedPars, ...)
  return(target)
}


