#################################

parManager <- function(parS, SWGparameterization, datInd, auxInfo = NULL) {
  UseMethod("parManager", parS)
}

SWGsim <- function(SWGpar, nTimes, randomTerm, auxInfo = NULL) {
  UseMethod("SWGsim", SWGpar)
}

#################################

simClim <- function(parS, # vector of pars (will change in optim)
                    modelTag,
                    modelInfo,
                    datInd,
                    randomTerm = NULL,
                    auxInfo = NULL) {
  SWGmodel <- strsplit(modelTag, "-")[[1]][[3]]
  SWGparameterization <- strsplit(modelTag, "-")[[1]][[2]]

  # timeStep = modelInfoList[[modelTag]]$timeStep
  timeStep <- modelInfo$timeStep
  if (is.null(timeStep)) {
    timeStep <- auxInfo$obs$timeStep
  }

  #  names(parS) = modelInfoList[[modelTag]]$parNam

  if (length(parS) != length(modelInfo$parNam)) {
    stop(paste0("parS has length ", length(parS), " but modelInfo$parNam has length ", length(modelInfo$parNam)))
  }

  names(parS) <- modelInfo$parNam
  class(parS) <- SWGmodel

  # SWGpar = parManager(parS = parS, SWGparameterization = SWGparameterization,
  #                     datInd=datInd[[aggNameShort[[timeStep]]]],
  #                     auxInfo=auxInfo)

  SWGpar <- parManager(
    parS = parS, SWGparameterization = SWGparameterization,
    datInd = datInd,
    auxInfo = auxInfo
  )

  class(SWGpar) <- SWGmodel

  # sim = SWGsim(SWGpar = SWGpar,
  #                nTimes = datInd[[aggNameShort[[timeStep]]]]$nTimes,
  #                randomTerm = randomTerm,
  #             auxInfo=auxInfo)

  sim <- SWGsim(
    SWGpar = SWGpar,
    nTimes = datInd$nTimes,
    randomTerm = randomTerm,
    auxInfo = auxInfo
  )

  WG_calls <- foreSIGHT_optimizationDiagnosticsEnv$WG_calls + 1
  assign("WG_calls", WG_calls, envir = foreSIGHT_optimizationDiagnosticsEnv)

  simVar <- modelInfo$simVar
  ppTypes <- modelInfo$ppTypes
  for (pp in ppTypes) {
    #    sim = runPP(sim=sim,obs=auxInfo$obs[[simVar]],PPname=pp,parS=parS,
    #                datInd=datInd[[aggNameShort[[timeStep]]]],randomTerm=randomTerm)
    sim <- runPP(
      sim = sim, obs = auxInfo$obs[[simVar]], PPname = pp, parS = parS,
      datInd = datInd, randomTerm = randomTerm
    )
  }

  return(sim)
}

#################################
