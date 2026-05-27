########################################################
## FUNCTION TO SIMULATE SERIES AND DETERMINE FITNESS ###
########################################################

# CONTAINS
# targetFinder() -
#------------------------------------------------------------------------------------------------
targetFinder <- function(x, # vector of pars (will change in optim)
                         modelInfo,
                         modelTag,
                         # modelEnv=NULL,      # tag to link/identify model
                         attSel, # attributes selected (vector of strings)
                         attPrim, # primary attribute label
                         attInfo, # added info regarding attributes (maybe add in attPrim here)!!!!!!!!!!!!!
                         datInd,
                         obs = NULL,
                         randomVector = NULL,
                         randomUnitNormalVector = NULL,
                         target, # target locations: desired changes in climate to be simulated, in % relative or abs diff to baseline levels (vector)
                         attObs, # observed series attribute values
                         lambda.mult, # lambda multiplier for penalty function
                         simSeed = NULL,
                         auxInfo = NULL,
                         simOut = NULL,
                         resid_ts = NULL,
                         returnThis = "objFunc",
                         obj.func = "SS_absPenalty",
                         ...
                         # Nw=NULL,            # warmup period in days
                         # N=NULL,             # seeds
                         # seed1=NULL,
                         # seed2=NULL,
                         # seed3=NULL
) {
  parS <- x

  timeStep <- obs$timeStep

  # SIMULATE SELECTED VARIABLE USING CHOSEN STOCHASTIC MODEL
  sim <- simClim(
    parS = parS,
    modelTag = modelTag,
    modelInfo = modelInfo,
    datInd = datInd[[aggNameShort[[timeStep]]]],
    randomTerm = list(
      randomVector = randomVector,
      randomUnitNormalVector = randomUnitNormalVector,
      seed = simSeed
    ),
    auxInfo = auxInfo
  )

  if (length(which(is.na(sim))) > 0) {
    score <- -150 # default here
  } else {
    aggList <- unique(attInfo$aggType)
    nagg <- length(aggList)


    varList <- unique(attInfo$varType)

    varAll <- c()
    for (var in varList) {
      tmp <- strsplit(x = var, split = "[/]")[[1]]
      varAll <- c(varAll, tmp)
    }
    varAll <- unique(varAll)

    simVar <- modelInfo$simVar
    data <- list(
      times = datInd[[aggNameShort[[timeStep]]]]$times,
      timeStep = timeStep
    )
    data[[simVar]] <- sim

    addVar <- varAll[which(varAll != simVar)]

    for (v in addVar) {
      data[[v]] <- simOut[[v]]$sim
    }

    sim.att <- aggregate_calculate_attributes(
      data = data,
      attSel = attSel,
      datInd = datInd,
      attInfo = attInfo
    )

    simPt <- unlist(Map(function(type, val, baseVal) simPt.converter.func(type, val, baseVal), attInfo$targetType, sim.att, attObs), use.names = FALSE)

    simPt[is.na(simPt)] <- 999.

    if (length(simPt) != length(unlist(target))) {
      browser()
    }

    if (returnThis == "sim") {
      return(simPt)
    } else if (returnThis == "resid") {
      weights <- rep(1, length(attSel))
      names(weights) <- attSel
      weights[attPrim] <- lambda.mult
      resid <- weights * (simPt - unlist(target))
      asr <- abs(sum(resid))
      if (is.infinite(asr) | is.nan(asr) | is.na(asr)) {
        browser()
      }
      return(resid)
    } else if (returnThis == "objFunc") {
      # GET OBJECTIVE FUNCTION VALUE ()
      score <- objFuncMC(
        attSel = attSel, # vector of selected attributes
        attPrim = attPrim, # any primary attributes
        attInfo = attInfo,
        simPt = simPt,
        target = target,
        obj.func = obj.func, # make this changeable (auto calc lambda)
        lambda = lambda.mult
      )
    }
  }
  return(score)
}
