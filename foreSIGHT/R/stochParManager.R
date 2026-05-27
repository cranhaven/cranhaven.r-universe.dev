#  ################################
# #### STOCHASTIC PAR MANAGER ####
# ################################
#
# #CONTAINS
#   #get.multi.model.info() - get modle info for multiple modelTags
#   #par.manager() - based on model tag converts input vector of pars into pdd, pwd, alpha & beta vectors of nperiod length
#   #init.calib() - initial calibration function. Uses obs data to fit model to baseline climate.
#     #uses model tag to determine what should be fitted (e.g. how many periods, using harmonic, etc.)
#     #gammaParsMLE2() - args(dat, wetThresh)
#     #pdd.pwd.estimator() -args (dat,ind,threshold)
#   #simHarTS.parmanager() - based on model tag converts input vector of pars into cor0, cor1, Hmean, Hsd vectors of period length
#   #whichPars
#   #update.simPriority() -update modelTag via simPriority
# #-------------------------------------------------------------------------------------------------------------
# Get info for multiple models
get.multi.model.info <- function(modelTag = NULL) {
  nMod <- length(modelTag)
  if (nMod == 1) {
    modelInfo <- list()
    modelInfo[[modelTag[1]]] <- get.model.info(modelTag[1]) # even if 1 model still stored in list format
  } else {
    modelInfo <- lapply(X = modelTag, FUN = get.model.info)
    names(modelInfo) <- modelTag
  }
  return(modelInfo)
}


# UPDATE MODEL INFO IF FIXED PARAMETERS
update_model_info <- function(modelTag = NULL, modelInfo = NULL, fixedPars = NULL, minUserBound = NULL, maxUserBound = NULL, file = NULL) {
  # check if model info correct
  # if((modelTag =="P-har12-wgen-FS")){  #only for FS currently
  #   #Check if the correct number of fixed parameters is supplied
  #   if(length(fixedPars)!=4){
  #     logfile("Error: Incorrect number of fixed parameters values provided in fixedPar (4 needed)",file)
  #     logfile("Program terminated",file)
  #     stop("Incorrect number of fixed parameters values provided in fixedPar (4 needed)")
  #   }
  #
  #   temp_modelInfo=get.model.info(modelTag="P-har12-wgen")
  #   modelInfo$parNam=temp_modelInfo$parNam
  #   modelInfo$npars=temp_modelInfo$npars
  #   modelInfo$fixedPars=NA   #update to no fixed pars
  #   #Insert fixed pars into vector quick way (only 1 option uses this so far)
  #   make_minBound=c(modelInfo$minBound[1:2],fixedPars[1],
  #                   modelInfo$minBound[3:4],fixedPars[2],
  #                   modelInfo$minBound[5:6],fixedPars[3],
  #                   modelInfo$minBound[7:8],fixedPars[4])
  #
  #   make_maxBound=c(modelInfo$maxBound[1:2],fixedPars[1],
  #                   modelInfo$maxBound[3:4],fixedPars[2],
  #                   modelInfo$maxBound[5:6],fixedPars[3],
  #                   modelInfo$maxBound[7:8],fixedPars[4])
  #   modelInfo$minBound=make_minBound
  #   modelInfo$maxBound=make_maxBound
  # }

  # USER SPECIFIED BOUNDS CASE
  if ((!is.null(minUserBound)) & (!is.null(maxUserBound))) { #
    # Check if the correct number of fixed parameters is supplied
    if ((length(minUserBound) != modelInfo$npars) | (length(maxUserBound) != modelInfo$npars)) {
      dummy <- paste0("Error: Incorrect length of supplied bounds ", modelInfo$npars, " needed")
      logfile(dummy, file)
      logfile("Program terminated", file)
      stop(dummy)
    } else {
      modelInfo$minBound <- minUserBound
      modelInfo$maxBound <- maxUserBound
    }
  }

  # Otherwise do nothing
  return(modelInfo)
}

# # Anjana : function for ar1 parameter calculation (moved up from modelSequencer.R)
# #-----------------------------------------------------------------------
#
# #Culley 2019 new loop to add ar(1)
# add_ar1Param <- function(modelTag, modelInfo, datInd) {
#
#   for(mod in 1:length(modelTag)){
#     if(modelTag[mod]=="P-har-wgen"){
#       # hard coded parameters
#       ar1ParMult=0.001 # correlation between MULTIPLIER of monthly toals (not same as correlation between monthly totals) was 0.97
#       multRange=0.8 # i.e. 0.1 is +/10%, so multiplier 95% limit is 0.9 to 1.1
#       # translated param values needed for AR1
#       multiplierMean=1
#       sdJumpDistr=multRange/1.96*sqrt(1-ar1ParMult^2)
#       # simulation
#       X=stats::arima.sim(n = datInd[[modelTag[mod]]]$nyr*12, list(ar =ar1ParMult),sd = sdJumpDistr)
#       X=X@.Data+multiplierMean # extract data and add on mean
#       multSim=rep(NA,datInd[[modelTag[mod]]]$ndays)
#       for(iy in 1:datInd[[modelTag[mod]]]$nyr){
#         for(im in 1:12){
#           ind=datInd[[modelTag[mod]]]$i.yy[[iy]][which(datInd[[modelTag[mod]]]$i.yy[[iy]]%in%datInd[[modelTag[mod]]]$i.mm[[im]])]  # to save time this step can be pre-processed into datInd$i.yymm, a list of length 12*nyr
#           multSim[ind]=X[(iy-1)*12+im]
#         }
#       }
#       multSim<-pmax(multSim,0)
#       modelInfo[[modelTag[mod]]]$ar1=multSim
#     } else {
#       if (modelTag[mod]!="Simple-ann") {
#         modelInfo[[modelTag[mod]]]$ar1=NULL
#       }
#     }
#   }
#   return(modelInfo)
# }
# #------------------------------------------------------------------------
# RETURN VARIOUS PARS
return.simPriority <- function(modelInfo = NULL) {
  return(modelInfo$simPriority)
}
return.simVar <- function(modelInfo = NULL) {
  return(modelInfo$simVar)
}

return.simAgg <- function(modelInfo = NULL) {
  return(modelInfo$simAgg)
}


assignSeasPars <- function(par1, par2, par3, par4, seasInd) {
  ndays <- length(unlist(seasInd))
  parAllDays <- rep_len(NA, length.out = ndays)
  parAllDays[seasInd[[1]]] <- par1
  parAllDays[seasInd[[2]]] <- par2
  parAllDays[seasInd[[3]]] <- par3
  parAllDays[seasInd[[4]]] <- par4
  if (any(is.na(parAllDays))) stop("NA values in assigned seasonal parameters.")
  return(parAllDays)
}

# #################################
# functions used in SWG parameter manager

assignSeasonalParameters <- function(parNamesSWG, parS, datInd, parTS = list()) {
  for (par in parNamesSWG) {
    parTS[[par]] <- assignSeasPars(
      parS[paste0(par, ".SON")],
      parS[paste0(par, ".DJF")],
      parS[paste0(par, ".MAM")],
      parS[paste0(par, ".JJA")],
      datInd[["i.ss"]]
    )
  }
  return(parTS)
}


assignHarmonicDailyParameters <- function(parNamesSWG, parS, datInd, parTS = list()) {
  for (par in parNamesSWG) {
    parTS[[par]] <- harmonicFunc(
      x = seq(1:datInd$nTimes),
      mean = parS[paste0(par, ".m")],
      amp = parS[paste0(par, ".amp")],
      phase.ang = parS[paste0(par, ".ang")],
      k = 1, nperiod = 365
    )
  }
  return(parTS)
}

assignHarmonicMonthlyParameters <- function(parNamesSWG, parS, datInd, parTS = list()) {
  for (par in parNamesSWG) {
    parTS[[par]] <- harmonicFunc(
      x = seq(1:datInd$nTimes),
      mean = parS[paste0(par, ".m")],
      amp = parS[paste0(par, ".amp")],
      phase.ang = parS[paste0(par, ".ang")],
      k = 1, nperiod = 12
    )
  }
  return(parTS)
}

assignAnnualParameters <- function(parNamesSWG, parS, datInd, parTS = list()) {
  for (par in parNamesSWG) {
    parTS[[par]] <- rep(parS[par], datInd$nTimes)
  }
  return(parTS)
}


# #################################


whichPars <- function(modelInfo = NULL) {
  parLoc <- list()
  pos.start <- 1
  modelTag <- names(modelInfo)
  for (mod in modelTag) {
    dummyA <- modelInfo[[mod]]$npars
    pos.end <- (dummyA - 1) + pos.start
    tmp <- c(pos.start, pos.end)
    parLoc[[mod]] <- tmp # store in list
    pos.start <- pos.end + 1 # update pos.start ready for next model
  }
  return(parLoc)
}

# Update modelTag order
update_simPriority <- function(modelInfo = NULL) {
  simPriority <- sort(sapply(X = modelInfo, FUN = return.simPriority, USE.NAMES = TRUE)) # get simulation priority of each model
  modelTag <- names(simPriority) # Force simVar="P" to come first via sorting by $simPrority
  return(modelTag)
}
