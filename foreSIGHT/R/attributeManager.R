#######################################
##      ATTRIBUTE MANAGER            ##
#######################################

# CONTAINS
# functions of form "func_XXX", used to calculate attributes
# attribute.calculator() - calculate values of attributes
# attribute.calculator.setup() - calculate arguments used in attribute.calculator() based on attribute names
# attribute.info.check() - get targetType, varType and identify any invalid model selections
# get.attribute.info() - Identify invalid models
# get.target.type() - treated as fractions, percent or abs value
# get.attribute.varType() - "P", "Temp" OR ...


#---------------------------------------------------------------------------------------

####################################
# Functions used to calculate attributes.
# Each has input of "data" and optional "attArgs"
# Note that custom functions starting with "func_" can also be specified

#' Calculates total of time series
#' @param data is a vector, representing a time series
#' @export
func_tot <- function(data) sum(data, na.rm = T)

#' Calculates seasonality ratio
#' @param data is a vector, representing a time series
#' @param attArgs is a list, with attArgs$indexSeas corresponding to season of interest
# seasonality ratio
#' @export
func_seasRatio <- function(data, attArgs) {
  Pseas <- sum(data = data[attArgs$indexSeas])
  Pseas <- max(Pseas, 0.001)
  Pall <- sum(data = data)
  if (Pall == 0.) {
    seasRatio <- 100.
  } else {
    seasRatio <- Pseas / Pall
  }

  # Prest = Pall-Pseas
  # seasRatio = Pseas/Prest

  return(seasRatio)
}

# func_fracTot = function(data,attArgs){
#   Pseas = func_tot(data)
#   fracTot = Pseas/attArgs$tot
#   return(fracTot)
# }
#
# func_fracNwet = function(data,attArgs){
#   nWetSeas = func_nWet(data,attArgs)
#   fracNwet = nWetSeas/attArgs$nWetTot
#   return(fracNwet)
# }
#
# func_fracCor = function(data,attArgs){
#   corSeas = func_cor(data)
#   fracCor = corSeas/attArgs$corTot
#   return(fracCor)
# }
#
# func_fracWDcor = function(data,attArgs){
#   WDcorSeas = func_WDcor(data)
#   fracWDcor = WDcorSeas/attArgs$WDcorTot
#   return(fracWDcor)
# }
#
#
# func_fracP99 = function(data,attArgs){
#   P99Seas = quantile(data,probs = 0.99,na.rm=T,names=F)
#   fracP99 = P99Seas/attArgs$P99Tot
#   return(fracP99)
# }
#
# func_fracxP99overPave = function(data,attArgs){
#   xP99overPaveSeas = func_xP99overPave(data)
#   fracxP99overPave = xP99overPaveSeas/attArgs$xP99overPave
#   if(is.na(fracxP99overPave)){browser()}
#   return(fracxP99overPave)
# }

# #  function for calulcating ratio of P99 to average rainfall
# func_xP99overPave = function(data){
#   m = mean(data,na.rm=T)
#   if (m==0){
#     xP99overPave = 1e3
#   } else {
#     xP99overPave = quantile(data,0.99,names=F,na.rm=T)/mean(data,na.rm=T)
#   }
#   return(xP99overPave)
# }
#
# #  function for calulcating ratio of P90 to average rainfall
# func_xP90overPave = function(data){
#   m = mean(data,na.rm=T)
#   if (m==0){
#     xP90overPave = 1e3
#   } else {
#     xP90overPave = quantile(data,0.9,na.rm=T,names=F)/mean(data,na.rm=T)
#   }
#   return(xP90overPave)
# }
#
# #  function for calulcating ratio of P99.9 to average rainfall
# func_xP99.9overPave = function(data){
#   m = mean(data,na.rm=T)
#   if (m==0){
#     xP99.9overPave = 1e3
#   } else {
#     xP99.9overPave = quantile(data,0.999,na.rm=T,names=F)/mean(data,na.rm=T)
#   }
#   return(xP99.9overPave)
# }

#' Calculates the average dwell time, i.e. average time for below median value spells
#' @param data is a vector, representing a time series
#' @export
func_avgDwellTime <- function(data) {
    spell.lengths <- get.spell.lengths(
      data = data,
      thresh = stats::median(data),
    type = "dry"
  )
  return(mean(spell.lengths))
}

#' Calculates number of wet days (above threshold)
#' @param data is a vector, representing a time series
#' @param attArgs is a list, with attArgs$threshold denoting the threshold
#' @export
func_nWet <- function(data, attArgs) get.nwet(data = data, threshold = attArgs$threshold)

#' Calculates maximum dry spell duration (below threshold)
#' @param data is a vector, representing a time series
#' @param attArgs is a list, with attArgs$threshold denoting the threshold
#' @export
func_maxDSD <- function(data, attArgs) get.spell.lengths.max(data = data, thresh = attArgs$threshold, type = "dry")

#' Calculates maximum wet spell duration (above threshold)
#' @param data is a vector, representing a time series
#' @param attArgs is a list, with attArgs$threshold denoting the threshold
#' @export
func_maxWSD <- function(data, attArgs) get.spell.lengths.max(data = data, thresh = attArgs$threshold, type = "wet")

#' Calculates average dry spell duration (below threshold)
#' @param data is a vector, representing a time series
#' @param attArgs is a list, with attArgs$threshold denoting the threshold
#' @export
func_avgDSD <- function(data, attArgs) base::mean(get.spell.lengths(data = data, thresh = attArgs$threshold, type = "dry"), na.rm = TRUE)

#' Calculates average wet spell duration (below threshold)
#' @param data is a vector, representing a time series
#' @param attArgs is a list, with attArgs$threshold denoting the threshold
#' @export
func_avgWSD <- function(data, attArgs) base::mean(get.spell.lengths(data = data, thresh = attArgs$threshold, type = "wet"), na.rm = TRUE)

#' Calculates average rainfall on wet days (above threshold)
#' @param data is a vector, representing a time series
#' @param attArgs is a list, with attArgs$threshold denoting the threshold
#' @export
func_dyWet <- function(data, attArgs) get.wet.average(data = data, threshold = attArgs$threshold)

#' Calculates the number of days above a threshold (often used for temperature)
#' @param data is a vector, representing a time series
#' @param attArgs is a list, with attArgs$threshold denoting the threshold
#' @export
func_R <- function(data, attArgs) get.nwet(data = data, threshold = attArgs$threshold)

#' Calculates a quantile value
#' @param data is a vector, representing a time series
#' @param attArgs is a list, with attArgs$quant denoting the probability of the quantile
#' @export
func_P <- function(data, attArgs) get.quantile(data = data, quant = attArgs$quant)

#' Calculates normalised quantile (quantile divided by mean)
#' @param data is a vector, representing a time series
#' @param attArgs is a list, with attArgs$quant denoting the probability of the quantile
#' @export
func_normP <- function(data, attArgs) {
  m <- base::mean(data, na.rm = T)
  if (m == 0) {
    normP <- 1e3
  } else {
    normP <- get.quantile(data = data, quant = attArgs$quant) / m
  }
  return(normP)
}

#' Calculates average of time series
#' @param data is a vector, representing a time series
#' @export
func_avg <- function(data) mean(data, na.rm = T)

# inter-quantile range
#' Calculates the inter-quantile range
#' @param data is a vector, representing a time series
#' @param attArgs is a list, with attArgs$lim denoting the probability limit width
#' @export
func_rng <- function(data, attArgs) get.quantile.rng(data = data, lim = attArgs$lim)

# Calculates the growing season length
# @param data is a vector, representing a time series
# @export
# NOTE: currently note exported since this is not generalised 
# (e.g. uses fixed threshold of 5 deg C, and moving window of 6 days) 
func_GSL <- function(data) GSLcalc(x = data)

# Calculates the cold season length
# @param data is a vector, representing a time series
# @export
# NOTE: currently note exported since this is not generalised (e.g. uses fixed threshold of 17 deg C) 
func_CSL <- function(data) CSLcalc(x = data)

#' Calculates the number of frost days
#' @param data is a vector, representing a time series
#' @export
func_F0 <- function(data) F0calc(x = data) # could be made generic

#' Calculates the day of year corresponding to the wettest 6 months
#' @param data is a vector, representing a time series
#' @param attArgs is a list, with attArgs$doy denoting the day of year for each value in the time series
#' @export
func_wettest6monPeakDay <- function(data, attArgs = NULL) {
  seas <- attArgs$seas
  i <- stats::median(which(seas == max(seas)))
}

#' Calculates the lag-1 autocorrelation for wet days
#' @param data is a vector, representing a time series
#' @export
func_WDcor <- function(data) {
  N <- length(data)
  data[data == 0] <- NA
  diff <- data[2:N] - data[1:(N - 1)]
  if (length(which(!is.na(diff))) < 2) {
    WDcor <- 1e3
  } else {
    WDcor <- stats::cor(data[1:(N - 1)], data[2:N], use = "pairwise.complete.obs")
  }
  if (is.na(WDcor)) {
    cor <- 1e3
  }
  return(WDcor)
}

#' Calculates the lag-1 autocorrelation
#' @param data is a vector, representing a time series
#' @export
func_cor <- function(data) {
  N <- length(data)
  if (sum(data[1:(N - 1)], na.rm = T) == 0 | sum(data[2:N], na.rm = T) == 0) {
    cor <- 1e3
  } else {
    cor <- suppressWarnings(stats::cor(data[1:(N - 1)], data[2:N], use = "pairwise.complete.obs"))
  }
  if (is.na(cor)) {
    cor <- 1e3
  }
  return(cor)
}

#' Calculates the coefficient of variation (mead/sd)
#' @param data is a vector, representing a time series
#' @export
func_cv <- function(data) {
  m <- base::mean(data, na.rm = T)
  if (m == 0) {
    cv <- 9999.
  } else {
    cv <- stats::sd(data, na.rm = T) / m
  }
  return(cv)
}
#' Calculates the ratio of wet season to dry season rainfall, based on wettest6monPeakDay
#' @param data is a vector, representing a time series
#' @param attArgs is a list, with attArgs$doy denoting the day of year for each value in the time series
#' @export
func_wettest6monSeasRatio <- function(data, attArgs = NULL) {
  seas <- attArgs$seas
  iwet <- stats::median(which(seas == max(seas)))
  idry <- stats::median(which(seas == min(seas)))
  seas_iwet <- seas[iwet]
  seas_idry <- seas[idry]
  wettest6monSeasRatio <- seas_iwet / seas_idry
  if ((seas_idry == 0.) | (wettest6monSeasRatio > 100.)) {
    wettest6monSeasRatio <- 100.
  }
  return(wettest6monSeasRatio)
}

func_ma3P99 <- function(data) {
  ma3 <- movingAverage(data, n = 3, centered = T)
  ma3P99 <- stats::quantile(ma3, 0.99, na.rm = T, names = F)
  return(ma3P99)
}

###############


#' Calculates the correlation between two time series 
#' @param data.1 a vector for the first climate variable
#' @param data.2 a vector for the second climate variable
#' @export
mvFunc_cor <- function(data.1, data.2) {
  return(stats::cor(data.1, data.2, use = "pairwise.complete.obs"))
}

#' Calculates the average value of a non-rainfall time series on wet-days   
#' @param data.1 represents a non-rainfall time series
#' @param data.2 represents rainfall time series
#' @export
mvFunc_avgWetDay <- function(data.1, data.2) {
  return(base::mean(data.1[data.2 > 0], na.rm = T))
}

mvFunc_sdWetDay <- function(data.1, data.2) {
  return(stats::sd(data.1[data.2 > 0], na.rm = T))
}

#' Calculates the average value of a non-rainfall time series on dry-days   
#' @param data.1 represents a non-rainfall time series
#' @param data.2 represents rainfall time series
#' @export
mvFunc_avgDryDay <- function(data.1, data.2) {
  return(base::mean(data.1[data.2 == 0], na.rm = T))
}

mvFunc_sdDryDay <- function(data.1, data.2) {
  return(stats::sd(data.1[data.2 == 0], na.rm = T))
}

func_sd <- function(data) {
  return(stats::sd(data, na.rm = T))
}

func_xP90 <- function(data) {
  P90 <- stats::quantile(data, probs = 0.9, na.rm = T, names = F)
  return(P90)
}

mvFunc_xP90WetDay <- function(data.1, data.2) {
  return(stats::quantile(data.1[data.2 > 0], probs = 0.9, na.rm = T, names = F))
}

mvFunc_xP90DryDay <- function(data.1, data.2) {
  return(stats::quantile(data.1[data.2 == 0], probs = 0.9, na.rm = T, names = F))
}

# func_cv <- function(data) {
#   m <- base::mean(data, na.rm = T)
#   if (m == 0) {
#     cv <- 9999.
#   } else {
#     cv <- stats::sd(data, na.rm = T) / m
#   }
#   return(cv)
# }

#' Calculates the coefficient of variation (sdev/mean) value of a non-rainfall time series on wet-days   
#' @param data.1 represents a non-rainfall time series
#' @param data.2 represents rainfall time series
#' @export
mvFunc_cvWetDay <- function(data.1, data.2) {
  return(func_cv(data.1[data.2 > 0]))
}

#' Calculates the coefficient of variation (sdev/mean) value of a non-rainfall time series on dry-days   
#' @param data.1 represents a non-rainfall time series
#' @param data.2 represents rainfall time series
#' @export
mvFunc_cvDryDay <- function(data.1, data.2) {
  return(func_cv(data.1[data.2 == 0]))
}

###############

# for each doy calculate which dates have that doy, store results in matrix
calc_keepMat <- function(doy) {
  keepMat <- matrix(nrow = 365, ncol = length(doy) / 365)
  for (n in 1:365) {
    keepMat[n, ] <- which(doy == n)
  }
  return(keepMat)
}

calc_mean_day_clim <- function(obs, keepMat) {
  mean_day_clim <- c()
  for (i in 1:365) {
    mean_day_clim[i] <- mean(obs[keepMat[i, ]])
  }
  return(mean_day_clim)
}

# Calculates the seasonal pattern (i.e. climatological mean)
#' @import Rcpp
calc_meanClimDaily_dayOfYearWindow <- function(obs, # vector representing a time series
                                               dates = NULL,
                                               keepMat = NULL, # matrix indexing days of year
                                               inc) # the half-window size used in moving average
{
  if (is.null(keepMat)) {
    doy <- as.integer(format(dates, "%j"))
    keepMat <- calc_keepMat(doy)
  }
  mean_day_clim <- calc_mean_day_clim_cpp(obs, keepMat)
  indicesRM <- c((365 - inc + 1):365, 1:365, 1:inc)
  # run_mean_day_clim = ma(mean_day_clim[indicesRM],n=(2*inc+1))
  run_mean_day_clim <- movingAverage(x = mean_day_clim[indicesRM], n = (2 * inc + 1), centered = T)
  return(run_mean_day_clim[(inc + 1):(inc + 365)])
}

# # Calculates the seasonal pattern (i.e. climatological mean) over all dates (not just single year)
# calc_meanClimDaily_dayOfYearWindow_allDates = function(obs,  # vector representing a time series
#                                               dates,
#                                               inc) #the half-window size used in moving average
# {
#
#   obsClim = calc_meanClimDaily_dayOfYearWindow (obs=obs,dates = dates,inc = inc)
#   obsClim[366] = obsClim[365]
#   doy = as.integer(format(dates,'%j'))
#   obsClimAll = obsClim[doy]
#
#   return(obsClimAll)
# }

####################################
# ATTRIBUTE CALCULATOR FUNCTION
attribute.calculator <- function(attSel = NULL, # list of evaluated attribute names
                                 data = NULL, # timeseries data
                                 datInd = NULL, # dat indices and properties (e.g. datInd$nyr, datInd$i.yy)
                                 attInfo = NULL # optional saved list of attribute information (from attribute.calculator.setup)
) {
  if (!is.null(attInfo$attCalcInfo)) {
    attCalcInfo <- attInfo$attCalcInfo
  } else {
    attCalcInfo <- attribute.calculator.setup(attSel, datInd)
  }

  if (any(c("P_day_all_wettest6monSeasRatio", "P_day_all_wettest6monPeakDay") %in% attSel)) {
    seas <- calc_meanClimDaily_dayOfYearWindow(obs = data, keepMat = attCalcInfo[["P_day_all_wettest6monSeasRatio"]]$attArgs$keepMat, inc = 91)
    attCalcInfo[["P_day_all_wettest6monSeasRatio"]]$attArgs$seas <- attCalcInfo[["P_day_all_wettest6monPeakDay"]]$attArgs$seas <- seas
  }

  # fracTotList = attSel[grepl('fracTot',attSel)]
  # if (length(fracTotList)>0){
  #   tot = func_tot(data)
  #   tot = max(tot,0.001)
  #   for (att in fracTotList){
  #     attCalcInfo[[att]]$attArgs$tot = tot
  #   }
  # }
  #
  # fracNwetList = attSel[grepl('fracNwet',attSel)]
  # if (length(fracNwetList)>0){
  #   threshold = attCalcInfo[[fracNwetList[1]]]$attArgs$threshold # note currently assumes thresold same for all attributes
  #   nWet = func_nWet(data,attArgs=list(threshold=threshold))
  #   nWet = max(nWet,0.001)
  #   for (att in fracNwetList){
  #     attCalcInfo[[att]]$attArgs$nWetTot = nWet
  #     attCalcInfo[[att]]$attArgs$threshold = threshold
  #   }
  # }
  #
  # fracCorList = attSel[grepl('fracCor',attSel)]
  # if (length(fracCorList)>0){
  #   cor = func_cor(data)
  #   for (att in fracCorList){
  #     attCalcInfo[[att]]$attArgs$corTot = cor
  #   }
  # }
  #
  # fracWDcorList = attSel[grepl('fracWDcor',attSel)]
  # if (length(fracWDcorList)>0){
  #   WDcor = func_WDcor(data)
  #   for (att in fracWDcorList){
  #     attCalcInfo[[att]]$attArgs$WDcorTot = WDcor
  #   }
  # }
  #
  # fracP99List = attSel[grepl('fracP99',attSel)]
  # if (length(fracP99List)>0){
  #   P99Tot = quantile(data,probs=0.99,na.rm=T,names=F)
  #   P99Tot = max(P99Tot,0.001)
  #   for (att in fracP99List){
  #     attCalcInfo[[att]]$attArgs$P99Tot = P99Tot
  #   }
  # }
  #
  # fracxP99overPaveList = attSel[grepl('fracxP99overPave',attSel)]
  # if (length(fracxP99overPaveList)>0){
  #   xP99overPaveTot = func_xP99overPave(data)
  #   xP99overPaveTot = max(xP99overPaveTot,0.001)
  #   for (att in fracxP99overPaveList){
  #     attCalcInfo[[att]]$attArgs$xP99overPaveTot = xP99overPaveTot
  #   }
  # }

  out <- list()
  for (att in attSel) {
    if (is.null(attCalcInfo[[att]]$opName)) { # case where there is no operator in attribute name
      if (is.null(dim(data))) {
        out[[att]] <- extractor(
          func = attCalcInfo[[att]]$func,
          data = data,
          indx = attCalcInfo[[att]]$indx,
          attArgs = attCalcInfo[[att]]$attArgs
        )
      } else {
        out[[att]] <- apply(
          X = data, MARGIN = 2, FUN = extractor,
          func = attCalcInfo[[att]]$func,
          indx = attCalcInfo[[att]]$indx,
          attArgs = attCalcInfo[[att]]$attArgs
        )
      }
    } else if (attCalcInfo[[att]]$opName %in% c("m", "m10yrBlock", "m40yrBlock", "m50yrBlock")) { # mean of values calculated in each year
      if (is.null(dim(data))) {
        out[[att]] <- extractor.summaryMean(
          func = attCalcInfo[[att]]$func,
          data = data,
          indx = attCalcInfo[[att]]$indx,
          attArgs = attCalcInfo[[att]]$attArgs
        )
      } else {
        out[[att]] <- apply(
          X = data, MARGIN = 2, FUN = extractor.summaryMean,
          func = attCalcInfo[[att]]$func,
          indx = attCalcInfo[[att]]$indx,
          attArgs = attCalcInfo[[att]]$attArgs
        )
      }
    } else if (attCalcInfo[[att]]$opName == "sd") { # sd of values calculated in each year
      if (is.null(dim(data))) {
        out[[att]] <- extractor.summarySD(
          func = attCalcInfo[[att]]$func,
          data = data,
          indx = attCalcInfo[[att]]$indx,
          attArgs = attCalcInfo[[att]]$attArgs
        )
      } else {
        out[[att]] <- apply(
          X = data, MARGIN = 2, FUN = extractor.summarySD,
          func = attCalcInfo[[att]]$func,
          indx = attCalcInfo[[att]]$indx,
          attArgs = attCalcInfo[[att]]$attArgs
        )
      }
      ####### NOTE: calling the following separately is inefficient (annual totals calculated for each)
    } else if (attCalcInfo[[att]]$opName == "cor") { # correlation between years
      if (is.null(dim(data))) {
        out[[att]] <- extractor.summaryCor(
          func = attCalcInfo[[att]]$func,
          data = data,
          indx = attCalcInfo[[att]]$indx,
          attArgs = attCalcInfo[[att]]$attArgs
        )
      } else {
        out[[att]] <- apply(
          X = data, MARGIN = 2, FUN = extractor.summaryCor,
          func = attCalcInfo[[att]]$func,
          indx = attCalcInfo[[att]]$indx,
          attArgs = attCalcInfo[[att]]$attArgs
        )
      }
    } else if (attCalcInfo[[att]]$opName == "cv") { # correlation between years
      if (is.null(dim(data))) {
        out[[att]] <- extractor.summaryCV(
          func = attCalcInfo[[att]]$func,
          data = data,
          indx = attCalcInfo[[att]]$indx,
          attArgs = attCalcInfo[[att]]$attArgs
        )
      } else {
        out[[att]] <- apply(
          X = data, MARGIN = 2, FUN = extractor.summaryCV,
          func = attCalcInfo[[att]]$func,
          indx = attCalcInfo[[att]]$indx,
          attArgs = attCalcInfo[[att]]$attArgs
        )
      }
      # } else if (attCalcInfo[[att]]$opName=='corSOI'){ #
      #   if (is.null(dim(data))){
      #     out[[att]] = extractor.summaryCorSOI(func=attCalcInfo[[att]]$func,
      #                                       data=data,
      #                                       indx=attCalcInfo[[att]]$indx,
      #                                       attArgs=attCalcInfo[[att]]$attArgs)
      #   } else {
      #     out[[att]] = apply(X=data,MARGIN=2,FUN=extractor.summaryCorSOI,
      #                        func=attCalcInfo[[att]]$func,
      #                        indx=attCalcInfo[[att]]$indx,
      #                        attArgs=attCalcInfo[[att]]$attArgs)
      #   }
    } else if (attCalcInfo[[att]]$opName == "dwellTime") { # sd of values calculated in each year
      if (is.null(dim(data))) {
        out[[att]] <- extractor.summaryDwellTime(
          func = attCalcInfo[[att]]$func,
          data = data,
          indx = attCalcInfo[[att]]$indx,
          attArgs = attCalcInfo[[att]]$attArgs
        )
      } else {
        out[[att]] <- apply(
          X = data, MARGIN = 2, FUN = extractor.summaryDwellTime,
          func = attCalcInfo[[att]]$func,
          indx = attCalcInfo[[att]]$indx,
          attArgs = attCalcInfo[[att]]$attArgs
        )
      }
      # } else if (attCalcInfo[[att]]$opName=='range90'){ # sd of values calculated in each year
      #   if (is.null(dim(data))){
      #     out[[att]] = extractor.summaryRange90(func=attCalcInfo[[att]]$func,
      #                                             data=data,
      #                                             indx=attCalcInfo[[att]]$indx,
      #                                             attArgs=attCalcInfo[[att]]$attArgs)
      #   } else {
      #     out[[att]] = apply(X=data,MARGIN=2,FUN=extractor.summaryRange90,
      #                        func=attCalcInfo[[att]]$func,
      #                        indx=attCalcInfo[[att]]$indx,
      #                        attArgs=attCalcInfo[[att]]$attArgs)
      #   }
    } else if (attCalcInfo[[att]]$opName %in% c("max3yr", "max5yr")) { # maximum of 3 or 5 year values
      if (is.null(dim(data))) {
        out[[att]] <- extractor.summaryMax(
          func = attCalcInfo[[att]]$func,
          data = data,
          indx = attCalcInfo[[att]]$indx,
          attArgs = attCalcInfo[[att]]$attArgs
        )
      } else {
        out[[att]] <- apply(
          X = data, MARGIN = 2, FUN = extractor.summaryMax,
          func = attCalcInfo[[att]]$func,
          indx = attCalcInfo[[att]]$indx,
          attArgs = attCalcInfo[[att]]$attArgs
        )
      }
    } else if (attCalcInfo[[att]]$opName %in% c("min3yr", "min5yr")) { # maximum of 3 or 5 year values
      if (is.null(dim(data))) {
        out[[att]] <- extractor.summaryMin(
          func = attCalcInfo[[att]]$func,
          data = data,
          indx = attCalcInfo[[att]]$indx,
          attArgs = attCalcInfo[[att]]$attArgs
        )
      } else {
        out[[att]] <- apply(
          X = data, MARGIN = 2, FUN = extractor.summaryMin,
          func = attCalcInfo[[att]]$func,
          indx = attCalcInfo[[att]]$indx,
          attArgs = attCalcInfo[[att]]$attArgs
        )
      }
      # } else if (attCalcInfo[[att]]$opName%in%c('p10.3yr','p10.5yr')){ # maximum of 3 or 5 year values
      #   if (is.null(dim(data))){
      #     out[[att]] = extractor.summaryP10(func=attCalcInfo[[att]]$func,
      #                                       data=data,
      #                                       indx=attCalcInfo[[att]]$indx,
      #                                       attArgs=attCalcInfo[[att]]$attArgs)
      #   } else {
      #     out[[att]] = apply(X=data,MARGIN=2,FUN=extractor.summaryP10,
      #                        func=attCalcInfo[[att]]$func,
      #                        indx=attCalcInfo[[att]]$indx,
      #                        attArgs=attCalcInfo[[att]]$attArgs)
      #   }
      # } else if (attCalcInfo[[att]]$opName%in%c('p1.3yr','p1.5yr')){ # maximum of 3 or 5 year values
      #   if (is.null(dim(data))){
      #     out[[att]] = extractor.summaryP1(func=attCalcInfo[[att]]$func,
      #                                       data=data,
      #                                       indx=attCalcInfo[[att]]$indx,
      #                                       attArgs=attCalcInfo[[att]]$attArgs)
      #   } else {
      #     out[[att]] = apply(X=data,MARGIN=2,FUN=extractor.summaryP1,
      #                        func=attCalcInfo[[att]]$func,
      #                        indx=attCalcInfo[[att]]$indx,
      #                        attArgs=attCalcInfo[[att]]$attArgs)
      #   }
    }
  }

  return(out)
}

####################################
# Some error handling functions.
# Note: need to fix up use of logfile() - currently just outputting to screen through stop()

invalidSuffixStop <- function(funcName, suffix) {
  if (is.na(suffix)) {
    errMess <- paste0("Error: invalid attribute name (must specify suffix for attribute function '", funcName, "')")
  } else {
    errMess <- paste0("Error: invalid attribute name (cannot use suffix '", suffix, "' for attribute function '", funcName, "')")
  }
  #  logfile(errMess,file)
  #  logfile("Program terminated",file)
  cat(errMess)
  stop(errMess)
}

invalidStratificationStop <- function(strat) {
  errMess <- paste0("Error: invalid attribute name (stratification '", strat, "' not valid)")
  cat(errMess)
  #  logfile(errMess,file)
  #  logfile("Program terminated",file)
  stop(errMess)
}

invalidAggregationStop <- function(agg) {
  errMess <- paste0("Error: invalid aggregation name (aggregation '", agg, "' not valid)")
  cat(errMess)
  #  logfile(errMess,file)
  #  logfile("Program terminated",file)
  stop(errMess)
}

invalidOperationStop <- function(opName) {
  errMess <- paste0("Error: invalid attribute name (operation '", opName, "' not valid)")
  cat(errMess)
  #  logfile(errMess,file)
  #  logfile("Program terminated",file)
  stop(errMess)
}

invalidFuncStop <- function(func) {
  errMess <- paste0("Error: invalid attribute name (function '", func, "' does not exist)")
  cat(errMess)
  #  logfile(errMess,file)
  #  logfile("Program terminated",file)
  stop(errMess)
}

####################################

calc_att_components <- function(att) {
  # split up attribute name
  chopped <- strsplit(x = att, split = "_")[[1]]

  out <- list()

  tmp <- strsplit(chopped[1], "[.]")[[1]]
  if (tmp[1] == "mv") {
    out$type <- "multivariable"
    if (length(tmp) != 3) {
      stop("require 2 variable names for multivariable attributes (e.g. mv.P.T)")
    }
    out$varName <- paste0(tmp[2], "/", tmp[3])
  } else if (tmp[1] == "ms") {
    out$type <- "multisite"
    if (length(tmp) != 2) {
      stop("require 1 variable name for multisite attributes (e.g. ms.P)")
    }
    out$varName <- tmp[2]
  } else {
    out$type <- "single"
    if (length(tmp) != 1) {
      stop("require 1 variable name for single site/variabvle attributes (e.g. P)")
    }
    out$varName <- tmp[1]
  }

  # variable name
  # out$varName = chopped[1]
  # aggregation
  out$aggName <- chopped[2]
  # stratification index name
  out$indexName <- chopped[3]
  # long function name (including parameters)
  out$funcNameLong <- chopped[4]
  # operator name
  out$opName <- NULL
  if (length(chopped) > 4) {
    out$opName <- chopped[5]
  }

  return(out)
}

####################################
# calculate attribute info based on attribute name

attribute.calculator.setup <- function(attSel, # list of evaluated attribute names
                                       datInd, # dat indices and properties (e.g. datInd$nyr, datInd$i.yy)
                                       attCalcInfo = list()) {
  for (att in attSel) {
    o <- calc_att_components(att)
    varName <- o$varName
    aggName <- o$aggName
    indexName <- o$indexName
    funcNameLong <- o$funcNameLong
    opName <- o$opName
    type <- o$type
    varName <- o$varName

    # calculate selected data indices
    indx <- calcStratIndex(indexName, opName, datInd)

    # calculate function names and arguments
    o <- calcFuncNamesAndArgs(funcNameLong, datInd, type)
    func <- o$func
    attArgs <- o$attArgs

    attCalcInfo[[att]] <- list(
      func = func, attArgs = attArgs,
      indx = indx, opName = opName,
      type = type, varName = varName
    )
  }

  return(attCalcInfo)
}

####################################
# Calculate function names and arguments

calcFuncNamesAndArgs <- function(funcNameLong, # long function name (including parameters)
                                 datInd, # dat indices and properties (e.g. datInd$nyr, datInd$i.yy)
                                 type) {
  # functions that require threshold arguments
  funcsWithThresh <- c("nWet", "dyWet", "maxDSD", "maxWSD", "avgWSD", "avgDSD")#, "fracNwet")

  attArgs <- NULL

  suffix <- NULL
  # select index for funcsWithThresh
  iFuncWithThresh <- which(startsWith(funcNameLong, funcsWithThresh))
  if (funcNameLong %in% funcsWithThresh) { # case where no additional params are specified as suffixes
    funcName <- funcNameLong
    attArgs <- list(threshold = 0.) # set default threshold to zero since no additional params specified
  } else if (length(iFuncWithThresh) > 0) { # case where suffix is specified
    funcName <- funcsWithThresh[iFuncWithThresh]
    suffix <- strsplit(funcNameLong, funcName)[[1]][2] # suffix
    if (substring(suffix, 1, 1) == "T") { # for case where suffix starts with T we read off threshold
      threshold <- as.numeric(substring(suffix, 2))
      if (is.na(threshold)) {
        invalidSuffixStop(funcName = funcName, suffix = suffix)
      } # stop if threshold not numeric
      attArgs <- list(threshold = threshold)
    } else {
      invalidSuffixStop(funcName = funcName, suffix = suffix)
    } # stop if suffix doesn't start with T

    # seasonality ratios
  } else if (funcNameLong == "seasRatio") { # note seasRatio not setup to work with monthly/seasonal stratification or with "_m" for mean annual
    funcName <- "seasRatio" # seasonality ratio from foreSIGHT 1.0 (wet season = MAM+JJA)
    m1 <- as.integer(3)
    m2 <- as.integer(8)
    if (!is.integer(m1) | m1 < 1 | m1 > 12 | !is.integer(m2) | m2 < 1 | m2 > 12) {
      invalidSuffixStop(funcName = funcName, suffix = suffix)
    }
    if (m1 < m2) {
      mSeas <- seq(m1, m2)
    } else { # handle case where wet season ends in next year
      mSeas <- (seq(m1, m2 + 12) - 1) %% 12 + 1
    }
    attArgs <- list(indexSeas = unlist(datInd$i.mm[mSeas]))
  } else if (startsWith(funcNameLong, "seasRatio")) {
    funcName <- "seasRatio"
    suffix <- strsplit(funcNameLong, funcName)[[1]][2]
    m1 <- match(substring(suffix, 1, 3), month.abb) # start month
    m2 <- match(substring(suffix, 4, 6), month.abb) # end month
    if (!is.integer(m1) | m1 < 1 | m1 > 12 | !is.integer(m2) | m2 < 1 | m2 > 12) {
      invalidSuffixStop(funcName = funcName, suffix = suffix)
    }
    if (m1 < m2) {
      mSeas <- seq(m1, m2)
    } else { # handle case where wet season ends in next year
      mSeas <- (seq(m1, m2 + 12) - 1) %% 12 + 1
    }

    attArgs <- list(indexSeas = unlist(datInd$i.mm[mSeas]))
    # quantile ranges
  } else if (funcNameLong == "rng") {
    funcName <- "rng"
    attArgs <- list(lim = 0.9) # default limit in 90% (i.e. 5-95%)
  } else if (startsWith(funcNameLong, "rng")) {
    funcName <- "rng"
    suffix <- strsplit(funcNameLong, funcName)[[1]][2]
    lim <- as.numeric(suffix) / 100.
    if (!is.na(lim)) {
      attArgs <- list(lim = lim)
    } else {
      invalidSuffixStop(funcName = funcName, suffix = suffix)
    }

    # percentiles
  } else if (substring(funcNameLong, 1, 1) == "P") {
    funcName <- "P"
    suffix <- strsplit(funcNameLong, funcName)[[1]][2]
    p <- as.numeric(suffix)
    if (!is.na(p)) {
      attArgs <- list(quant = 0.01 * p) # convert percentile to quantile
    } else {
      invalidSuffixStop(funcName = funcName, suffix = suffix)
    }

    # normalised percentiles
  } else if (substring(funcNameLong, 1, 5) == "normP") {
    funcName <- "normP"
    suffix <- strsplit(funcNameLong, funcName)[[1]][2]
    p <- as.numeric(suffix)
    if (!is.na(p)) {
      attArgs <- list(quant = 0.01 * p) # convert percentile to quantile
    } else {
      invalidSuffixStop(funcName = funcName, suffix = suffix)
    }

    # num days above threshold
  } else if (substring(funcNameLong, 1, 1) == "R") {
    funcName <- "R"
    suffix <- strsplit(funcNameLong, funcName)[[1]][2]
    t <- as.numeric(suffix)
    if (!is.na(t)) {
      attArgs <- list(threshold = t)
    } else {
      invalidSuffixStop(funcName = funcName, suffix = suffix)
    }
  } else if (funcNameLong %in% c("wettest6monPeakDay", "wettest6monSeasRatio")) {
    funcName <- funcNameLong
    doy <- datInd$jj
    keepMat <- calc_keepMat(doy)
    attArgs <- list(doy = doy, keepMat = keepMat)
  } else {
    funcName <- funcNameLong
  }

  if (type == "single") {
    func_str <- paste0("func_", funcName)
  } else if (type == "multivariable") {
    func_str <- paste0("mvFunc_", funcName)
  } else if (type == "multisite") {
    func_str <- paste0("msFunc_", funcName)
  }
  if (!exists(func_str, mode = "function")) {
    print(func_str)
    invalidFuncStop(func = func_str)
  }
  func <- get(func_str)

  return(list(func = func, attArgs = attArgs, funcName = funcName, suffix = suffix))
}

####################################
# calculate stratification index

calcStratIndex <- function(indexName, opName, datInd) {
  # abbreviate for season names
  season.abb <- c("SON", "DJF", "MAM", "JJA")
  month.str.abb <- c("JFMAMJJASONDJFMAMJJASOND") # 2 year month abbreviation to allow for wrap around months
  month_number <- c(1:12, 1:12) # month.str.abb as month numbers

  stratIndx <- NULL
  if (indexName == "all") { # this uses all data
    stratIndx <- 1:datInd$nTimes
  } else if (indexName %in% month.abb) { # this only uses data from given month
    mSel <- match(indexName, month.abb)
    stratIndx <- datInd$i.mm[[mSel]]
  } else if (indexName %in% season.abb) { # this only uses data from given season
    sSel <- match(indexName, season.abb)
    stratIndx <- datInd$i.ss[[sSel]]
  } else if (regexpr(indexName, month.str.abb)[1] != -1 & nchar(indexName) > 1 & nchar(indexName) < 12) { # check if string is sequential months and are greater than 1 month and less than 12
    indexName_posit <- regexpr(indexName, month.str.abb) # find position of month string in 2 year abbreviation
    mSel <- sort(month_number[seq(indexName_posit[1], length = attr(indexName_posit, "match.length"), by = 1)]) # match month letter index to number index and sort months sequentially
    for (n in 1:nchar(indexName)) {
      stratIndx <- c(stratIndx, datInd$i.mm[[mSel[n]]])
    }
  } else {
    invalidStratificationStop(strat = indexName)
  }

  if (is.null(opName)) {
    indx <- calc_indx_list(stratIndx)
  } else { # here we calculate stratification for each year (later used to calculate mean/max values over all years)
    yrIndx <- list()
    if (opName %in% c("m", "sd", "cor", "dwellTime", "range90", "corSOI", "cv")) {
      for (y in 1:length(datInd$i.yy)) {
        yrIndx[[y]] <- intersect(datInd$i.yy[[y]], stratIndx)
      }
    } else if ((opName == "max3yr") | (opName == "min3yr") | (opName == "p10.3yr") | (opName == "p1.3yr")) {
      for (y in 1:length(datInd$i.3yy)) {
        yrIndx[[y]] <- intersect(datInd$i.3yy[[y]], stratIndx)
      }
    } else if ((opName == "max5yr") | (opName == "min5yr") | (opName == "p10.5yr") | (opName == "p1.5yr")) {
      for (y in 1:length(datInd$i.5yy)) {
        yrIndx[[y]] <- intersect(datInd$i.5yy[[y]], stratIndx)
      }
    } else if (opName == "m10yrBlock") { # note this is binned average, not moving average (unlike max5yr)
      yrIndx <- list()
      for (y in 1:length(datInd$i.10yyBlock)) {
        yrIndx[[y]] <- intersect(datInd$i.10yyBlock[[y]], stratIndx)
      }
    } else if (opName == "m40yrBlock") { # note this is binned average, not moving average (unlike max5yr)
      yrIndx <- list()
      for (y in 1:length(datInd$i.40yyBlock)) {
        yrIndx[[y]] <- intersect(datInd$i.40yyBlock[[y]], stratIndx)
      }
    } else if (opName == "m50yrBlock") { # note this is binned average, not moving average (unlike max5yr)
      yrIndx <- list()
      for (y in 1:length(datInd$i.50yyBlock)) {
        yrIndx[[y]] <- intersect(datInd$i.50yyBlock[[y]], stratIndx)
      }
    } else {
      invalidOperationStop(opName = opName)
    }
    indx <- list()
    for (y in 1:length(yrIndx)) {
      indx[[y]] <- calc_indx_list(yrIndx[[y]])
    }
  }

  return(indx)
}

###################

calc_indx_list <- function(val) {
  breaks <- which(diff(val) > 1)

  N.new <- length(val) + length(breaks)
  isNA <- breaks + seq(1, length(breaks))
  a <- 1:N.new
  notNA <- a[!a %in% isNA]

  indx_list <- list(val = val, breaks = breaks, N = N.new, notNA = notNA)

  return(indx_list)
}

####################################
# ATTRIBUTE AUX INFO (determine attribute type and if approved combo with model used)
attribute.info.check <- function(attSel = NULL, # vector of selected attributes (strings)
                                 attPrim = NULL,
                                 lambda.mult = NULL,
                                 targetType = NULL
                                 # simVar=NULL    # vector of variables simulated using models e.g. c("P","Temp")
                                 # modelTag=NULL # model selected
) {
  nAtt <- length(attSel) # no. of attributes nominated
  attInfo <- list() # create blank list for storage

  # attribute name chopper function
  attInfo$varType <- vapply(attSel, FUN = get.attribute.varType, FUN.VALUE = character(1), USE.NAMES = FALSE) # drop use of names as comes ordered anyway

  attInfo$aggType <- vapply(attSel, FUN = get.attribute.aggType, FUN.VALUE = character(1), USE.NAMES = FALSE) # drop use of names as comes ordered anyway
  # check if valid aggregation period
  i <- which(!attInfo$aggType %in% names(aggNameLong))
  if (length(i) > 0) {
    stop(paste0("Atttribute ", attSel[i], " has invalid aggregation period ", attInfo$aggType[i]))
  }

  # ASSIGN TARGET TYPE (IF P USE "FRAC", IF T USE "DIFF")
  attInfo$targetType <- targetType

  # FIND WHICH ARE PRIMARY
  if (is.null(attPrim)) {
    attInfo$primType <- rep(FALSE, nAtt)
    attInfo$primMult <- rep(0, nAtt)
  } else {
    get.ind <- function(x, y) {
      which(x == y)
    } # quick function to find which are primary attributes
    primInd <- vapply(attPrim, FUN = get.ind, FUN.VALUE = numeric(1), x = attSel, USE.NAMES = FALSE) # Indices of primary attributes
    attInfo$primType <- rep(FALSE, nAtt)
    attInfo$primType[primInd] <- TRUE # mark primary ones 'TRUE'

    # Get lambda.mult (corresponding to attPrim) in the correct locations wrt attSel
    # Infomation in lamda.mult will be transferred to primMult & used in the penalty score calculation for the objective function
    # Note: attPrim is a member of attSel - but the order need not be exact, so assignment has to be done inside the loop
    attInfo$primMult <- rep(0, nAtt)
    for (i in 1:length(attPrim)) {
      # Locate attPrim
      indPrim <- which(attSel == attPrim[i])
      # Assign lambda.mult corresponding to attPrim
      attInfo$primMult[indPrim] <- lambda.mult[i]
    }
  }

  return(attInfo)
}

get.att.ind.withAggs <- function(attInfo = NULL,
                                 simVar = NULL,
                                 simAgg = NULL,
                                 multVar = F) {
  simVar <- unique(attInfo$varType)
  if (multVar) {
    simVar <- unique(unlist(strsplit(simVar, "[/]")))
  }
  simAgg <- unique(attInfo$aggType)
  # DETERMINE WHICH ATTRIBUTE RELATES TO WHICH SIMULATOR
  attInd <- list()
  if (simVar[1] != "All") { # ONLY DO IF STOCHASTIC GENERATION IS SELECTED (not simple scaling)
    for (i in 1:length(simVar)) {
      attInd[[simVar[i]]] <- list()
      for (j in 1:length(simAgg)) {
        if (!multVar) {
          attInd[[simVar[i]]][[simAgg[j]]] <- which((attInfo$varType == simVar[i]) &
            (attInfo$aggType == simAgg[j]))
        } else {
          attInd[[simVar[i]]][[simAgg[j]]] <- which(((attInfo$varType == simVar[i]) |
            startsWith(attInfo$varType, paste0(simVar[i], "/"))) &
            (attInfo$aggType == simAgg[j]))
        }
      }
    }
  }
  return(attInd)
}

get.att.ind <- function(attInfo = NULL,
                        simVar = NULL) {
  # DETERMINE WHICH ATTRIBUTE RELATES TO WHICH SIMULATOR
  attInd <- list()
  if (simVar[1] != "All") { # ONLY DO IF STOCHASTIC GENERATION IS SELECTED (not simple scaling)
    for (i in 1:length(simVar)) {
      attInd[[simVar[i]]] <- which((attInfo$varType == simVar[i]) |
        startsWith(attInfo$varType, paste0(simVar[i], "/")))
    }
  }
  return(attInd)
}



update_att_Info <- function(attInfo = NULL,
                            attInd = NULL,
                            modelTag = NULL,
                            simVar = NULL) {
  # divide up attInfo to different models
  for (i in 1:length(modelTag)) {
    attInfo[[modelTag[i]]]$varType <- attInfo$varType[attInd[[simVar[i]]]]
    attInfo[[modelTag[i]]]$aggType <- attInfo$aggType[attInd[[simVar[i]]]]
    attInfo[[modelTag[i]]]$targetType <- attInfo$targetType[attInd[[simVar[i]]]]
    attInfo[[modelTag[i]]]$primType <- attInfo$primType[attInd[[simVar[i]]]]
    attInfo[[modelTag[i]]]$primMult <- attInfo$primMult[attInd[[simVar[i]]]]
  }
  return(attInfo)
}

# GETS VARTYPE of attribute
get.attribute.varType <- function(attrib = NULL, # attribute name
                                  sep = "_") {
  varType <- calc_att_components(attrib)$varName

  return(varType)
}

# GETS aggregation type of attribute
get.attribute.aggType <- function(attrib = NULL, # attribute name
                                  sep = "_") {
  aggType <- calc_att_components(attrib)$aggName

  return(aggType)
}

# GETS function name of attribute
get.attribute.funcType <- function(attrib = NULL, # attribute name
                                   sep = "_") {
  funcType <- calc_att_components(attrib)$funcName

  return(funcType)
}


# TARGET TYPE CLASSIFIER
get.target.type <- function(varType = NULL) {
  if (varType == "P") {
    targetType <- "frac"
  } else {
    if (varType == "Temp") {
      targetType <- "diff"
    } else {
      targetType <- "frac"
    }
  }
  return(targetType)
}

#######################################################################################

tagBlender <- function(attLab = NULL) {
  o <- calc_att_components(attLab)
  varName <- o$varName
  aggName <- o$aggName
  indexName <- o$indexName
  funcNameLong <- o$funcNameLong
  opName <- o$opName
  type <- o$type
  varName <- o$varName
  aggName <- o$aggName

  # variable type
  if (varName == "P") {
    vtype <- "rainfall"
  } else if (varName == "Temp") {
    vtype <- "temperature"
  } else if (varName == "PET") {
    vtype <- "PET"
  } else if (varName == "Radn") {
    vtype <- "Radiation"
  } else {
    vtype <- varName
  }

  # stratification type
  month.str.abb <- c("JFMAMJJASONDJFMAMJJASOND") # 2 year month abbreviation to allow for wrap around months
  month_number <- c(1:12, 1:12) # month.str.abb as month numbers
  if (indexName == "all") {
    stype <- NULL
  } else if (indexName == "DJF") {
    stype <- "DJF"
  } else if (indexName == "MAM") {
    stype <- "MAM"
  } else if (indexName == "JJA") {
    stype <- "JJA"
  } else if (indexName == "SON") {
    stype <- "SON"
  } else if (indexName == "Jan") {
    stype <- "Jan"
  } else if (indexName == "Feb") {
    stype <- "Feb"
  } else if (indexName == "Mar") {
    stype <- "Mar"
  } else if (indexName == "Apr") {
    stype <- "Apr"
  } else if (indexName == "May") {
    stype <- "May"
  } else if (indexName == "Jun") {
    stype <- "Jun"
  } else if (indexName == "Jul") {
    stype <- "Jul"
  } else if (indexName == "Aug") {
    stype <- "Aug"
  } else if (indexName == "Sep") {
    stype <- "Sep"
  } else if (indexName == "Oct") {
    stype <- "Oct"
  } else if (indexName == "Nov") {
    stype <- "Nov"
  } else if (indexName == "Dec") {
    stype <- "Dec"
  } else if (regexpr(indexName, month.str.abb)[1] != -1 & nchar(indexName) > 1 & nchar(indexName) < 12) {
    stype <- indexName
  } else {
    cat(paste0("invalid attribute: cannot use ", indexName, " stratification"))
    return(invisible())
  }

  # use calcFuncNamesAndArgs() to calculate parameter values from long function name
  o <- calcFuncNamesAndArgs(funcNameLong = funcNameLong, datInd = NULL, type = type)

  if (funcNameLong == "tot") {
    ftype <- "total"
  } else if (funcNameLong == "avg") {
    ftype <- "average"
  } else if (startsWith(funcNameLong, "seasRatio")) {
    if (is.null(o$suffix)) {
      ftype <- "ratio of season to total"
    } else {
      wetStart <- substring(o$suffix, 1, 3)
      wetEnd <- substring(o$suffix, 4, 6)
      ftype <- paste0("ratio of ", wetStart, "-", wetEnd, " to total")
    }
    if (indexName == "all") {
      stype <- NULL
    } else {
      errMess <- paste0("invalid attribute: cannot compute seasRatio for ", indexName, " stratification\n")
      cat(errMess)
      return(invisible())
    }
  } else if (substring(funcNameLong, 1, 1) == "P") {
    if (is.null(o$suffix)) {
      errMess <- "invalid attribute: P attribute requires specification of percentile\n"
      cat(errMess)
      return(invisible())
    } else {
      p <- o$suffix
      ftype <- paste0(p, "th percentile")
    }
    if (indexName == "all") {
      stype <- NULL
    }
  } else if (startsWith(funcNameLong, "nWet")) {
    if (is.null(o$suffix)) {
      ftype <- "no. wet days"
    } else {
      thresh <- o$suffix
      ftype <- paste0("no. wet days (above ", thresh, ")")
    }
  } else if (startsWith(funcNameLong, "maxDSD")) {
    if (is.null(o$suffix)) {
      ftype <- "max dryspell duration"
    } else {
      thresh <- o$suffix
      ftype <- paste0("max dryspell duration (below ", thresh, ")")
    }
  } else if (startsWith(funcNameLong, "maxWSD")) {
    if (is.null(o$suffix)) {
      ftype <- "max wetspell duration"
    } else {
      thresh <- o$suffix
      ftype <- paste0("max wetspell duration (above ", thresh, ")")
    }
  } else if (startsWith(funcNameLong, "avgDSD")) {
    if (is.null(o$suffix)) {
      ftype <- "average dryspell duration"
    } else {
      thresh <- o$suffix
      ftype <- paste0("average dryspell duration (below ", thresh, ")")
    }
  } else if (startsWith(funcNameLong, "avgWSD")) {
    if (is.null(o$suffix)) {
      ftype <- "average wetspell duration"
    } else {
      thresh <- o$suffix
      ftype <- paste0("average wetspell duration (above ", thresh, ")")
    }
  } else if (startsWith(funcNameLong, "dyWet")) {
    if (is.null(o$suffix)) {
      ftype <- "wet day amount"
    } else {
      thresh <- o$suffix
      ftype <- paste0("wet day amount (above ", thresh, ")")
    }
  } else if (funcNameLong == "GSL") {
    ftype <- "growing season length"
  } else if (funcNameLong == "CSL") {
    ftype <- "cold season length"
  } else if (funcNameLong == "F0") {
    if (varName != "T") {
      errMess <- "invalid attribute: can only compute frost days for T\n"
      cat(errMess)
      return(invisible())
    }
    ftype <- "no. frost days"
  } else if (substring(funcNameLong, 1, 1) == "R") {
    if (is.null(o$suffix)) {
      errMess <- "invalid attribute: R attribute requires specification of threshold\n"
      cat(errMess)
      return(invisible())
    } else {
      t <- o$suffix
      ftype <- paste0("no. days above ", t)
    }
  } else if (startsWith(funcNameLong, "rng")) {
    if (is.null(o$attArgs$lim)) {
      errMess <- "invalid attribute: rng attribute requires attrtibe argument for lim\n"
      cat(errMess)
      return(invisible())
    } else {
      lim <- 100 * as.numeric(o$attArgs$lim)
      ftype <- paste0(lim, "% range")
    }
  } else {
    # errMess = paste0('invalid attribute: built-in function not available for ',funcNameLong)
    # cat(errMess)
    ftype <- funcNameLong
    # return(invisible())
  }

  # aggType
  if (aggName == "day") {
    atype <- "daily"
  } else if (aggName == "hour") {
    atype <- "hourly"
  } else if (aggName == "month") {
    atype <- "monthly"
  } else if (aggName == "year") {
    atype <- "annual"
  } else {
    atype <- aggName
  }

  # operation
  if (is.null(opName)) {
    otype <- NULL
  } else {
    if (opName == "m") {
      otype <- "mean annual"
    } else if (opName == "sd") {
      otype <- "sdev annual"
    } else if (opName == "min5yr") {
      otype <- "Min 5yr"
    } else if (opName == "dwellTime") {
      otype <- "dwell time annual"
    } else if (opName == "range90") {
      otype <- "90% range annual"
    } else {
      otype <- opName
    }
  }

  if (funcNameLong == "tot") {
    atype <- NULL
    if (!is.null(opName)) {
      ftype <- NULL
    }
  }
  if (startsWith(funcNameLong, "seasRatio")) {
    atype <- NULL
  }

  # stitch togther
  # phrase=paste(otype,stype,atype,ftype,vtype)
  phrase <- otype
  if (!is.null(ftype)) {
    phrase <- paste(phrase, ftype)
  }
  if (!is.null(stype)) {
    phrase <- paste(phrase, stype)
  }
  if (!is.null(atype)) {
    phrase <- paste(phrase, atype)
  }
  if (!is.null(vtype)) {
    phrase <- paste(phrase, vtype)
  }

  phrase <- trimws(phrase)
  phrase <- paste(toupper(substr(phrase, 1, 1)), substr(phrase, 2, nchar(phrase)), sep = "")

  return(phrase)
}
