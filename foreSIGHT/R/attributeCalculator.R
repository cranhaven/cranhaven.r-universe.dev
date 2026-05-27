# attribute calculator#

#' Calculates the attributes of the hydroclimate time series
#'
#' \code{calculateAttributes} calculates the specified attributes of the input daily hydroclimate time series.
#' @param climateData list; reference climate data, with vector \emph{times} in POSIXct format,
#' and climate variables \emph{*variable_name1*}, \emph{*variable_name2*}. Climate variables are specified as vectors for single-site data, and matrices for multi-site data (with columns for each site). \cr
#'            Please refer to data provided with the package that may be loaded using \code{data(tankDat)} and \code{data(barossaDat)} for examples of the expected format of single site and multi-site \code{reference}.
#' @param attSel a vector; specifying the names of the attributes to be calculated.
#' @param startYr a number (default \code{NULL}); to specify the starting year to subset \code{climateData} if required.
#' If \code{NULL}, \code{startYr} is starting year in the input \code{climateData}.
#' @param endYr a number (default \code{NULL}); to specify the ending year to subset \code{climateData} if required.
#' If \code{NULL}, \code{endYr} is last year in the input \code{climateData}.
#' @return The function returns a vector of attributes with names of the attributes (\code{attSel}).
#' For multi-site data, names are combinations of attribute and site names.
#' @examples
#' #----------------------------------------------------------------------
#' # Example 1: Single-site  input
#' # load 'tank' example climate data available in the package
#' data("tankDat")
#' # specify rainfall and temperature attributes to calculate
#' attSel <- c(
#'   "P_day_all_tot_m", "P_day_all_nWet_m", "P_day_all_R10_m",
#'   "Temp_day_all_rng_m", "Temp_day_all_avg_m"
#' )
#' tank_obs_atts <- calculateAttributes(tank_obs, attSel = attSel)
#' #----------------------------------------------------------------------
#' # Example 2: Multi-site  input
#' # load 'Barossa' example climate data available in the package
#' data("barossaDat")
#' # specify rainfall attributes to calculate
#' attSel <- c("P_day_all_tot_m", "P_day_all_nWet_m", "P_day_all_P99")
#' barossa_obs_atts <- calculateAttributes(barossa_obs, attSel = attSel)
#' @export

calculateAttributes <- function(climateData, # input data in the format of tank_obs (can be reference, obs, or, future projections)
                                attSel, # vector of selected attributes
                                startYr = NULL, # changed slice & window to startYr and endYr
                                endYr = NULL # ,                    #       - can specify one without the other as well
                                #                              attCalcInfo=NULL,
                                #                              return_attCalcInfo = F
) {
  IOmode <- "verbose"
  arrayID <- NULL
  simLengthNyrs <- NULL
  file <- filename(IOmode = IOmode, arrayID = arrayID)

  input <- input_process_check(climateData, file, simLengthNyrs) # Checks for missing values/full years of data
  obs <- input

  year <- as.integer(format(obs$times, "%Y"))

  if (!is.null(startYr)) {
    if (!is.null(endYr)) {
      if (startYr > endYr) stop("startYr should be less than or equal to endYr.")
    }
    if (startYr < obs$year[1]) warning("startYr is less than the starting year of climateData.")
  } else {
    startYr <- year[1]
  }

  if (!is.null(endYr)) {
    if (endYr > utils::tail(year, 1)) warning("endYr is greater than the last year of climateData.")
  } else {
    endYr <- utils::tail(year, 1)
  }

  varNames <- names(obs)
  varNames <- varNames[!(varNames %in% c("times", "timeStep"))]

  keep <- which(year >= startYr & year <= endYr)
  for (var in varNames) {
    if (is.null(dim(obs[[var]]))) {
      obs[[var]] <- obs[[var]][keep]
    } else {
      obs[[var]] <- obs[[var]][keep, ]
    }
  }

  # Get necessary variables for historical atts
  attInfo <- attribute.info.check(attSel = attSel) # vector of selected attributes (strings)
  simVar <- attInfo$varType
  simVar <- unique(simVar)
  nvar <- length(simVar)

  simAgg <- unique(attInfo$aggType)

  timeStep <- obs$timeStep

  i <- which(timestep_rank[simAgg] < timestep_rank[aggNameShort[[timeStep]]])
  if (length(i) > 0) {
    cat("attributes require time step of", aggNameLong[[simAgg[i]]], "but data has time step", timeStep, "\n")
    stop()
  }

  # browser()

  # i = which(!simVar%in%varNames)
  # if (length(i)>0){
  #   cat('variable',simVar[i],'not in climateData\n')
  #   stop()
  # }

  datInd <- setup_datInd_agg(simAgg = simAgg, obs$times, timeStep = timeStep)

  # if (return_attCalcInfo){
  #     attCalcInfo = attribute.calculator.setup(attSel=attSel,
  #                                              datInd=datInd[[1]])
  #     return(attCalcInfo)
  # } else {
  #   attInfo$attCalcInfo = attCalcInfo
  # }

  attObs <- aggregate_calculate_attributes(
    data = obs,
    attSel = attSel,
    datInd = datInd,
    attInfo = attInfo
  )

  return(attObs)
}

########################
# names used for timesteps

aggNameLong <- list()
aggNameLong[["hour"]] <- "1 hour"
aggNameLong[["3hour"]] <- "3 hour"
aggNameLong[["12hour"]] <- "12 hour"
aggNameLong[["day"]] <- "1 day"
aggNameLong[["week"]] <- "1 week"
aggNameLong[["month"]] <- "1 month"
aggNameLong[["year"]] <- "1 year"

aggNameShort <- list()
aggNameShort[["1 hour"]] <- "hour"
aggNameShort[["3 hour"]] <- "3hour"
aggNameShort[["12 hour"]] <- "12hour"
aggNameShort[["1 day"]] <- "day"
aggNameShort[["1 week"]] <- "week"
aggNameShort[["1 month"]] <- "month"
aggNameShort[["1 year"]] <- "year"

timestep_order <- c("hour", "3hour", "12hour", "day", "week", "month", "year")
timestep_rank <- setNames(seq_along(timestep_order), timestep_order)

########################
# aggregate data to different aggregation periods
#' @importFrom dplyr '%>%'
aggregate_data <- function(data = NULL, times, timeStep, aggPeriod) {

  if (timeStep == aggNameLong[[aggPeriod]]) {
    out <- list(
      times = times,
      data = data,
      timeStep = timeStep
    )
  } else {
    
    d1 <- data.frame(timePeriod = seq(
      from = times[1],
      to = times[length(times)],
      by = aggNameLong[[aggPeriod]]
    ))

    if (!is.null(data)) {
      d2 <- data.frame(times, data)
    } else {
      d2 <- data.frame(times)
    }

    d3 <- d2 %>%
      dplyr::mutate(timePeriod = lubridate::floor_date(times, aggNameLong[[aggPeriod]])) %>%
      dplyr::group_by(timePeriod) %>%
      dplyr::summarise(sum = sum(data)) %>%
      dplyr::right_join(d1, by = dplyr::join_by(timePeriod))

    out <- list(
      times = d3$timePeriod,
      data = d3$sum,
      timeStep = aggPeriod
    )
  }

  return(out)
}

########################
# define timePeriod as global variable to avoid timePeriod appearing as an 
# undefined variable when performing devtools::check() 
utils::globalVariables("timePeriod")
########################

setup_datInd_agg <- function(simAgg, times, timeStep, nperiod = 1) {
  datInd <- list()
  for (agg in simAgg) {
    o <- aggregate_data(data = NULL, times = times, timeStep = timeStep, aggPeriod = agg)
    datInd[[agg]] <- get.date.ind(o$times, nperiod = nperiod, southHemi = TRUE)
    datInd[[agg]]$times <- o$times
  }
  return(datInd)
}

########################
# this function calculate attribute values for attSel based on data
# temporal aggregation is performed depending on attSel
aggregate_calculate_attributes <- function(data, attSel, datInd, attInfo = NULL) {
  # varList = unique(sapply(X=attSel,FUN=get.attribute.varType))
  varList <- unique(attInfo$varType)
  if (is.null(varList)) {
    browser()
  }

  # aggList = unique(sapply(X=attSel,FUN=get.attribute.aggType))
  aggList <- unique(attInfo$aggType)
  if (is.null(aggList)) {
    browser()
  }

  att.ind <- get.att.ind.withAggs(attInfo)

  varAll <- c()
  for (var in varList) {
    tmp <- strsplit(x = var, split = "[/]")[[1]]
    varAll <- c(varAll, tmp)
  }
  varAll <- unique(varAll)

  agg_data <- list()
  for (var in varAll) {
    agg_data[[var]] <- list()
    for (agg in aggList) {
      agg_data[[var]][[agg]] <- aggregate_data(
        data = data[[var]],
        times = data$times,
        timeStep = data$timeStep,
        aggPeriod = agg
      )
    }
  }

  attValues <- list() # make this into its own function (also inserted into model sequencer)
  for (v in 1:length(varList)) {
    var <- varList[v]
    # print(var)
    tmp <- strsplit(var, split = "/")[[1]]
    attValues[[v]] <- list()
    for (a in 1:length(aggList)) {
      agg <- aggList[a]
      # print(agg)

      # determine if single or multivariable attribute
      if (length(tmp) == 1) {
        data <- agg_data[[var]][[agg]]$data
      } else if (length(tmp) == 2) {
        data <- list()
        data[[1]] <- agg_data[[tmp[1]]][[agg]]$data
        data[[2]] <- agg_data[[tmp[2]]][[agg]]$data
      }

      if (any(is.na(attSel[att.ind[[var]][[agg]]]))) {
        browser()
      }

      attValues[[v]][[a]] <- attribute.calculator(
        attSel = attSel[att.ind[[var]][[agg]]],
        data = data,
        datInd = datInd[[agg]],
        attInfo = attInfo
      )
    }
  }

  attValues <- unlist(attValues)

  # revert original attributes to original order - except when dealing with multisite data
  if (length(attValues) == length(attSel)) {
    attValues <- attValues[attSel]
  } # else {
  #    browser()
  #  }

  return(attValues)
}

##########################################
