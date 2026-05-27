##################

calc_timeStep <- function(times) {
  dtDays <- difftime(times[2], times[1], units = "days")
  if (dtDays == 1) {
    timeStep <- "1 day"
  } else if (dtDays %in% c(28, 30, 31)) {
    timeStep <- "1 month"
  } else if (dtDays %in% c(365, 366)) {
    timeStep <- "1 year"
  } else if (dtDays == 1 / 24) {
    timeStep <- "1 hour"
  } else if (dtDays == 1 / 24 / 60) {
    timeStep <- "1 minute"
  }

  return(timeStep)
}

##################

#' Converts "old" reference climate data format (<V1.2) to "new" format with POSIXct dates.
#'
#' \code{convert_climYMD_POSIXct} produces reference climate data list for use in V2.0 and newer.
#' @param clim data.frame or list; contains reference daily climate data from foreSIGHT V1.2 or earlier. \cr
#' @return The function returns a list containing \code{times} in POSIXct format (corresponding to year, month and day from original data \code{clim}), and climate variables.
#' @export
convert_climYMD_POSIXct <- function(clim) {
  clim <- as.list(clim)

  times <- as.POSIXct(
    paste0(
      clim$year, "/",
      clim$month, "/",
      clim$day
    ),
    tz = "UTC"
  )

  clim$year <- clim$month <- clim$day <- NULL
  clim$times <- times

  return(clim)
}
##################

#' Create foreSIGHT reference climate object from time information and climate data.
#'
#' \code{create_clim} produces reference climate data list.
#' @param timeStart a character; the first time for the climate data
#' @param timeEnd a character; the last time
#' @param timeStep a character; the time step, containing one of "hour", "day", "week", "month" or "year"
#' @param fmt a character; format of \code{timeStart} and \code{timeEnd}
#' @param tz a character; the timezone. Default is 'UTC', which avoids issues with daylight savings.
#' @param ... vectors or matrices; climate data objects which will be added to the output list.
#' @return The function returns a list containing \code{times} in POSIXct format and climate data.
#' @examples
#' # create small reference climate list (only 10 days)
#' clim <- create_clim(
#'   timeStart = "2007/01/01", # start date
#'   timeEnd = "2007/01/10", # end date
#'   timeStep = "day", # time step
#'   fmt = "%Y/%m/%d", # format of start/end dates
#'   P = c(0, 0, 0, 0, 0, 4.5, 2.6, 0, 0, 0), # precip data
#'   Temp = c(25, 24, 30, 32, 33, 27, 21, 21, 22, 30) # temperature data
#' )
#' @export
create_clim <- function(timeStart, timeEnd, timeStep, fmt = NULL, tz = "UTC", ...) {
  vars <- list(...)

  timeStart <- as.POSIXct(timeStart, tz = tz, fmt = fmt)
  timeEnd <- as.POSIXct(timeEnd, tz = tz, fmt = fmt)

  times <- seq(timeStart, timeEnd, by = timeStep)

  clim <- list(times = times)

  for (varName in names(vars)) {
    clim[[varName]] <- vars[[varName]]
  }

  return(clim)
}
##################




#########################################
##   CHECKS FOR DATA INPUT TIMESERIES  ##
#########################################

# CONTAINS
# input checking for data frame

# obs has list format. contains obs$times, obs$timeStep (optional), and variables (e.g. obs$P).
# assume missing data is NA

input_process_check <- function(obs,
                                file,
                                simLengthNyrs = NULL) { # This input has its dates in first three columns

  # check times exist
  if (is.null(obs$times)) {
    logfile("Error: Observed data must have variable times ", file)
    logfile("Program terminated", file)
    stop("Observed data must have variable 'times'")
  }

  # names of observed variables
  obsVars <- names(obs)[-which(names(obs) %in% c("times", "timeStep"))]

  times <- obs$times
  nT <- length(times)
  year <- as.integer(format(times, "%Y"))

  # convert data vectors to matrices so that all are matrices.
  for (var in obsVars) {
    if (is.vector(obs[[var]])) {
      obs[[var]] <- as.matrix(obs[[var]], ncol = 1)
    }
  }

  # check times and data have same length
  for (var in obsVars) {
    if (dim(obs[[var]])[1] != nT) {
      logfile("Error: Observed data has different length from times ", file)
      logfile("Program terminated", file)
      stop("Observed data has different length from times")
    }
  }

  # calculate time step from data
  timeStep <- obs$timeStep
  if (is.null(timeStep)) {
    timeStep <- calc_timeStep(times)
  }
  if (is.null(timeStep)) {
    logfile("Error: Unable to determine data time step", file)
    logfile("Program terminated", file)
    stop("Unable to determine data time step - specify obs$timeStep")
  }

  ####################
  # TRUNCATE TO START AND END AT WHOLE YEAR

  ##########
  # calculate which times to keep
  firstYr <- year[1]
  lastYr <- year[nT]

  keep <- 1:nT

  firstTimeFirstYr <- as.POSIXct(paste0(firstYr, "/1/1 0:0:0"), tz = "UTC")
  if (firstTimeFirstYr != times[1]) {
    keep <- keep[year != firstYr]
  }

  firstTimeAfterLastYr <- as.POSIXct(paste0((lastYr + 1), "/1/1 0:0:0"), tz = "UTC")
  lastTimeLastYr <- seq(firstTimeAfterLastYr, by = paste0("-", timeStep), length = 2)[2]
  if (lastTimeLastYr != times[nT]) {
    keep <- keep[year != lastYr]
  }

  ##########
  # select subset of data

  nT <- length(keep)
  times <- times[keep]

  obsNew <- list()
  obsNew$times <- times
  obsNew$timeStep <- timeStep
  for (var in obsVars) {
    obsNew[[var]] <- obs[[var]][keep, ]
    if (is.vector(obsNew[[var]])) {
      obsNew[[var]] <- as.matrix(obsNew[[var]], ncol = 1)
    }
  }

  ####################

  # ERROR VALUES/MISSING VALUES
  M <- sapply(obs, function(x) sum(is.na(x))) # this might need to be fixed up when we have multiple cats and some missing data
  for (Var in obsVars) {
    if (M[Var] > 0) {
      missing <- which(is.na(obs[[Var]]))
      warn(p("Missing entries in ", Var), file)
      cat(apply(times[missing], 1, paste, collapse = "-"), sep = "\n")
      cat("\n")
      logfile(apply(times, 1, paste, collapse = "-"), file)
    }
  }

  if (sum(M) > 0) {
    logfile("Error: There are missing data entries in the variables", file)
    logfile("Program terminated", file)
    stop("There are missing data entries in the variables")
  }

  # CHECK IF SIMLENGTHNYRS> SUPPLIED DATA
  if (!is.null(simLengthNyrs)) {
    last <- year[1] + simLengthNyrs - 1
    obsLast <- year[nT]
    if (last < obsLast) {
      logfile("Error: Simulation length requested is shorter than observed data", file)
      logfile("Program terminated", file)
      stop("Simulation length requested is shorter than observed data")
    }
  }

  # warning if < 10 years data
  nDays <- difftime(seq(times[nT], by = timeStep, length = 2)[2], times[1], units = "days")
  if (nDays < 3650) {
    warn("You have provided less than 10 years of data", file)
  }

  # check if all times are there
  temp <- as.numeric(length(seq(times[1], times[nT], by = timeStep)))
  if (nT != temp) {
    logfile("Error: There are missing dates from the provided data. Ensure leap days are included", file)
    logfile("Program terminated", file)
    stop("There are missing dates from the provided data. Ensure leap days are included")
  }

  return(obsNew)
} # END
