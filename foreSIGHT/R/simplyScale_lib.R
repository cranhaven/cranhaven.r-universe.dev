#######################################
##   SIMPLE SCALING FUNCTIONS     ##
#######################################

# CONTAINS
# function for scaling over a period

simple.scaling <- function(target = NULL, # extracted from matrix output of exposure space maker. annual format is a vector, passed from wrapper.
                           targetType = NULL, # diff or frac
                           data = NULL, # data series stripped of dates
                           varType = NULL, # names of data series
                           period = NULL, # number of separations for scaling e.g. annual all at once, seasonal in four stages.
                           i.pp = NULL # index to control what time step entries are changed at a time
) {
  temp <- list()
  nLoc <- list()
  for (var in varType) {
    dimVar <- dim(data[[var]])
    nLoc[[var]] <- dimVar[2]
    temp[[var]] <- list()
    temp[[var]]$sim <- matrix(NA, nrow = dimVar[1], ncol = dimVar[2])
    colnames(temp[[var]]$sim) <- colnames(data[[var]])
  }

  for (p in 1:period) { # for annual this is equal to 1
    # select target subset if doing seasonal
    #****** need method of dealing with different targets
    for (j in 1:length(varType)) { # loop for each variable, to check its scale type.
      var <- varType[j]
      switch(targetType[j],
        "frac" = {
          temp[[var]]$sim[i.pp[[p]], 1:nLoc[[var]]] <- as.matrix(data[[var]][i.pp[[p]], 1:nLoc[[var]]]) * target[j]
        },
        "diff" = {
          temp[[var]]$sim[i.pp[[p]], 1:nLoc[[var]]] <- as.matrix(data[[var]][i.pp[[p]], 1:nLoc[[var]]]) + target[j]
        },
        -99.00
      )
    }
  }
  return(temp)
}

###################################################################

seasonal.scaling <- function(target_total_fac = NULL, # change for tot/ave
                             target_seas_fac = NULL, # change for seas ratio
                             targetType = NULL, # diff or frac
                             data = NULL, # data time series
                             i.seas = NULL) { # indices for rest of year

  i.all <- 1:nrow(data)
  i.rest <- i.all[-i.seas]

  nsite <- ncol(data)
  data.new <- NA * data

  data.new <- target_total_fac * data

  for (s in 1:nsite) {
    if (targetType == "frac") {
      fac_rest_year <- (sum(data.new[, s]) - sum(data.new[i.seas, s] * target_seas_fac)) / sum(data.new[i.rest, s])
      data.new[i.seas, s] <- target_seas_fac * data.new[i.seas, s]
      data.new[i.rest, s] <- fac_rest_year * data.new[i.rest, s]
    } else {
      stop("cannot handle targetType other than fac for seasonal scaling")
    }
  }

  return(list(sim = data.new))
}

#############################
# climatology based on window about day of year (see McInerney et al, WRR, 2020)
calc_ClimDaily_dayOfYearWindow <- function(obs, dateObs, dateClim, inc) {
  dObs <- as.integer(format(dateObs, "%j"))
  leapYear <- as.integer(is.leapyear(as.integer(format(dateObs, "%Y"))))
  nYear <- round(length(dateObs) / 365.25)

  day_clim <- matrix(nrow = 366, ncol = (2 * inc + 1) * nYear)
  for (d in 1:365) {
    in_window <- (abs(dObs - d) <= inc) |
      (dObs - d) >= (365 + leapYear - inc) |
      (dObs - d + 365 + leapYear) <= inc
    day_clim[d, ] <- obs[in_window]
  }
  day_clim[366, ] <- day_clim[365, ]

  nDaysClim <- length(dateClim)
  clim <- matrix(nrow = nDaysClim, ncol = (2 * inc + 1) * nYear)
  for (n in 1:nDaysClim) {
    d <- as.integer(format(dateClim[n], "%j"))
    clim[n, ] <- day_clim[d, ]
  }

  return(clim)
}

# #############################

is.leapyear <- function(year) {
  # http://en.wikipedia.org/wiki/Leap_year
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

###################################################################

# TEST
# simple.scaling(target=target,targetType=targetType, data=mat,varType=varType,period=1,index=i.pp )

#----------------------------------------------
# try to imitate format seen in generic generation functions in WGEN_lib.R
