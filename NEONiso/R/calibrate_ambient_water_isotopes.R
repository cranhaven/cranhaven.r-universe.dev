#' calibrate_ambient_water_isotopes
#'
#' Function called by `calibrate_ambient_water_linreg` to apply
#' slope and intercept parameters to the ambient datasets (000_0x0_09m and
#' 000_0x0_30m) to correct to the VSMOW scale.
#' This function should generally not be used independently,
#' but should be used with `calibrate_ambient_water_linreg`.
#' Note that in this version *NO CORRECTION FOR HUMIDITY* is performed.
#' Use with caution.
#' 
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param amb_data_list List containing ambient d18O/d2H datasets.
#'             Will include all variables in 000_0x0_xxm. (character)
#' @param caldf Calibration data frame containing slope and intercept values
#'             for d18O and d2H values.
#' @param outname Output variable name. Inherited from
#'             `calibrate_ambient_water_linreg`
#' @param site Four-letter NEON code corresponding to site being processed.
#' @param force_to_end In given month, calibrate ambient data later than last
#'             calibration, using the last calibration? (default true)
#' @param force_to_beginning In given month, calibrate ambient data before than
#'             first calibration, using the first calibration? (default true)
#' @param r2_thres Minimum r2 value for calibration to be considered "good" and
#'             applied to ambient data.
#' @param filter_data Apply a median filter to output ambient data? inherited.
#'
#' @return Nothing to environment; returns calibrated ambient observations to
#'     the output file. This function is not designed to be called on its own.
#' @export
#'
#' @importFrom magrittr %>%
calibrate_ambient_water_linreg <- function(amb_data_list,
                                           caldf,
                                           outname,
                                           site,
                                           filter_data = TRUE,
                                           force_to_end = TRUE,
                                           force_to_beginning = TRUE,
                                           r2_thres = 0.9) {

  # print status.
  print("Processing water ambient data...")

  # In contrast to carbon calibration - need to get both 18O and 2H separately
  oxydf <- amb_data_list$dlta18OH2o
  hyddf <- amb_data_list$dlta2HH2o

  #-------------------------------------------------------
  # oxygen.
  #-------------------------------------------------------
  # ensure that time variables are in POSIXct.
  # (note: these should be the same for 18O and 2H?)
  amb_start_times <- convert_NEONhdf5_to_POSIXct_time(oxydf$timeBgn)
  amb_end_times <- convert_NEONhdf5_to_POSIXct_time(oxydf$timeEnd)

  # if force.to.end and/or force.to.beginning are true, match out$start[1]
  # to min(amb time) and/or out$end[nrow] to max(amb time)

  if (force_to_end == TRUE) {
    caldf$end[nrow(caldf)] <- amb_end_times[length(amb_end_times)]
  }
  if (force_to_beginning == TRUE) {
    caldf$start[1] <- amb_start_times[1]
  }

  # determine which cal period each ambient data belongs to.
  var_inds_in_calperiod <- list()

  for (i in seq_len(nrow(caldf))) {
    int <- lubridate::interval(caldf$timeBgn[i], caldf$timeEnd[i])
    var_inds_in_calperiod[[i]] <- which(amb_end_times %within% int)
  }

  # calibrate data at this height.
  oxydf$mean_cal <- oxydf$mean
  oxydf$max_cal  <- oxydf$max
  oxydf$min_cal  <- oxydf$min

  for (i in seq_along(var_inds_in_calperiod)) {
    if (!is.na(caldf$r2_18O[i]) && caldf$r2_18O[i] > r2_thres) {

      oxydf$mean_cal[var_inds_in_calperiod[[i]]] <- caldf$intercept18O[i] +
        oxydf$mean[var_inds_in_calperiod[[i]]] * caldf$slope18O[i]
      oxydf$min_cal[var_inds_in_calperiod[[i]]] <- caldf$intercept18O[i] +
        oxydf$min[var_inds_in_calperiod[[i]]] * caldf$slope18O[i]
      oxydf$max_cal[var_inds_in_calperiod[[i]]] <- caldf$intercept18O[i] +
        oxydf$max[var_inds_in_calperiod[[i]]] * caldf$slope18O[i]

    } else {

      oxydf$mean_cal[var_inds_in_calperiod[[i]]] <- NA
      oxydf$min_cal[var_inds_in_calperiod[[i]]]  <- NA
      oxydf$max_cal[var_inds_in_calperiod[[i]]]  <- NA

    }

  }

  # apply median filter to data
  if (filter_data == TRUE) {
    oxydf$mean_cal     <- filter_median_brock86(oxydf$mean_cal)
    oxydf$min_cal      <- filter_median_brock86(oxydf$min_cal)
    oxydf$max_cal      <- filter_median_brock86(oxydf$max_cal)
  }

  # replace ambdf in amb_data_list
  amb_data_list$dlta18OH2o <- oxydf

  #-------------------------------------------------------
  # hydrogen.
  #-------------------------------------------------------
  rm(amb_start_times, oxydf, amb_end_times, i, var_inds_in_calperiod)

  # ensure that time variables are in POSIXct.
  amb_start_times <- convert_NEONhdf5_to_POSIXct_time(hyddf$timeBgn)
  amb_end_times   <- convert_NEONhdf5_to_POSIXct_time(hyddf$timeEnd)

  # if force.to.end and/or force.to.beginning are true,
  # match out$start[1] to min(amb time) and/or out$end[nrow] to max(amb time)

  if (force_to_end == TRUE) {
    caldf$end[nrow(caldf)] <- amb_end_times[length(amb_end_times)]
  }
  if (force_to_beginning == TRUE) {
    caldf$start[1] <- amb_start_times[1]
  }

  # determine which cal period each ambient data belongs to.
  var_inds_in_calperiod <- list()

  for (i in seq_len(nrow(caldf))) {
    int <- lubridate::interval(caldf$timeBgn[i], caldf$timeEnd[i])
    var_inds_in_calperiod[[i]] <- which(amb_end_times %within% int)
  }

  hyddf$mean_cal <- hyddf$mean
  hyddf$max_cal  <- hyddf$max
  hyddf$min_cal  <- hyddf$min

  for (i in seq_along(var_inds_in_calperiod)) {

    if (!is.na(caldf$r2_2H[i]) && caldf$r2_2H[i] > r2_thres) {

      hyddf$mean_cal[var_inds_in_calperiod[[i]]] <- caldf$intercept2H[i] +
        hyddf$mean[var_inds_in_calperiod[[i]]] * caldf$slope2H[i]
      hyddf$min_cal[var_inds_in_calperiod[[i]]] <- caldf$intercept2H[i] +
        hyddf$min[var_inds_in_calperiod[[i]]] * caldf$slope2H[i]
      hyddf$max_cal[var_inds_in_calperiod[[i]]] <- caldf$intercept2H[i] +
        hyddf$max[var_inds_in_calperiod[[i]]] * caldf$slope2H[i]

    } else {

      hyddf$mean_cal[var_inds_in_calperiod[[i]]] <- NA
      hyddf$min_cal[var_inds_in_calperiod[[i]]] <- NA
      hyddf$max_cal[var_inds_in_calperiod[[i]]] <- NA

    }

  }

  # apply median filter to data
  if (filter_data == TRUE) {
    hyddf$mean_cal <- filter_median_brock86(hyddf$mean_cal)
    hyddf$min_cal  <- filter_median_brock86(hyddf$min_cal)
    hyddf$max_cal  <- filter_median_brock86(hyddf$max_cal)
  }

  # replace ambdf in amb_data_list
  amb_data_list$dlta2HH2o <- hyddf

  # return list
  return(amb_data_list)
}
