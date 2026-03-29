calibrate_ambient_carbon_Bowling2003 <- function(...) {
  lifecycle::deprecate_soft("0.7.1", "calibrate_ambient_carbon_Bowling2003()",
                            "calibrate_ambient_carbon_gainoffset()")
  calibrate_ambient_carbon_gainoffset(...)
}

#' Calibrate ambient carbon isotope data using gain-and-offset method
#'
#' Function called by `calibrate_carbon_bymonth()` to apply
#' gain and offset parameters to the ambient datasets (000_0x0_09m and
#' 000_0x0_30m). This function should generally not be used independently,
#' but should be used in coordination with
#' `calibrate_carbon_bymonth()`.
#' 
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param amb_data_list List containing an ambient d13C dataset.
#'                      Will include all variables in 000_0x0_xxm. (character)
#' @param caldf Calibration data frame containing gain and offset values for
#'              12C and 13C isotopologues.
#' @param site Four-letter NEON code corresponding to site being processed.
#' @param filter_data Apply median absolute deviation filter from Brock 86 to
#'             remove impulse spikes?
#' @param force_to_end In given month, calibrate ambient data later than last
#'             calibration, using the last calibration? (default true)
#' @param force_to_beginning In given month, calibrate ambient data before than
#'              first calibration, using the firs t calibration? (default true)
#' @param r2_thres Minimum r2 value for calibration to be considered "good" and
#'             applied to ambient data.
#' @param gap_fill_parameters Should function attempt to 'gap-fill' across a
#'            bad calibration by carrying the last known good calibration
#'            forward? Implementation is fairly primitive currently, as
#'            it only carries the last known good calibration that's
#'            available forward rather than interpolating, etc. Default FALSE.
#'
#' @return Depends on `write_to_file` argument.
#'    If true, returns nothing to environment;
#'    but returns calibrated ambient observations to the output file.
#'    If false, returns modified version of amb_data_list that include
#'    calibrated ambient data.
#'
#'
#' @importFrom magrittr %>%
#' @importFrom lubridate %within%
#'
calibrate_ambient_carbon_gainoffset <- function(amb_data_list,
                                                caldf,
                                                site,
                                                filter_data = TRUE,
                                                force_to_end = TRUE,
                                                force_to_beginning = TRUE,
                                                gap_fill_parameters = FALSE,
                                                r2_thres = 0.9) {

  #-----------------------------------------------------------
  # should be able to get a calGainsOffsets object from the H5 file.

  # only working on the d13C of the amb_data_list, so extract just this...
  amb_delta <- amb_data_list$dlta13CCo2
  amb_co2   <- amb_data_list$rtioMoleDryCo2

  # in some cases, there's an issue where nrow(amb_delta) and nrow(amb_co2)
  # are not the same - time arrays need to be merged prior to continuing.
  if (!setequal(amb_delta$timeBgn, amb_co2$timeBgn)) {

    # get rows that are only present in both data frames
    amb_co2   <- amb_co2[amb_co2$timeBgn %in% amb_delta$timeBgn, ]
    amb_delta <- amb_delta[amb_delta$timeBgn %in% amb_co2$timeBgn, ]

  }

  # instead of using the [12CO2] and [13CO2] values, calculate from
  # the isotope ratio instead.
  amb_12co2 <- amb_13co2 <- amb_co2

  amb_12co2$mean <- calculate_12CO2(amb_co2$mean, amb_delta$mean)
  amb_13co2$mean <- calculate_13CO2(amb_co2$mean, amb_delta$mean)

  amb_12co2$min  <- calculate_12CO2(amb_co2$min, amb_delta$min)
  amb_13co2$min  <- calculate_13CO2(amb_co2$min, amb_delta$min)

  amb_12co2$max  <- calculate_12CO2(amb_co2$max, amb_delta$max)
  amb_13co2$max  <- calculate_13CO2(amb_co2$max, amb_delta$max)

  # ensure that time variables are in POSIXct.
  amb_start_times <- convert_NEONhdf5_to_POSIXct_time(amb_delta$timeBgn)
  amb_end_times   <- convert_NEONhdf5_to_POSIXct_time(amb_delta$timeEnd)

  # if force_to_end and/or force_to_beginning are true, match
  # out$start[1] to min(amb time) and/or out$end[nrow] to max(amb time)

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

    if (gap_fill_parameters) {

      # print notice that we're gap filling
      print("Gap filling calibrations...")
      # 12CO2 calibration parameters.
      if (!is.na(caldf$r2_12C[i]) && caldf$r2_12C[i] < r2_thres) {
        # if we're in calibration period 2 or later, carry previous
        # calibration period forward. else if the first calibration period
        # is bad, find the first good calibration period at index n,
        # and apply to first n periods.
        if (i > 1) {
          caldf$gain12C[i] <- caldf$gain12C[i - 1]
          caldf$offset12C[i] <- caldf$offset12C[i - 1]
          caldf$r2_12C[i] <- caldf$r2_12C[i - 1]
        } else { # i = 1, and need to find first good value.
          first_good_val <- min(which(caldf$r2_12C > r2_thres))
          caldf$gain12C[i] <- caldf$gain12C[first_good_val]
          caldf$offset12C[i] <- caldf$offset12C[first_good_val]
          caldf$r2_12C[i] <- caldf$r2_12C[first_good_val]
        }
      }

      # 13CO2 calibration parameters - equivalent logic to 12Co2.
      if (!is.na(caldf$r2_13C[i]) && caldf$r2_13C[i] < r2_thres) {
        if (i > 1) {
          caldf$gain13C[i] <- caldf$gain13C[i - 1]
          caldf$offset13C[i] <- caldf$offset13C[i - 1]
          caldf$r2_13C[i] <- caldf$r2_13C[i - 1]
        } else {
          first_good_val <- min(which(caldf$r2_13C > r2_thres))
          caldf$gain13C[i] <- caldf$gain13C[first_good_val]
          caldf$offset13C[i] <- caldf$offset13C[first_good_val]
          caldf$r2_13C[i] <- caldf$r2_13C[first_good_val]
        }
      }
    }
  }

  # calibrate data at this height.
  #-------------------------------------
  # extract 12CO2 and 13CO2 concentrations from the ambient data,
  # set as NA, unless overwritten

  # placeholders for 12CO2 vecs
  mean12c <- max12c <- min12c <- rep(NA, length(amb_delta$mean))
  cv5rmse12c <- cvloo12c      <- rep(NA, length(amb_delta$mean))
  # placeholders for 13CO2 vecs
  mean13c <- max13c <- min13c <- rep(NA, length(amb_delta$mean))
  cv5rmse13c <- cvloo13c      <- rep(NA, length(amb_delta$mean))

  for (i in seq_len(length(var_inds_in_calperiod))) {
    # calculate calibrated 12CO2 concentrations
    mean12c[var_inds_in_calperiod[[i]]] <- caldf$gain12C[i] *
      amb_12co2$mean[var_inds_in_calperiod[[i]]] + caldf$offset12C[i]
    min12c[var_inds_in_calperiod[[i]]] <- caldf$gain12C[i] *
      amb_12co2$min[var_inds_in_calperiod[[i]]] + caldf$offset12C[i]
    max12c[var_inds_in_calperiod[[i]]] <- caldf$gain12C[i] *
      amb_12co2$max[var_inds_in_calperiod[[i]]] + caldf$offset12C[i]
    cv5rmse12c[var_inds_in_calperiod[[i]]] <- caldf$cv5rmse_12C[i]
    cvloo12c[var_inds_in_calperiod[[i]]] <- caldf$cvloo_12C[i]

    # calculate calibrated 13CO2 concentrations
    mean13c[var_inds_in_calperiod[[i]]] <- caldf$gain13C[i] *
      amb_13co2$mean[var_inds_in_calperiod[[i]]] + caldf$offset13C[i]
    min13c[var_inds_in_calperiod[[i]]] <- caldf$gain13C[i] *
      amb_13co2$min[var_inds_in_calperiod[[i]]] + caldf$offset13C[i]
    max13c[var_inds_in_calperiod[[i]]] <- caldf$gain13C[i] *
      amb_13co2$max[var_inds_in_calperiod[[i]]] + caldf$offset13C[i]
    cv5rmse13c[var_inds_in_calperiod[[i]]] <- caldf$cv5rmse_13C[i]
    cvloo13c[var_inds_in_calperiod[[i]]] <- caldf$cvloo_13C[i]

  }

  # output calibrated delta values.
  amb_delta$mean_cal <- round(R_to_delta(mean13c / mean12c, "carbon"), 2)
  amb_delta$min_cal  <- round(R_to_delta(min13c / min12c, "carbon"), 2)
  amb_delta$max_cal  <- round(R_to_delta(max13c / max12c, "carbon"), 2)
  ### amb_delta$vari  could be <- round(amb_delta$vari, 2) # <- not sure why,
  # but this crashes code some times as ab_delta$vari is not numeric??

  # calibrate co2 mole fractions and uncertainty
  amb_co2$mean_cal <- (mean13c + mean12c) / (1 - 0.00474)

  # apply median filter to data
  if (filter_data == TRUE) {
    amb_delta$mean_cal <- filter_median_brock86(amb_delta$mean_cal)
    amb_delta$min_cal  <- filter_median_brock86(amb_delta$min_cal)
    amb_delta$max_cal  <- filter_median_brock86(amb_delta$max_cal)
  }

  # calculate uncertainties:
  # save this ucrt propogation for later version.
  amb_delta$CVcalUcrt <- round(abs(amb_delta$mean_cal) *
                                 sqrt((cv5rmse12c / mean12c)^2 +
                                        (cv5rmse13c / mean13c)^2), 3)
  amb_delta$LOOcalUcrt <- round(abs(amb_delta$mean_cal) *
                                  sqrt((cvloo12c / mean12c)^2 +
                                         (cvloo13c / mean13c)^2), 3)
  amb_co2$CVcalUcrt   <- round(sqrt(cv5rmse12c^2 + cv5rmse13c^2), 3)
  amb_co2$LOOcalUcrt  <- round(sqrt(cvloo12c^2 + cvloo13c^2), 3)

  # replace ambdf in amb_data_list, return amb_data_list
  amb_data_list$dlta13CCo2 <- amb_delta

  amb_data_list$rtioMoleDryCo2 <- amb_co2

  return(amb_data_list)
}
