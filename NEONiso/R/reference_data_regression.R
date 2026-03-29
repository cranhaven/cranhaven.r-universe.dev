#' Leave-one-out cross validation
#'
#' Calculate analytic leave-one-out cross variance error estimate
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param mod Fitted model to estimate leave-one-out CV on.
#'
loocv <- function(mod) {
  h <- stats::lm.influence(mod)$hat
  # might need to add na.rm
  return(base::mean((stats::residuals(mod) / (1 - h)) ^ 2))
}

#' Produce estimates of the calibration error.
#'
#' Estimate calibration error using a 5-fold cross-validation. A 5-fold
#' cross-validation was chosen as each calibration window should have
#' at least 6 data points (e.g., if only daily validation data are used for the
#' calibration) and therefore this ensures that the cross-validation should
#' always run. Model is fit using \code{lm}, with root-mean-square error
#' (RMSE) and mean-absolute error (MAE) extracted from the cross-validation.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param data Data frame to perform cross-validation on.
#' @param formula Formula to pass to lm for cross validation.
#'
estimate_calibration_error <- function(formula, data) {

  data <- as.data.frame(data)

  # remove incomplete cases for the formula variables
  vars <- all.vars(formula)
  complete <- stats::complete.cases(data[vars])
  data <- data[complete, ]

  n <- nrow(data)
  if (n < 5) {
    return(data.frame(RMSE = NA_real_, MAE = NA_real_))
  }

  # assign folds
  fold_ids <- rep(seq_len(5), length.out = n)
  fold_ids <- fold_ids[sample(n)]

  resp_var <- vars[1]
  errors <- numeric(n)

  for (k in seq_len(5)) {
    test_idx <- which(fold_ids == k)
    train_idx <- which(fold_ids != k)
    mod <- stats::lm(formula, data = data[train_idx, ])
    pred <- stats::predict(mod, newdata = data[test_idx, ])
    errors[test_idx] <- data[[resp_var]][test_idx] - pred
  }

  output <- data.frame(
    RMSE = sqrt(mean(errors^2)),
    MAE = mean(abs(errors))
  )

  return(output)

}

#' Estimate slope/intercept of carbon isotope calibration regression
#'
#' Performs regression between measured and known carbon isotope and mole
#' fractions to generate a transfer function and associated uncertainty
#' estimates using both 5-fold and leave-one-out cross-validation methods.
#' Regression occurs either on 12CO2/13CO2 mole fractions (gainoffset method)
#' or on the CO2 and d13C values (linreg). 
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param method Are we using the gain-and-offset method
#'              ("gainoffset"), formerly called the Bowling et al. 2003 method
#'              in this package, or direct linear regression of
#'              d13C and CO2 mole fractions ("linreg")?
#' @param calibration_half_width Determines the period (in days)
#'        from which reference data are selected (period
#'        is 2*calibration_half_width).
#' @param ref_data Reference data.frame from which to estimate
#'        calibration parameters.
#' @param plot_regression_data True or false - should we plot the data used in
#'        the regression? Useful for debugging.
#' @param plot_dir If plot_regression_data is true, where should the
#'        plots be saved?
#' @param site Needed for regression plots.
#' @param min_nobs Minimum number of high-frequency observations
#'                 to define a peak.
#'
#' @return Returns a data.frame of calibration parameters. If
#'        `method == "gainoffset"`, then data.frame includes
#'        gain and offset parameters for 12CO2 and 13CO2, and r^2
#'        values for each regression. If `method == "linreg"`,
#'        then data.frame includes slope, intercept, and r^2 values
#'        for d13C and CO2 values.
#'
#' @importFrom stats coef lm
#' @importFrom magrittr %>%

fit_carbon_regression <- function(ref_data, method, calibration_half_width,
                                  plot_regression_data = FALSE,
                                  plot_dir = "/dev/null",
                                  site,
                                  min_nobs = NA) {

  # First, identify year/month combination and then select data.
  yrmn <- paste(lubridate::year(ref_data$timeBgn)[1],
                lubridate::month(ref_data$timeBgn)[1],
                sep = "-")

  # validate yrmn - is it actually a date? if no refdata, then no.
  #---------------------------------------------------------------
  # Select which validation data to carry through to calibration
  #---------------------------------------------------------------
  ref_data <- select_daily_reference_data(ref_data,
                                          analyte = "co2",
                                          min_nobs = min_nobs)

  if (method == "gainoffset" || method == "Bowling_2003") {

    # calculate mole fraction (12CO2 / 13CO2) for ref gases and observed values
    ref_data$conc12CCO2_ref <- calculate_12CO2(ref_data$rtioMoleDryCo2Refe.mean,
                                               ref_data$dlta13CCo2Refe.mean)
    ref_data$conc13CCO2_ref <- calculate_13CO2(ref_data$rtioMoleDryCo2Refe.mean,
                                               ref_data$dlta13CCo2Refe.mean)
    ref_data$conc12CCO2_obs <- calculate_12CO2(ref_data$rtioMoleDryCo2.mean,
                                               ref_data$dlta13CCo2.mean)
    ref_data$conc13CCO2_obs <- calculate_13CO2(ref_data$rtioMoleDryCo2.mean,
                                               ref_data$dlta13CCo2.mean)

    if (nrow(ref_data) == 0) {

      # output dataframe giving valid time range, slopes, intercepts, rsquared.
      out <- data.frame(timeBgn = as.POSIXct(yrmn,
                                             format = "%Y-%m",
                                             tz = "UTC",
                                             origin = "1970-01-01"),
                        timeEnd = as.POSIXct(yrmn,
                                             format = "%Y-%m",
                                             tz = "UTC",
                                             origin = "1970-01-01"),
                        gain12C = as.numeric(NA),
                        gain13C = as.numeric(NA),
                        offset12C = as.numeric(NA),
                        offset13C = as.numeric(NA),
                        r2_12C = as.numeric(NA),
                        r2_13C = as.numeric(NA),
                        cvloo_12C = as.numeric(NA),
                        cvloo_13C = as.numeric(NA),
                        cv5mae_12C = as.numeric(NA),
                        cv5mae_13C = as.numeric(NA),
                        cv5rmse_12C = as.numeric(NA),
                        cv5rmse_13C = as.numeric(NA))
    } else {

      # get start and end days.
      start_date <- as.Date(min(ref_data$timeBgn))
      end_date   <- as.Date(max(ref_data$timeEnd))

      # generate date sequence
      date_seq <- base::seq.Date(start_date, end_date, by = "1 day")
      n_days <- length(date_seq)

      # pre-allocate output with correct size
      out <- data.frame(gain12C     = numeric(length = n_days),
                        gain13C     = numeric(length = n_days),
                        offset12C   = numeric(length = n_days),
                        offset13C   = numeric(length = n_days),
                        r2_12C      = numeric(length = n_days),
                        r2_13C      = numeric(length = n_days),
                        cvloo_12C   = numeric(length = n_days),
                        cvloo_13C   = numeric(length = n_days),
                        cv5mae_12C  = numeric(length = n_days),
                        cv5mae_13C  = numeric(length = n_days),
                        cv5rmse_12C = numeric(length = n_days),
                        cv5rmse_13C = numeric(length = n_days))

      # vectorize time construction
      start_time <- as.POSIXct(paste(date_seq, "00:00:00.0001"),
                               tz = "UTC", origin = "1970-01-01")
      end_time   <- as.POSIXct(paste(date_seq, "23:59:59.0000"),
                               tz = "UTC", origin = "1970-01-01")

      # hoist constants out of loop
      delta <- lubridate::ddays(calibration_half_width)
      fmla_12c <- stats::formula(conc12CCO2_ref ~ conc12CCO2_obs)
      fmla_13c <- stats::formula(conc13CCO2_ref ~ conc13CCO2_obs)

      # okay, now run calibrations...

      for (i in seq_along(date_seq)) {

        # define calibration interval
        cal_period <- lubridate::interval(date_seq[i] - delta,
                                          date_seq[i] + delta)

        # select data subset using the interval
        cal_subset <- ref_data %>%
          dplyr::filter(.data$timeBgn %within% cal_period)

        if (plot_regression_data) {
          print("Plotting regression data...this step will take some time...")
          carbon_regression_plots(cal_subset,
                                  plot_filename = paste0(plot_dir,
                                                         "/", site,
                                                         "_", date_seq[i],
                                                         ".pdf"),
                                  method = method,
                                  mtitle = paste0(site, date_seq[i]))
        }
        #---------------------------------------------
        # do some light validation of these points.
        cal_subset <- cal_subset %>%
          dplyr::filter(.data$dlta13CCo2.vari < 5 &
                          abs(.data$rtioMoleDryCo2.mean -
                                .data$rtioMoleDryCo2Refe.mean) < 10 &
                          abs(.data$dlta13CCo2.mean -
                                .data$dlta13CCo2Refe.mean) < 5)

        if (length(unique(cal_subset$verticalPosition)) >= 2 &&
              !all(is.na(cal_subset$dlta13CCo2.mean)) &&
              !all(is.na(cal_subset$dlta13CCo2Refe.mean))) {

          tmpmod12c <- stats::lm(fmla_12c, data = cal_subset)
          tmpmod13c <- stats::lm(fmla_13c, data = cal_subset)

          # calculate gain and offset values.
          out$gain12C[i]   <- stats::coef(tmpmod12c)[[2]]
          out$gain13C[i]   <- stats::coef(tmpmod13c)[[2]]
          out$offset12C[i] <- stats::coef(tmpmod12c)[[1]]
          out$offset13C[i] <- stats::coef(tmpmod13c)[[1]]

          # extract r2 (cache summary to avoid recomputation)
          sum12c <- summary(tmpmod12c)
          sum13c <- summary(tmpmod13c)
          out$r2_12C[i] <- sum12c$r.squared
          out$r2_13C[i] <- sum13c$r.squared

          # extract leave-one-out CV value
          out$cvloo_12C[i] <- loocv(tmpmod12c)
          out$cvloo_13C[i] <- loocv(tmpmod13c)

          # get cv5 values
          cv12c <- estimate_calibration_error(fmla_12c, cal_subset)
          cv13c <- estimate_calibration_error(fmla_13c, cal_subset)

          # assign cv values:
          out$cv5mae_12C[i] <- cv12c$MAE
          out$cv5mae_13C[i] <- cv13c$MAE
          out$cv5rmse_12C[i] <- cv12c$RMSE
          out$cv5rmse_13C[i] <- cv13c$RMSE

        } else {

          out$gain12C[i]      <- NA
          out$gain13C[i]      <- NA
          out$offset12C[i]    <- NA
          out$offset13C[i]    <- NA
          out$r2_12C[i]       <- NA
          out$r2_13C[i]       <- NA
          out$cvloo_12C[i]    <- NA
          out$cvloo_13C[i]    <- NA
          out$cv5mae_12C[i]   <- NA
          out$cv5mae_13C[i]   <- NA
          out$cv5rmse_12C[i]  <- NA
          out$cv5rmse_13C[i]  <- NA
        }
      }

      # output dataframe giving valid time range, slopes, intercepts, rsquared.
      out$timeBgn <- start_time
      out$timeEnd <- end_time

      # re-order columns to ensure that they are consistent across methods
      out <- out[, c("timeBgn", "timeEnd",
                     "gain12C", "offset12C", "r2_12C",
                     "cvloo_12C", "cv5mae_12C", "cv5rmse_12C",
                     "gain13C", "offset13C", "r2_13C",
                     "cvloo_13C", "cv5mae_13C", "cv5rmse_13C")]

    }
  } else if (method == "linreg") {

    if (nrow(ref_data) == 0) {
      # output dataframe giving valid time range, slopes, intercepts, rsquared.
      out <- data.frame(timeBgn = as.POSIXct(yrmn,
                                             format = "%Y-%m",
                                             tz = "UTC",
                                             origin = "1970-01-01"),
                        timeEnd = as.POSIXct(yrmn,
                                             format = "%Y-%m",
                                             tz = "UTC",
                                             origin = "1970-01-01"),
                        d13C_slope     = as.numeric(NA),
                        d13C_intercept = as.numeric(NA),
                        d13C_r2        = as.numeric(NA),
                        d13C_cvloo     = as.numeric(NA),
                        d13C_cv5mae    = as.numeric(NA),
                        d13C_cv5rmse   = as.numeric(NA),
                        co2_slope      = as.numeric(NA),
                        co2_intercept  = as.numeric(NA),
                        co2_r2         = as.numeric(NA),
                        co2_cvloo      = as.numeric(NA),
                        co2_cv5mae     = as.numeric(NA),
                        co2_cv5rmse    = as.numeric(NA))
    } else {

      # get start and end days.
      start_date <- as.Date(min(ref_data$timeBgn))
      end_date   <- as.Date(max(ref_data$timeEnd))

      # generate date sequence
      date_seq <- base::seq.Date(start_date, end_date, by = "1 day")
      n_days <- length(date_seq)

      # pre-allocate output with correct size
      out <- data.frame(d13C_slope     = numeric(length = n_days),
                        d13C_intercept = numeric(length = n_days),
                        d13C_r2        = numeric(length = n_days),
                        d13C_cvloo     = numeric(length = n_days),
                        d13C_cv5mae    = numeric(length = n_days),
                        d13C_cv5rmse   = numeric(length = n_days),
                        co2_slope      = numeric(length = n_days),
                        co2_intercept  = numeric(length = n_days),
                        co2_r2         = numeric(length = n_days),
                        co2_cvloo      = numeric(length = n_days),
                        co2_cv5mae     = numeric(length = n_days),
                        co2_cv5rmse    = numeric(length = n_days))

      # vectorize time construction
      start_time <- as.POSIXct(paste(date_seq, "00:00:00.0001"),
                               tz = "UTC", origin = "1970-01-01")
      end_time   <- as.POSIXct(paste(date_seq, "23:59:59.0000"),
                               tz = "UTC", origin = "1970-01-01")

      # hoist constants out of loop
      delta <- lubridate::ddays(calibration_half_width)
      fmla_d13c <- stats::formula(dlta13CCo2Refe.mean ~ dlta13CCo2.mean)
      fmla_co2 <- stats::formula(rtioMoleDryCo2Refe.mean ~ rtioMoleDryCo2.mean)

      # okay, now run calibrations...
      for (i in seq_along(date_seq)) {

        # define calibration interval
        cal_period <- lubridate::interval(date_seq[i] - delta,
                                          date_seq[i] + delta)

        # select data subset using the interval
        cal_subset <- ref_data %>%
          dplyr::filter(.data$timeBgn %within% cal_period)

        #---------------------------------------------
        # do some light validation of these points.
        cal_subset <- cal_subset %>%
          dplyr::filter(.data$dlta13CCo2.vari < 5 &
                          abs(.data$rtioMoleDryCo2.mean -
                                .data$rtioMoleDryCo2Refe.mean) < 10 &
                          abs(.data$dlta13CCo2.mean -
                                .data$dlta13CCo2Refe.mean) < 5)

        if (length(unique(cal_subset$verticalPosition)) >= 2 &&
              !all(is.na(cal_subset$dlta13CCo2.mean)) &&
              !all(is.na(cal_subset$dlta13CCo2Refe.mean))) {

          # model to calibrate delta 13C values.
          tmpmod_d13c <- stats::lm(fmla_d13c, data = cal_subset)
          tmpmod_co2 <- stats::lm(fmla_co2, data = cal_subset)

          out$d13C_slope[i]     <- coef(tmpmod_d13c)[[2]]
          out$d13C_intercept[i] <- coef(tmpmod_d13c)[[1]]
          sum_d13c <- summary(tmpmod_d13c)
          out$d13C_r2[i]        <- sum_d13c$r.squared

          out$co2_slope[i]      <- coef(tmpmod_co2)[[2]]
          out$co2_intercept[i]  <- coef(tmpmod_co2)[[1]]
          sum_co2 <- summary(tmpmod_co2)
          out$co2_r2[i]         <- sum_co2$r.squared

          # extract uncertainties:
          # extract leave-one-out CV value
          out$d13C_cvloo[i] <- loocv(tmpmod_d13c)
          out$co2_cvloo[i]  <- loocv(tmpmod_co2)

          # get cv5 values
          cv_d13c <- estimate_calibration_error(fmla_d13c, cal_subset)
          cv_co2 <- estimate_calibration_error(fmla_co2, cal_subset)

          # assign cv values:
          out$d13C_cv5mae[i]  <- cv_d13c$MAE
          out$co2_cv5mae[i]   <- cv_co2$MAE
          out$d13C_cv5rmse[i] <- cv_d13c$RMSE
          out$co2_cv5rmse[i]  <- cv_co2$RMSE

        } else {

          out$d13C_slope[i]     <- NA
          out$d13C_intercept[i] <- NA
          out$d13C_r2[i]        <- NA

          out$co2_slope[i]      <- NA
          out$co2_intercept[i]  <- NA
          out$co2_r2[i]         <- NA

          # set uncertainty values to NA as well.
          out$d13C_cvloo[i]   <- NA
          out$d13C_cv5mae[i]  <- NA
          out$d13C_cv5rmse[i] <- NA
          out$co2_cvloo[i]    <- NA
          out$co2_cv5mae[i]   <- NA
          out$co2_cv5rmse[i]  <- NA

        }
      }

      # output dataframe giving valid time range, slopes, intercepts, rsquared.
      out$timeBgn <- start_time
      out$timeEnd <- end_time

      # re-order columns to ensure that they are consistent across methods
      out <- out[, c("timeBgn", "timeEnd",
                     "d13C_slope", "d13C_intercept", "d13C_r2",
                     "d13C_cvloo", "d13C_cv5mae", "d13C_cv5rmse",
                     "co2_slope", "co2_intercept", "co2_r2",
                     "co2_cvloo", "co2_cv5mae", "co2_cv5rmse")]

    }
  }

  # ensure that not all dates are missing...narrowly defined here to
  # be if out only has 1 row (should be the only case where this happens!)
  if (nrow(out) == 1) {
    if (is.na(out$timeBgn)) {
      out$timeBgn <- as.POSIXct("2010-01-01", format = "%Y-%m-%d")
      out$timeEnd <- as.POSIXct("2010-01-01", format = "%Y-%m-%d")
    }
  }

  # check to see if any out$timeBgn or end are NA
  if (sum(is.na(out$timeBgn)) > 0 || sum(is.na(out$timeEnd)) > 0) {
    stop("NA in calibration data frame time - how did I get here?")
  }

  return(out)

}

#' Estimate slope/intercept of water isotope calibration regression
#'
#' Performs regression between measured and known carbon isotope and mole
#' fractions to generate a transfer function and associated uncertainty
#' estimates using both 5-fold and leave-one-out cross-validation methods.
#' Regression occurs on d18O and d2H values. 
#' 
#' @param ref_data Reference data.frame from which to estimate
#'        calibration parameters.
#' @param calibration_half_width Determines the period (in days)
#'        from which reference data are selected (period
#'        is 2*calibration_half_width).
#' @param slope_tolerance Allows for filtering of slopes that deviate
#'        from 1 by slope_tolerance.
#' @param r2_thres What is the minimum r2 value permitted in a 'useful'
#'        calibration relationship.
#' @param plot_regression_data True or false - should we plot the data used in
#'        the regression? Useful for debugging.
#' @param plot_dir If plot_regression_data is true, where should the
#'        plots be saved?
#' @param site Needed for regression plots.
#' @param min_nobs Minimum number of high-frequency observations to
#'                 define a peak.
#'
#' @return Returns a data.frame of calibration parameters.
#'        Output data.frame includes slope, intercept, and r^2 values
#'        for d13C and CO2 values.
#'
#' @importFrom stats coef lm
#' @importFrom magrittr %>%
#'
fit_water_regression <- function(ref_data,
                                 calibration_half_width,
                                 slope_tolerance,
                                 r2_thres,
                                 plot_regression_data = FALSE,
                                 plot_dir = "/dev/null",
                                 site,
                                 min_nobs = NA) {

  # First, identify year/month combination and then select data.
  yrmn <- paste(lubridate::year(ref_data$timeBgn)[1],
                lubridate::month(ref_data$timeBgn)[1],
                sep = "-")

  # validate yrmn - is it actually a date? if no refdata, then no.
  #---------------------------------------------------------------
  # Select which validation data to carry through to calibration
  #---------------------------------------------------------------
  ref_data <- select_daily_reference_data(ref_data,
                                          analyte = "h2o",
                                          min_nobs = min_nobs)

  #-----------------------------------------------------------
  # CALIBRATE WATER ISOTOPE VALUES
  if (nrow(ref_data) == 0) {

    # output dataframe giving valid time range, slopes, intercepts, rsquared.
    out <- data.frame(timeBgn = as.POSIXct(yrmn,
                                           format = "%Y-%m",
                                           tz = "UTC",
                                           origin = "1970-01-01"),
                      timeEnd = as.POSIXct(yrmn,
                                           format = "%Y-%m",
                                           tz = "UTC",
                                           origin = "1970-01-01"),
                      slope18O = as.numeric(NA),
                      slope2H  = as.numeric(NA),
                      intercept18O = as.numeric(NA),
                      intercept2H = as.numeric(NA),
                      r2_18O = as.numeric(NA),
                      r2_2H = as.numeric(NA),
                      cvloo_18O = as.numeric(NA),
                      cvloo_2H = as.numeric(NA),
                      cv5mae_18O = as.numeric(NA),
                      cv5mae_2H = as.numeric(NA),
                      cv5rmse_18O = as.numeric(NA),
                      cv5rmse_2H = as.numeric(NA))
  } else {

    # get start and end days.
    start_date <- as.Date(min(ref_data$timeBgn))
    end_date   <- as.Date(max(ref_data$timeEnd))

    # generate date sequence
    date_seq <- base::seq.Date(start_date, end_date, by = "1 day")
    n_days <- length(date_seq)

    # pre-allocate output with correct size
    out <- data.frame(slope18O     = numeric(length = n_days),
                      slope2H      = numeric(length = n_days),
                      intercept18O = numeric(length = n_days),
                      intercept2H  = numeric(length = n_days),
                      r2_18O       = numeric(length = n_days),
                      r2_2H        = numeric(length = n_days),
                      cvloo_18O    = numeric(length = n_days),
                      cvloo_2H     = numeric(length = n_days),
                      cv5mae_18O   = numeric(length = n_days),
                      cv5mae_2H    = numeric(length = n_days),
                      cv5rmse_18O  = numeric(length = n_days),
                      cv5rmse_2H   = numeric(length = n_days))

    # vectorize time construction
    start_time <- as.POSIXct(paste(date_seq, "00:00:00.0001"),
                             tz = "UTC", origin = "1970-01-01")
    end_time   <- as.POSIXct(paste(date_seq, "23:59:59.0000"),
                             tz = "UTC", origin = "1970-01-01")

    # hoist constants out of loop
    delta <- lubridate::ddays(calibration_half_width)
    fmla_18o <- stats::formula(dlta18OH2oRefe.mean ~ dlta18OH2o.mean)
    fmla_2h <- stats::formula(dlta2HH2oRefe.mean ~ dlta2HH2o.mean)

    # okay, now run calibrations...

    for (i in seq_along(date_seq)) {

      # define calibration interval
      cal_period <- lubridate::interval(date_seq[i] - delta,
                                        date_seq[i] + delta)

      # select data subset using the interval
      cal_subset <- ref_data %>%
        dplyr::filter(.data$timeBgn %within% cal_period)

      if (plot_regression_data) {
        stop("No regression data plots for water yet.")
      }

      if (length(unique(cal_subset$verticalPosition)) >= 2 && # >= 2 standards
            !all(is.na(cal_subset$dlta18OH2o.mean)) && # not all obs missing
            !all(is.na(cal_subset$dlta18OH2oRefe.mean))) { # not all ref missing

        tmpmod18o <- stats::lm(fmla_18o, data = cal_subset)
        tmpmod2h <- stats::lm(fmla_2h, data = cal_subset)

        # calculate gain and offset values.
        out$slope18O[i]     <- stats::coef(tmpmod18o)[[2]]
        out$slope2H[i]      <- stats::coef(tmpmod2h)[[2]]
        out$intercept18O[i] <- stats::coef(tmpmod18o)[[1]]
        out$intercept2H[i]  <- stats::coef(tmpmod2h)[[1]]

        # extract r2 (cache summary to avoid recomputation)
        sum18o <- summary(tmpmod18o)
        sum2h <- summary(tmpmod2h)
        out$r2_18O[i] <- sum18o$r.squared
        out$r2_2H[i] <- sum2h$r.squared

        # extract leave-one-out CV value
        out$cvloo_18O[i] <- loocv(tmpmod18o)
        out$cvloo_2H[i] <- loocv(tmpmod2h)

        # get cv5 values
        cv18o <- estimate_calibration_error(fmla_18o, cal_subset)
        cv2h <- estimate_calibration_error(fmla_2h, cal_subset)

        # assign cv values:
        out$cv5mae_18O[i] <- cv18o$MAE
        out$cv5mae_2H[i] <- cv2h$MAE
        out$cv5rmse_18O[i] <- cv18o$RMSE
        out$cv5rmse_2H[i] <- cv2h$RMSE

      } else {

        out$slope18O[i]      <- NA
        out$slope2H[i]       <- NA
        out$intercept18O[i]  <- NA
        out$intercept2H[i]   <- NA
        out$r2_18O[i]        <- NA
        out$r2_2H[i]         <- NA
        out$cvloo_18O[i]     <- NA
        out$cvloo_2H[i]      <- NA
        out$cv5mae_18O[i]    <- NA
        out$cv5mae_2H[i]     <- NA
        out$cv5rmse_18O[i]   <- NA
        out$cv5rmse_2H[i]    <- NA
      }
    }

    # output dataframe giving valid time range, slopes, intercepts, rsquared.
    out$timeBgn <- start_time
    out$timeEnd <- end_time

    # re-order columns to ensure that they are consistent across methods
    out <- out[, c("timeBgn", "timeEnd",
                   "slope18O", "intercept18O", "r2_18O",
                   "cvloo_18O", "cv5mae_18O", "cv5rmse_18O",
                   "slope2H", "intercept2H", "r2_2H",
                   "cvloo_2H", "cv5mae_2H", "cv5rmse_2H")]
  }
  return(out)

}
