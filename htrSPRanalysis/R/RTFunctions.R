#' @importFrom rlang .data
#' @importFrom grDevices pdf dev.off
#internal

# maybe do this on the selected concentration? Then can go back to 10% of signal?

find_dissociation_window_biphasic <- function(well_idx, sample_info, x_vals, y_vals,
                                     incl_concentrations_ligand, max_RU_tol, min_RU_tol){

  if (sample_info[well_idx,]$`Automate Dissoc. Window` != "Y")
    return(NULL)

   # check for 2 phase decay

  baseline <- sample_info[well_idx,]$Baseline
  baseline_start <- sample_info[well_idx,]$`Bsl Start`
  association <- sample_info[well_idx,]$Association

  start_time <- baseline + baseline_start + association
  dissociation <- sample_info[well_idx,]$Dissociation
  start_idx <- sample_info[well_idx,]$FirstInclConcIdx
  num_conc <- sample_info[well_idx,]$NumInclConc
  end_idx <- start_idx + num_conc - 1

  end_dissoc_list <- NULL
  is_biphasic <- NULL
  RU_cutoff <- NULL
  max_idx <- 0
  for (i in start_idx:end_idx){
    Time <- x_vals[, i]
    RU <- y_vals[, i]
    df <- suppressMessages(dplyr::bind_cols(Time, RU))
    names(df) <- c("Time", "RU")
    df %>% dplyr::filter(.data$Time > start_time) -> df

    # Do not base on low information concentrations
  #  if (mean(df$RU, na.rm = TRUE) < min_RU_tol | mean(df$RU, na.rm = TRUE) > max_RU_tol)
  #    next

    #Smooth first because very noisy data will cause crash
    df$RU_before <- df$RU

    df$RU <- stats::loess(df$RU ~ df$Time, ) %>% stats::predict()
    RU_0 <- max(df$RU, na.rm = TRUE)

    single_exp_safe <- purrr::safely(.f = stats::nls)
    single_exp_fit <- single_exp_safe(RU ~ I(a * exp(b * Time)),
                                    df,
                                    list(a = RU_0, b = 10^(-3)))

    biphasic_exp_safe <- purrr::safely(.f = stats::nls)
    biphasic_exp_fit <- biphasic_exp_safe(RU ~ I(a1 * exp(b1 * Time)
                                                  + a2 * exp(b2 * Time)),
                                           data = df,
                                           start = list( a1 = .75 * RU_0,
                                                                    a2 = .25 * RU_0,
                                                                    b1 = 10^(-2),
                                                                    b2 = 10^(-4)))

    if (is.null(single_exp_fit$error)){
      single_exp_summary <- summary(single_exp_fit$result)
      single_sse <- sum(single_exp_summary$residuals^2)
    } else
      single_sse <- NA

    if (is.null(biphasic_exp_fit$error)){
       biphasic_exp_summary <- summary(biphasic_exp_fit$result)
       biphasic_sse <- sum(biphasic_exp_summary$residuals^2)
    } else {
      is_biphasic <- c(is_biphasic, 0)
      next
    }

    if (single_sse < biphasic_sse)
      is_biphasic <- c(is_biphasic, 0)
    else {
      sse_diff <- (single_sse - biphasic_sse)/(biphasic_sse + single_sse)
      if (sse_diff > .2 | is.na(sse_diff)){# single error is more than 1.5 times biphasic
        is_biphasic <- c(is_biphasic, 1)
        # find slow component
        params <- stats::coef(biphasic_exp_fit)
        if (abs(params[3] > abs(params[4]))){ #first param is fast rate
           a_slow <- params[2]
        } else {
          a_slow <- params[1]
        }
        RU_cutoff <- c(RU_cutoff, a_slow)
      }

    }


    # if (is.na(window_idx)){
    #   # Once this happens, we are using the entire time series for all concentrations
    #   end_dissoc_list <- c((df$Time)[length(df$Time)], end_dissoc_list)
    #   next
    # } else
    #   end_dissoc <- df$Time[window_idx]
    #
    # end_dissoc_list <- c(end_dissoc, end_dissoc_list)

  }
  if (sum(is_biphasic) > 1){ # more than one concentration has biphasic behavior
    RU_cutoff <- max(RU_cutoff, na.rm = TRUE)
    window_idx <- which(df$RU > RU_cutoff)
    if (!is.na(window_idx[1]))
      return(df$Time[window_idx[1]])
  }
  return(NA) # not truncating for biphasic
}

find_dissociation_window_flat <- function(well_idx, sample_info, x_vals, y_vals,
                                     incl_concentrations_ligand, max_RU_tol, min_RU_tol){

  if (sample_info[well_idx,]$`Automate Dissoc. Window` != "Y")
    return(NULL)

  baseline <- sample_info[well_idx,]$Baseline
  baseline_start <- sample_info[well_idx,]$`Bsl Start`
  association <- sample_info[well_idx,]$Association

  start_time <- baseline + baseline_start + association
  dissociation <- sample_info[well_idx,]$Dissociation
  start_idx <- sample_info[well_idx,]$FirstInclConcIdx
  num_conc <- sample_info[well_idx,]$NumInclConc
  end_idx <- start_idx + num_conc - 1

  end_dissoc_list <- NULL
  df_all <- NULL
  max_idx <- 0
  for (i in start_idx:end_idx){
    Time <- x_vals[, i]
    RU <- y_vals[, i]
    df <- suppressMessages(dplyr::bind_cols(Time, RU))
    names(df) <- c("Time", "RU")
    df %>% dplyr::filter(.data$Time > (start_time - 5)) -> df

    # Do not base on low information concentrations
    #  if (mean(df$RU, na.rm = TRUE) < min_RU_tol | mean(df$RU, na.rm = TRUE) > max_RU_tol)
    #    next

    #Smooth first because very noisy data will cause crash
    df$RU_before <- df$RU
    df$RU <- stats::loess(df$RU ~ df$Time, ) %>% stats::predict()

    df_zoo <- zoo::as.zoo(df)
    #  max_idx <- max_idx + 1
    tibble::as_tibble(
      zoo::rollapply(
        data = df_zoo, FUN =  function(x){
          x_df <- tibble::as_tibble(x)
          if (all(is.na(x_df$RU)| is.nan(x_df$RU)))
            return(c(NA,NA))
          else
            return(stats::coef(stats::lm(RU ~ Time, singular.ok = TRUE,
                                         data = x_df)))}, by.column = FALSE,

        width = 20)) -> df_out
    names(df_out) <- c("Intercept", "Slope")
    n_vals <- dim(df_out)[1]

    #  df_out %>% dplyr::mutate(x = rep(max_idx, n_vals)) -> df_out
    df_out %>% dplyr::mutate(RollIndex = 1:n_vals) -> df_out
    max_slope <- max(abs(df_out$Slope))
    min_slope <- min(abs(df_out$Slope))
    target_slope <- 0.40*max_slope
    window_idx <- which(abs(df_out$Slope) < target_slope)[1]

    if (is.na(window_idx)){
      # Once this happens, we are using the entire entered dissociation duration for all concentrations
      end_dissoc_list <- c(NA, end_dissoc_list)
      next
    } else
      end_dissoc <- df$Time[window_idx]

    end_dissoc_list <- c(end_dissoc, end_dissoc_list)

  }
  # Now we have a candidate end of dissoc for each concentration. Thia should be done for selected concentrations
  # Overall window is smallest window that accommodates all the concentrations
  return(max(as.numeric(end_dissoc_list), na.rm = TRUE))
}

#internal.

first_conc_indices <- function(well_idx, num_conc_ligand){

  #Compute the correction to baseline. Usually for regenerative case.

  # find displacement from last ligand
  if (well_idx == 1){
    first_conc_idx <- 1
  } else
    first_conc_idx <- sum(num_conc_ligand[1:(well_idx - 1)]) + 1 #number of time series up to this well
  first_conc_idx
}

first_conc_indices_from_titration_data <- function(well_idx, ROI, ligand_and_ROI){

  # If we have ROI information in the titration data, we can use this to match cols in data
  # to rows in the sample_info

  col_idx <- which(ligand_and_ROI$ROI == ROI[well_idx])
  if (sum(is.null(col_idx)) > 0 | sum(is.na(col_idx)) > 0)
    stop(paste("Could not identify starting index for titration data for spot", well_idx))

  col_idx[1]
}

#internal. Find the carterra index for baseline adjustment
get_baseline_indices <- function(well_idx, sample_info, x_vals, y_vals){
  start_idx <- sample_info[well_idx,]$FirstConcIdx
  num_conc <- sample_info[well_idx,]$NumConc
  end_idx <- start_idx + num_conc - 1

  baseline <- sample_info[well_idx,]$Baseline
  baseline_start <- sample_info[well_idx,]$`Bsl Start`
  baseline_avg_list <- NULL

  for (i in start_idx:end_idx){
    Time <- x_vals[, i]
    RU <- y_vals[, i]
    df <- suppressMessages(dplyr::bind_cols("Time" = Time, "RU" = RU))
    colnames(df) <- c("Time", "RU")
    df %>% dplyr::filter(.data$Time > baseline_start &
                           .data$Time < baseline+ baseline_start) %>%
      dplyr::select("RU") -> base_meas
    base_meas <- base_meas$RU
    baseline_avg <- mean(base_meas, na.rm = TRUE)
    baseline_avg_list <- c(baseline_avg_list, baseline_avg)
  }
  min_baseline <- min(baseline_avg_list)
  try_baseline <- which(baseline_avg_list == min_baseline)
  if (length(try_baseline) > 1)
    try_baseline <- try_baseline[1]
  baseline_idx <- start_idx + try_baseline - 1

  #is highest baseline negative?
  if (baseline_avg < 0)
    baseline_neg <- TRUE else
      baseline_neg <- FALSE

  list(baseline_idx = baseline_idx, min_baseline = min_baseline, baseline_negative = baseline_neg)
}

# internal function.
create_dataframe_with_conc <- function(begin_conc_idx, end_conc_idx, x_vals, y_vals,
                                       numerical_concentrations,
                                       n_time_points){
  n_vals <- dim(x_vals)[2]
  names(x_vals) <- as.character(1:n_vals)
  names(y_vals) <- as.character(1:n_vals)

  Time <- x_vals[, begin_conc_idx:end_conc_idx] %>%
    tidyr::pivot_longer(cols = tidyselect::everything()) %>%
    dplyr::arrange(as.numeric(.data$name)) %>%
    dplyr::select("value")
  RU <- y_vals[, begin_conc_idx:end_conc_idx] %>%
    tidyr::pivot_longer(cols = tidyselect::everything()) %>%
    dplyr::arrange(as.numeric(.data$name)) %>%
    dplyr::select("value")

  purrr::map_dfr(.x = tibble::tibble(numerical_concentrations),
                 .f = function(x, n_time_points) rep(x, n_time_points), n_time_points) %>%
    dplyr::arrange(.data$numerical_concentrations) -> numerical_concentrations
  Concentrations <- numerical_concentrations
  df <- suppressMessages(dplyr::bind_cols("Time" = Time, "RU" = RU, "Concentration" = Concentrations))
  colnames(df) <- c("Time", "RU", "Concentration")
  df
}


# internal. Baseline correction
baseline_correction <- function(well_idx, x_vals, y_vals, sample_info){

  negative_baseline <- sample_info[well_idx, ]$BaselineNegative
  baseline_average <- sample_info[well_idx, ]$BaselineAverage
  baseline <- sample_info[well_idx,]$Baseline
  baseline_start <- sample_info[well_idx,]$`Bsl Start`

  start_idx <- sample_info[well_idx, ]$FirstConcIdx
  num_conc <- sample_info[well_idx,]$NumConc  #number of concentrations for this well
  end_idx <- start_idx + num_conc - 1


  #Need to test if baseline of highest conc is < 0

  if (sample_info[well_idx,]$Regen. == "N" & !negative_baseline){
    # add baseline average to all timepoints

    y_vals[, start_idx:end_idx] <-
      y_vals[, start_idx:end_idx] - baseline_average
  } else
  {
    # correct all to mean of zero
    for (i in 1:num_conc){
      # compute baseline average for each concentration
      # subtract from baseline average from all RU vals for that concentration

      Time <- x_vals[, start_idx + (i-1)]
      RU <- y_vals[, start_idx + (i-1)]

      df <- suppressMessages(dplyr::bind_cols("Time" = Time, "RU" = RU))
      colnames(df) <- c("Time", "RU")

      df %>%
        dplyr::filter(.data$Time > baseline_start &
                        .data$Time < baseline + baseline_start) %>%
        dplyr::select("RU") -> base_meas # select for defined baseline time period

      # This command is split up because mean(.$RU) would not parse properly
      base_corr <- mean(base_meas$RU, na.rm = TRUE)
      y_vals[, start_idx + (i-1)] <- y_vals[, start_idx + (i-1)] - base_corr
    }
  }
  y_vals[, start_idx:end_idx]

}

# internal. Get response curve for a well.
get_response_curve <- function(well_idx, sample_info, x_vals, y_vals,
                               all_concentrations_values,
                               incl_concentrations_values, n_time_points){

  start_incl_idx <- sample_info[well_idx,]$FirstInclConcIdx
  start_idx <- sample_info[well_idx,]$FirstConcIdx
  num_conc <- sample_info[well_idx,]$NumConc
  end_idx <- start_idx + num_conc - 1

  num_incl_conc <- sample_info[well_idx,]$NumInclConc
  end_incl_idx <- start_incl_idx + num_incl_conc - 1
  incl_conc_values <- incl_concentrations_values[start_incl_idx:end_incl_idx]

  ligand_desc <- sample_info[well_idx,]$Ligand
  baseline <- sample_info[well_idx,]$Baseline
  baseline_start <- sample_info[well_idx,]$`Bsl Start`
  association <- sample_info[well_idx,]$Association

  df <- create_dataframe_with_conc(start_idx, end_idx, x_vals, y_vals,
                                   all_concentrations_values[start_idx:end_idx],
                                   n_time_points)

  df %>% dplyr::filter((Time >= baseline + baseline_start + association - 10)
                       & (Time <= baseline + baseline_start + association - 5)) %>%
    dplyr::group_by(Concentration) %>%
    dplyr::summarise(`Dose Response` = mean(.data$RU, na.rm = TRUE)) -> df_RC
  df_RC %>% dplyr::mutate(Included =
                            forcats::as_factor(ifelse(Concentration %in% incl_conc_values,
                                                      "Yes", "No"))) -> df_RC
  ggplot2::ggplot(df_RC, ggplot2::aes(x = Concentration,
                                      y = `Dose Response`)) +
    ggplot2::geom_point(ggplot2::aes(color = Included)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_log10() +
    ggplot2::ggtitle(ligand_desc)
}

# internal. Find best concentration window
get_best_window <- function(well_idx, sample_info, x_vals, y_vals,
                            num_conc, concentrations, start_idx, n_time_points){

  association <- sample_info[well_idx,]$Association
  baseline <- sample_info[well_idx,]$Baseline
  baseline_start <- sample_info[well_idx,]$`Bsl Start`
  end_assoc_time <- association + baseline + baseline_start

  end_idx <- start_idx + num_conc - 1
  end_assoc_resp <- NULL

  association_end <- baseline + baseline_start + association

  df <- create_dataframe_with_conc(start_idx, end_idx, x_vals, y_vals,
                                   concentrations,
                                   n_time_points)

  #this has failed in some data sets where these observations are missing.
  df %>% dplyr::filter((.data$Time >= baseline + baseline_start + association - 15)
                       & (.data$Time <= baseline + baseline_start + association - 5)) %>%
    dplyr::group_by(.data$Concentration) %>%
    dplyr::summarise(`Dose Response` = mean(.data$RU, na.rm = TRUE)) -> df_RC

  # check to see if any results for df_RC

  if (dim(df_RC)[1] == 0)
    return(NULL)

  # record the differences between consecutive responses
  sum_diff <- NULL
  conc_diff <- NULL
  for (i in 1:(num_conc - 1)){
    sum_diff <- suppressMessages(
      dplyr::bind_cols(
        sum_diff, df_RC[i+1,]$`Dose Response` - df_RC[i,]$`Dose Response`))
    conc_diff <- suppressMessages(
      dplyr::bind_cols(
        conc_diff, log(df_RC[i+1,]$Concentration) - log(df_RC[i,]$Concentration)))
    slope_diff <- abs(sum_diff/conc_diff)
  }
  slope_diff <- sum_diff/conc_diff
  # add sums for each 5 cycle window.
  cum_sum <- zoo::rollapply(
    purrr::as_vector(
      purrr::flatten(slope_diff)), 4, FUN = sum)
  start_conc_idx <- which(cum_sum == max(cum_sum, na.rm = TRUE))

  # check start and end slopes, may be better to fit 4 instead of five

  slopes <- purrr::as_vector(slope_diff[start_conc_idx:(start_conc_idx+3)])

  remove_concentration <- ifelse(slopes < 0.30*mean(slopes), 1, 0)

  # return 5 best consecutive concentrations
  if(sum(remove_concentration) == 0)
    return(concentrations[start_conc_idx:(start_conc_idx + 4)])

  # if some slopes are less than 30% of the mean, remove one concentration
  # low end or high end, depending on which slope is smaller

  # remove_concentration has at least one '1' value
  # if both first and last are tagged, we remove the smallest
  # if only one is tagged, it will still be the smallest
  if (slopes[1] < slopes[4])
    return(concentrations[(start_conc_idx+1):(start_conc_idx + 4)])
  else
    return(concentrations[(start_conc_idx):(start_conc_idx + 3)])

}

#internal. Plot all data for a well

plot_sensorgrams <- function(well_idx,
                             sample_info,
                             x_vals,
                             y_vals,
                             incl_conc_values,
                             all_concentrations_values,
                             n_time_points,
                             all_concentrations = FALSE){
  if (!all_concentrations){
    start_idx <- sample_info[well_idx,]$FirstInclConcIdx
    num_conc <- sample_info[well_idx,]$NumInclConc
  } else
  {
    start_idx <- sample_info[well_idx,]$FirstConcIdx
    num_conc <- sample_info[well_idx,]$NumConc
    incl_conc_values <- all_concentrations_values
  }
  end_idx <- start_idx + num_conc - 1

  incl_conc_values <- incl_conc_values[start_idx:end_idx]

  ligand_desc <- paste("Ligand:", sample_info[well_idx,]$Ligand)
  analyte_desc <- paste("Analyte:", sample_info[well_idx,]$Analyte)

  n_vals <- dim(x_vals)[2]
  names(x_vals) <- as.character(1:n_vals)
  names(y_vals) <- as.character(1:n_vals)


  Time <- x_vals[, start_idx:end_idx] %>%
    tidyr::pivot_longer(cols = tidyselect::everything()) %>%
    dplyr::arrange(as.numeric(.data$name)) %>%
    dplyr::select("value")
  RU <- y_vals[, start_idx:end_idx]%>%
    tidyr::pivot_longer(cols = tidyselect::everything()) %>%
    dplyr::arrange(as.numeric(.data$name)) %>%
    dplyr::select("value")


  purrr::map_dfr(.x = tibble::tibble(incl_conc_values),
                 .f = function(x, n_time_points)
                   rep(x, n_time_points), n_time_points) %>%
    dplyr::arrange(.data$incl_conc_values) -> incl_conc_values

  Concentrations <- incl_conc_values

  df <- suppressMessages(dplyr::bind_cols("Time" = Time, "RU" = RU, "Concentration" = Concentrations))

  colnames(df) <- c("Time", "RU", "Concentration")

  df$Concentration <- forcats::as_factor(formatC(df$Concentration, format = "e",digits = 2))

  spot <- paste0(sample_info[well_idx,]$Row, sample_info[well_idx,]$Column)
  sub_title <- paste("Block", sample_info[well_idx,]$Block, "Spot", spot,
                     "ROI", sample_info[well_idx,]$ROI)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$Time,
                                   y = .data$RU,
                                   color = .data$Concentration)) +
    ggplot2::geom_point(size = 0.01) +
    ggplot2::ggtitle(paste(analyte_desc, ligand_desc), subtitle = sub_title)
}

# internal. Called by UserFunctions get_fitted_plots
plot_sensorgrams_with_fits <- function(well_idx, sample_info,
                                       fits, x_vals, y_vals,
                                       incl_conc_values, n_time_points){

  if (!is.null(fits[[well_idx]]$error))
    return(NULL)

  start_idx <- sample_info[well_idx,]$FirstInclConcIdx
  num_conc <- sample_info[well_idx,]$NumInclConc
  end_idx <- start_idx + num_conc - 1

  baseline <- sample_info[well_idx,]$Baseline
  baseline_start <- sample_info[well_idx,]$`Bsl Start`
  association <- sample_info[well_idx,]$Association
  dissociation <- sample_info[well_idx,]$Dissociation

  assoc_start <- baseline + baseline_start

  ligand_desc <- paste("Ligand:", sample_info[well_idx,]$Ligand)
  analyte_desc <- paste("Analyte:", sample_info[well_idx,]$Analyte)



  n_vals <- dim(x_vals)[2]
  names(x_vals) <- as.character(1:n_vals)
  names(y_vals) <- as.character(1:n_vals)


  Time <- x_vals[, start_idx:end_idx] %>%
    tidyr::pivot_longer(cols = tidyselect::everything()) %>%
    dplyr::arrange(as.numeric(.data$name)) %>%
    dplyr::select("value")
  RU <- y_vals[, start_idx:end_idx]%>%
    tidyr::pivot_longer(cols = tidyselect::everything()) %>%
    dplyr::arrange(as.numeric(.data$name)) %>%
    dplyr::select("value")

  numerical_concentration <- incl_conc_values[start_idx:end_idx]

  purrr::map_dfr(.x = tibble::tibble(numerical_concentration),
                 .f = function(x, n_time_points)
                   rep(x, n_time_points), n_time_points) %>%
    dplyr::arrange(.data$numerical_concentration) -> numerical_concentration

  Concentrations <- numerical_concentration

  df <- suppressMessages(dplyr::bind_cols("Time" = Time, "RU" = RU, "Concentration" = Concentrations))

  colnames(df) <- c("Time", "RU", "Concentration")

  end_time <- ifelse(!is.finite(sample_info[well_idx, ]$DissocEnd), baseline + baseline_start + association + dissociation, sample_info[well_idx, ]$DissocEnd)

  df %>% dplyr::filter(.data$Time > baseline + baseline_start &
                         .data$Time < end_time) -> df

  fits[[well_idx]]$result$FitOutcomes %>%
    dplyr::filter(.data$Time < (end_time - assoc_start)) %>%
    dplyr::pull(RU) -> fit_RU

  suppressMessages(dplyr::bind_cols(df,FittedRU = fit_RU)) -> df

  # Correct to zero time

  df$Time <- df$Time - assoc_start

  colnames(df) <- c("Time", "RU", "Concentration", "FittedRU")

  df$Concentration <- forcats::as_factor(formatC(df$Concentration, format = "e",digits = 2))

  spot <- paste0(sample_info[well_idx,]$Row, sample_info[well_idx,]$Column)

  sub_title <- paste("Block", sample_info[well_idx,]$Block, "Spot", spot,
                     "ROI", sample_info[well_idx,]$ROI)


  ggplot2::ggplot(df, ggplot2::aes(x = .data$Time, y = .data$RU)) +
    ggplot2::geom_point(size = 0.09, ggplot2::aes(color = .data$Concentration)) +
    ggplot2::ggtitle(paste(analyte_desc, ligand_desc), subtitle = sub_title) +
    ggplot2::geom_line(ggplot2::aes(x = .data$Time, y = .data$FittedRU, group = .data$Concentration), color = "black")
}

# This function is used internally. It is a replacement for summary.nlm that allows for a non-singular
# Hessian due to the constrained fits.

summary_fit_with_constraints <- function(fit_object){

  info <- fit_object$info
  hessian <- fit_object$hessian
  pars <- fit_object$par
  n <- length(pars)
  std_err_full <- rep(NA, n)

  if (info == 0 | info == 5) {
    df <- data.frame(Estimate = pars, "Std. Error" = std_err_full)
    colnames(df) <- c("Estimate", "Std. Error")
    return(df)
  }

  n <- nrow(hessian)
  test_zeroes <- apply(hessian, 1 , function(x) sum(x==0))
  nonsingular_rows <- which(test_zeroes != n)
  # if (length(nonsingular_rows) == n & info != 5)
  #    return(summary(fit_object)$coefficients)
  hessian <- hessian[nonsingular_rows, nonsingular_rows]

  std_err_full <- rep(NA, n)

  # get table directly. Code is pulled from summary.minpack.lm

  if (info != 5) {            # when info is 5, that means the iterations maxed out and fit is not valid
    ibb <- chol(hessian)
    ih <- chol2inv(ibb)
    p <- length(pars)
    rdf <- length(fit_object$fvec) - p
    resvar <- stats::deviance(fit_object)/rdf
    se <- sqrt(diag(ih) * resvar)
  }
  else
    se <- rep(NA, length(nonsingular_rows))

  std_err_full[nonsingular_rows] <- se

  df <- data.frame(Estimate = pars, "Std. Error" = std_err_full)
  colnames(df) <- c("Estimate", "Std. Error")
  df
}

# Fit only kd. Not implemented
fit_kd <- function(pars, df, incl_concentrations, num_conc, kd, t0 = t0){
  #pars ("Rmax" one for each concentration,"ka", "tstart" one for each concentration)

  R0 <- pars[1:num_conc]
  kd <- pars[(num_conc+1)]

  err_assoc <- NULL
  err_dissoc <- NULL

  for (i in 1:num_conc){

    df_i <- df %>% dplyr::filter(Concentration == incl_concentrations[i])
    RU <- df_i$RU
    Time <- df_i$Time
    Concentration <- df_i$Concentration

    df_i %>% dplyr::filter(.data$DissocIndicator == 1) -> df_dissoc

    dissoc_formula <- R0[i]*exp(-kd*(df_dissoc$Time - t0))

    err_dissoc <-   c(err_dissoc, df_dissoc$RU - dissoc_formula)

  }
  err_dissoc

}

# Internal function that is passed to nlm. It computes the objective function for the fit.
fit_as_system <- function(pars, df, incl_concentrations,
                          num_conc,
                          association,
                          bulkshift,
                          global_rmax,
                          regenerated_surface){

  t0 <- rep(0, num_conc)
  if(global_rmax){
    #pars (global "Rmax","ka", "R0" one for each concentration, kd, shift one for each concentration)

    Rmax <- pars[1]

    ka <- pars[2]

    if (!regenerated_surface){
      t0 <- pars[3:(2 + num_conc)]
      kd <- pars[(3+num_conc)]
    } else
      kd <- pars[3]


    if (bulkshift & !regenerated_surface)
      shift <- pars[(4 + num_conc):(3 + 2*num_conc)]
    else
      if (bulkshift)
        shift <- pars[4:(3 + num_conc)]

  } else{
    #pars ("Rmax" one for each concentration,"ka", "R0" one for each concentration, kd, shift one for each concentration)

    Rmax <- pars[1:num_conc]

    ka <- pars[num_conc+1]

    if (!regenerated_surface){
      t0 <- pars[(num_conc + 2):(2*num_conc + 1)]
      kd <- pars[2*num_conc + 2]
    } else
      kd <- pars[(num_conc + 2)]


    if (bulkshift & !regenerated_surface)
      shift <- pars[(2*num_conc + 3):(3*num_conc + 2)]
    else
      if (bulkshift)
        shift <- pars[(num_conc + 3):(2*num_conc + 2)]

  }

  err_assoc <- NULL
  err_dissoc <- NULL

  for (i in 1:num_conc){

    df_i <- df %>% dplyr::filter(.data$Concentration == incl_concentrations[i])
    #  RU <- df_i$RU

    df_i %>% dplyr::filter(.data$AssocIndicator == 1) -> df_assoc
    df_i %>% dplyr::filter(.data$DissocIndicator == 1) -> df_dissoc

    if (global_rmax){
      assoc_formula_first_term <-
        (Rmax * ka * incl_concentrations[i])/(ka*incl_concentrations[i] + kd)

    } else {
      assoc_formula_first_term <-
        (Rmax[i] * ka * incl_concentrations[i])/(ka*incl_concentrations[i] + kd)

    }

    assoc_formula_second_term <-
      (1 - exp(-((ka*incl_concentrations[i] + kd)*(df_assoc$Time + t0[i]))))

    assoc_formula_full <- assoc_formula_first_term * assoc_formula_second_term

    err_assoc <- c(err_assoc, df_assoc$RU - assoc_formula_full)

    end_of_association_RU <-  assoc_formula_first_term *
      (1 - exp(-((ka*incl_concentrations[i] + kd)*(association + t0[i]))))

    dissoc_decay <- exp(-kd*(df_dissoc$Time - association))

    if (bulkshift)
      end_of_association_RU <- end_of_association_RU + shift[i]

    dissoc_formula_full <- end_of_association_RU * dissoc_decay

    err_dissoc <-   c(err_dissoc, df_dissoc$RU - dissoc_formula_full)

  }
  c(err_assoc, err_dissoc)
}

# Internal. Called by fit_association_dissociation

get_fit_outcomes <- function(Rmax, ka, t0, kd, df, num_conc,
                             incl_concentrations,
                             association,
                             shift,
                             global_rmax){

  full_output_RU <- NULL

  for (i in 1:num_conc){

    df_i <- df %>% dplyr::filter(Concentration == incl_concentrations[i])
    #   RU <- df_i$RU
    Time <- df_i$Time
    Concentration <- df_i$Concentration

    df_i %>% dplyr::filter(.data$AssocIndicator == 1) -> df_assoc
    df_i %>% dplyr::filter(.data$DissocIndicator == 1) -> df_dissoc

    if (global_rmax){
      assoc_formula_first_term <-
        (Rmax * ka * df_assoc$Concentration)/(ka*df_assoc$Concentration + kd)
      end_of_association_RU <-  (Rmax * ka * df_dissoc$Concentration)/(ka*df_dissoc$Concentration + kd) *
        (1 - exp(-((ka*df_dissoc$Concentration + kd)*(association + t0[i]))))


    } else{
      assoc_formula_first_term <-
        (Rmax[i] * ka * df_assoc$Concentration)/(ka*df_assoc$Concentration + kd)
      end_of_association_RU <-  (Rmax[i] * ka * df_dissoc$Concentration)/(ka*df_dissoc$Concentration + kd) *
        (1 - exp(-((ka*df_dissoc$Concentration + kd)*(association + t0[i]))))


    }
    assoc_formula_second_term <-
      (1 - exp(-((ka*df_assoc$Concentration + kd)*(df_assoc$Time + t0[i]))))

    assoc_formula_full <- assoc_formula_first_term * assoc_formula_second_term

    df_assoc$RU <- assoc_formula_full


    dissoc_decay <- exp(-kd*(df_dissoc$Time - association))

    # shift is zero if no bulkshift

    end_of_association_RU <- end_of_association_RU + shift[i]

    dissoc_formula_full <- end_of_association_RU * dissoc_decay

    df_dissoc$RU <- dissoc_formula_full

    full_output_RU <- dplyr::bind_rows(full_output_RU, df_assoc, df_dissoc)

  }
  # return fitted values
  full_output_RU %>% dplyr::select("Time", "RU", "Concentration")
}

# internal function. Fits sensorgrams for a given well

fit_association_dissociation <- function(well_idx, sample_info, x_vals, y_vals,
                                         incl_concentrations_values, n_time_points,
                                         min_allowed_kd = 10^(-5),
                                         max_iterations = 500,
                                         ptol = 10^(-10),
                                         ftol = 10^(-10),
                                         max_RU_tol = 300){

  # this function will fit all selected concentrations for one well

  baseline <- sample_info[well_idx,]$Baseline
  baseline_start <- sample_info[well_idx,]$`Bsl Start`

  if (sample_info[well_idx,]$Bulkshift == "Y")
    bulkshift <- TRUE else
      bulkshift <- FALSE

  if (sample_info[well_idx,]$`Global Rmax` == "Y")
    global_rmax <- TRUE else
      global_rmax <- FALSE

  if (sample_info[well_idx,]$`Regen.` == "Y" |
      sample_info[well_idx,]$BaselineNegative)
    regenerated_surface <- TRUE else
      regenerated_surface <- FALSE

  start_idx <- sample_info[well_idx,]$FirstInclConcIdx
  num_conc <- sample_info[well_idx,]$NumInclConc
  end_idx <- start_idx + num_conc - 1

  association <- sample_info[well_idx,]$Association
  dissociation <- sample_info[well_idx,]$Dissociation

  assoc_start <- baseline + baseline_start
  assoc_end <- assoc_start + association

  dissoc_start <- assoc_end
  dissoc_end <- assoc_end + dissociation

  if (sample_info[well_idx,]$`Automate Dissoc. Window` == "Y" &
      (is.finite(sample_info[well_idx, ]$DissocEnd)))
    dissoc_end <- sample_info[well_idx,]$DissocEnd

  n_vals <- dim(x_vals)[2]
  names(x_vals) <- as.character(1:n_vals)
  names(y_vals) <- as.character(1:n_vals)

  Time <- x_vals[, start_idx:end_idx] %>%
    tidyr::pivot_longer(cols = tidyselect::everything()) %>%
    dplyr::arrange(as.numeric(.data$name)) %>%
    dplyr::select("value")
  RU <- y_vals[, start_idx:end_idx]%>%
    tidyr::pivot_longer(cols = tidyselect::everything()) %>%
    dplyr::arrange(as.numeric(.data$name)) %>%
    dplyr::select("value")

  incl_concentrations <-
    incl_concentrations_values[start_idx:end_idx]

  purrr::map_dfr(.x = tibble::tibble(incl_concentrations),
                 .f = function(x, n_time_points) rep(x, n_time_points), n_time_points) %>%
    dplyr::arrange(.data$incl_concentrations) -> incl_concentrations_rep

  df <- suppressMessages(dplyr::bind_cols("Time" = Time,
                                          "RU" = RU,
                                          "Concentration" = incl_concentrations_rep))
  colnames(df) <- c("Time", "RU", "Concentration") #force correct names - dplyr is doing weird things


  #do both dissociation and association
  df %>% dplyr::mutate(AssocIndicator =
                         ifelse((.data$Time >= assoc_start & .data$Time < assoc_end), 1, 0),
                       DissocIndicator = ifelse(Time > dissoc_start & Time < dissoc_end, 1, 0)) -> df
  df %>% dplyr::filter((.data$AssocIndicator == 1 | .data$DissocIndicator == 1)) -> df

  df %>% dplyr::group_by(.data$Concentration) %>% dplyr::summarise(max = max(.data$RU, na.rm = TRUE)) -> Rmax_start_df

  t0_start <- rep(0, num_conc)

  if (global_rmax){
    Rmax_start <- max(Rmax_start_df$max, na.rm = TRUE)
  } else {
    Rmax_start <- Rmax_start_df$max
  }

  kd_start <- 10^(-5)
  ka_start <-  10^(5)

  # shift time to start at zero
  df$Time <- df$Time - assoc_start

  # bulkshift initial value
  shift <- rep(0, num_conc)

  if (bulkshift & !regenerated_surface){
    init_params <- c(Rmax_start, ka_start, t0_start, kd_start, shift)
    param_lower_bounds <- c(rep(0, length(Rmax_start)), 10, rep(-Inf,num_conc), min_allowed_kd, rep(-100, num_conc))
    param_upper_bounds <- c(rep(max_RU_tol, length(Rmax_start)), 10^7, rep(Inf,num_conc), 1, rep(100, num_conc))
  } else { if (!bulkshift & !regenerated_surface){
    init_params <- c(Rmax_start, ka_start, t0_start, kd_start)
    param_lower_bounds <- c(rep(0, length(Rmax_start)), 10, rep(-Inf,num_conc), min_allowed_kd)
    param_upper_bounds <- c(rep(max_RU_tol, length(Rmax_start)), 10^7, rep(Inf,num_conc), 1)

  } else { if (bulkshift & regenerated_surface){
    init_params <- c(Rmax_start, ka_start, kd_start, shift)
    param_lower_bounds <- c(rep(0, length(Rmax_start)), 10, min_allowed_kd, rep(-100, num_conc))
    param_upper_bounds <- c(rep(max_RU_tol, length(Rmax_start)), 10^7, 1, rep(100, num_conc))

  } else { if(!bulkshift & regenerated_surface){
    init_params <- c(Rmax_start, ka_start, kd_start)
    param_lower_bounds <- c(rep(0, length(Rmax_start)), 10, min_allowed_kd)
    param_upper_bounds <- c(rep(max_RU_tol, length(Rmax_start)), 10^7, 1)
  }}}}

  fit_result <- minpack.lm::nls.lm(init_params,fn = fit_as_system, df = df,
                                   incl_concentrations = incl_concentrations,
                                   num_conc = num_conc,
                                   association = association,
                                   bulkshift,
                                   global_rmax = global_rmax,
                                   regenerated_surface = regenerated_surface,
                                   control = minpack.lm::nls.lm.control(maxiter = max_iterations, ptol = ptol, ftol = ftol),
                                   lower = param_lower_bounds,
                                   jac = NULL,
                                   upper = param_upper_bounds)

  pars <- unlist(fit_result$par)

  # initialize t0. If the surface is regenerated, we need to pass something to get_fit_outcomes
  t0 <- rep(0, num_conc)

  if (global_rmax){
    #pars (global "Rmax","ka", "tstart" one for each concentration)
    # tstart is only used if the surface is not regenerated

    Rmax <- pars[1]
    ka <- pars[2]

    if (!regenerated_surface){
      t0 <- pars[3:(2 + num_conc)]
      kd <- pars[(3 + num_conc)]
    } else {
      kd <- pars[3]
    }


    if (bulkshift & !regenerated_surface)
      shift <- pars[(num_conc + 4):(2*num_conc + 3)]
    else
      if (bulkshift)
        shift <- pars[4:(num_conc + 3)]


  } else {
    #pars ("Rmax" one for each concentration,"ka", "tstart" one for each concentration)

    Rmax <- pars[1:num_conc]
    ka <- pars[num_conc+1]

    if (!regenerated_surface){
      t0 <- pars[(num_conc + 2):(2*num_conc + 1)]
      kd <- pars[2*num_conc + 2]
    } else
      kd <- pars[num_conc + 2]


    if (bulkshift & !regenerated_surface)
      shift <- pars[(2*num_conc +3):(3*num_conc + 2)]
    else
      if (bulkshift)
        shift <- pars[(num_conc +3):(2*num_conc + 2)]
  }



  fit_outcomes <- get_fit_outcomes(Rmax, ka, t0, kd, df, num_conc,
                                   incl_concentrations,
                                   association = association,
                                   shift = shift,
                                   global_rmax = global_rmax)

  list("FitResult" = fit_result, "FitOutcomes" = fit_outcomes)
}


combine_output <- function(well_idx, fits_list, plot_list_out, rc_list, sample_info){

  if (!is.null(fits_list[[well_idx]]$error))
    return(NULL)

  if (sample_info[well_idx,]$`Global Rmax` == "Y")
    global_rmax <- TRUE else
      global_rmax <- FALSE

    if (sample_info[well_idx,]$`Bulkshift` == "Y")
      bulkshift <- TRUE else
        bulkshift <- FALSE

      if (sample_info[well_idx,]$Regen. == "Y" | sample_info[well_idx, ]$BaselineNegative)
        regenerated_surface <- TRUE else
          regenerated_surface <- FALSE

        num_conc <- sample_info[well_idx,]$NumInclConc

        if (global_rmax)
          Rmax_label <- "Rmax" else
            Rmax_label <- purrr::map_dfr(tibble::tibble(1:num_conc), function(x) paste("Rmax", x))

        if (!regenerated_surface)
          R0_label <- purrr::map_dfr(tibble::tibble(1:num_conc), function(x) paste("R_0", x)) else
            R0_label <- NULL
        bulkshift_label <- purrr::map_dfr(tibble::tibble(1:num_conc), function(x) paste("Bulkshift", x))


        pars <- unlist(fits_list[[well_idx]]$result$FitResult$par)

        if (bulkshift)
          par_names <- purrr::as_vector(purrr::flatten(c(Rmax_label, "ka", R0_label, "kd", bulkshift_label))) else
            par_names <- purrr::as_vector(purrr::flatten(c(Rmax_label, "ka", R0_label, "kd")))

        # result_summary <- summary(fits_list[[well_idx]]$result$FitResult)
        #R's built-in summary method doesn't play nicely when the some of the parameters hit their limiting values (the hessian is singular)
        # I've adapted the function to return NA's for std error when the limits are reached.

        result_summary <- summary_fit_with_constraints(fits_list[[well_idx]]$result$FitResult)
        #summary_fit_with_constraints returns the coefficients table from summary.minpack.lm

        summary_names <- colnames(result_summary)
        result_summary %>% tibble::as_tibble() -> par_err_table

        colnames(par_err_table) <- summary_names
        par_err_table <- suppressMessages(dplyr::bind_cols(Names = par_names, par_err_table))

        par_err_table %>%
          dplyr::filter(!stringr::str_detect(.data$Names,"R_0")) -> par_err_table
        par_err_table %>%
          dplyr::filter(!stringr::str_detect(.data$Names,"Bulkshift")) -> par_err_table


        par_names <- par_err_table$Names

        par_err_table %>%
          dplyr::filter(.data$Names == "ka") %>%
          dplyr::select("Estimate") %>%
          as.numeric() -> ka
        par_err_table %>%
          dplyr::filter(.data$Names == "ka") %>%
          dplyr::select("Std. Error") %>%
          as.numeric() -> ka_se
        par_err_table %>%
          dplyr::filter(.data$Names == "kd") %>%
          dplyr::select("Estimate") %>%
          as.numeric() -> kd
        par_err_table %>%
          dplyr::filter(.data$Names == "kd") %>%
          dplyr::select("Std. Error") %>%
          as.numeric() -> kd_se

        KD <- kd/ka
        KD_se <- KD * ((ka_se/ka)^2 + (kd_se/kd)^2)^(1/2)

        if (kd == 10^(-5)){
          kd_se <- NA
          KD_se <- NA
          idx <- which(par_err_table$Names == "kd")
          par_err_table$`Std. Error`[idx] <- NA
        }

        par_err_table %>%
          dplyr::filter(.data$Names == "ka" | .data$Names == "kd") %>%
          dplyr::select("Estimate", "Std. Error")  %>%
          dplyr::mutate(Estimate = format(signif(.data$Estimate, 3),big.mark=",",decimal.mark=".", scientific = TRUE)) %>%
          dplyr::mutate(`Std. Error` = format(signif(.data$`Std. Error`, 3),big.mark=",",decimal.mark=".", scientific = TRUE)) -> kakd_out

        par_err_table %>%
          dplyr::filter(!(.data$Names == "ka" | .data$Names == "kd")) %>%
          dplyr::select("Estimate", "Std. Error")  %>%
          dplyr::mutate(Estimate = format(round(.data$Estimate,2),big.mark=",",decimal.mark=".", scientific = FALSE))  %>%
          dplyr::mutate(`Std. Error` = format(round(.data$`Std. Error`, 2),big.mark=",",decimal.mark=".", scientific = FALSE)) -> rest_out


        KD_tbl <- tibble::tibble(Estimate = KD, `Std. Error` = KD_se)

        KD_tbl %>%
          dplyr::select("Estimate", "Std. Error")  %>%
          dplyr::mutate(Estimate = format(signif(.data$Estimate, 3),big.mark=",",decimal.mark=".", scientific = TRUE)) %>%
          dplyr::mutate(`Std. Error` = format(signif(.data$`Std. Error`, 3),big.mark=",",decimal.mark=".", scientific = TRUE)) -> KD_tbl

        par_names <- c(par_names, "KD")
        dplyr::bind_rows(rest_out, kakd_out) %>%
          dplyr::bind_rows(KD_tbl) %>%
          gridExtra::tableGrob(rows = par_names,
                               theme = gridExtra::ttheme_minimal(core=list(fg_params=list(hjust=1, x=0.9)))) -> tb1

        RU_resid <- fits_list[[well_idx]]$result$FitResult$fvec
        fits_list[[well_idx]]$result$FitOutcomes$Time -> Time_resid
        fits_list[[well_idx]]$result$FitOutcomes$Concentration -> Concentration_resid


        resid_plot <- ggplot2::ggplot(data = tibble::tibble(Residuals = RU_resid,
                                                            Time = Time_resid,
                                                            Concentration = forcats::as_factor(Concentration_resid)),
                                      ggplot2::aes(x = .data$Time, y = .data$Residuals, color = .data$Concentration)) + ggplot2::geom_point(size = 0.01) +
          ggplot2::ggtitle(label = "Residuals")

        gridExtra::grid.arrange(plot_list_out[[well_idx]], tb1, resid_plot,
                                rc_list[[well_idx]], ncol=2)
}

print_output <- function(well_idx, pages_list, plot_list_out, sample_info){

  if (is.null(pages_list[[well_idx]]$error) & !is.null(pages_list[[well_idx]]$result))
    return(gridExtra::arrangeGrob(pages_list[[well_idx]]$result))

  err_msg <- paste("The following well has an unrecoverable error:",
                   well_idx,
                   "Block ", sample_info[well_idx,]$Block,
                   "Row", sample_info[well_idx,]$Row,
                   "ROI", sample_info[well_idx,]$ROI)
  err_msg <- paste0(err_msg, sample_info$Column)

  if (is.null(plot_list_out[[well_idx]]))
    return(p1 = gridExtra::arrangeGrob(grid::textGrob(err_msg)))
  else
    return(p1 = gridExtra::arrangeGrob(grid::textGrob(err_msg), plot_list_out[[well_idx]]))

}
get_response_curve <- function(well_idx, sample_info, x_vals, y_vals,
                               all_concentrations_values,
                               incl_concentrations_values,
                               n_time_points){

  start_incl_idx <- sample_info[well_idx,]$FirstInclConcIdx
  start_idx <- sample_info[well_idx,]$FirstConcIdx
  num_conc <- sample_info[well_idx,]$NumConc
  num_incl_conc <- sample_info[well_idx,]$NumInclConc
  end_incl_idx <- start_incl_idx + num_incl_conc - 1
  end_idx <- start_idx + num_conc - 1

  ligand_desc <- sample_info[well_idx,]$Ligand
  baseline <- sample_info[well_idx,]$Baseline
  baseline_start <- sample_info[well_idx,]$`Bsl Start`
  association <- sample_info[well_idx,]$Association

  n_vals <- dim(x_vals)[2]
  names(x_vals) <- as.character(1:n_vals)
  names(y_vals) <- as.character(1:n_vals)


  Time <- x_vals[, start_idx:end_idx] %>%
    tidyr::pivot_longer(cols = tidyselect::everything()) %>%
    dplyr::arrange(as.numeric(.data$name)) %>%
    dplyr::select("value")
  RU <- y_vals[, start_idx:end_idx]%>%
    tidyr::pivot_longer(cols = tidyselect::everything()) %>%
    dplyr::arrange(as.numeric(.data$name)) %>%
    dplyr::select("value")


  numerical_concentration <- all_concentrations_values[start_idx:end_idx]
  numerical_concentration_incl <-
    incl_concentrations_values[start_incl_idx:end_incl_idx]

  purrr::map_dfr(.x = tibble::tibble(numerical_concentration),
                 .f = function(x, n_time_points)
                   rep(x, n_time_points), n_time_points) %>%
    dplyr::arrange(.data$numerical_concentration) -> numerical_concentration

  Concentrations <- numerical_concentration

  df <- suppressMessages(dplyr::bind_cols("Time" = Time, "RU" = RU, "Concentration" = Concentrations))

  colnames(df) <- c("Time", "RU", "Concentration")


  df %>% dplyr::filter((.data$Time >= baseline + baseline_start + association - 10)
                       & (.data$Time <= baseline + baseline_start + association - 5)) %>%
    dplyr::group_by(.data$Concentration) %>%
    dplyr::summarise(`Dose Response` = mean(RU, na.rm = TRUE)) -> df_RC

  df_RC %>% dplyr::mutate(Included =
                            forcats::as_factor(ifelse(.data$Concentration %in% numerical_concentration_incl,
                                                      "Yes", "No"))) -> df_RC


  ggplot2::ggplot(df_RC, ggplot2::aes(x = .data$Concentration,
                                      y = .data$`Dose Response`)) +
    ggplot2::geom_point(ggplot2::aes(color = .data$Included)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_log10() +
    ggplot2::ggtitle(ligand_desc)
}

get_csv <- function(well_idx, fits_list, sample_info){

  num_conc <- sample_info[well_idx,]$NumInclConc

  if (sample_info[well_idx,]$`Global Rmax` == "Y")
    global_rmax <- TRUE else
      global_rmax <- FALSE


    if (sample_info[well_idx, ]$Bulkshift == "Y")
      bulkshift <- TRUE else
        bulkshift <- FALSE

      if (sample_info[well_idx, ]$`Regen.` == "Y" | sample_info[well_idx,]$BaselineNegative)
        regenerated_surface <- TRUE else
          regenerated_surface <- FALSE


        if (!global_rmax){
          Rmax <- rep(NA, 5)
          Rmax_se <- rep(NA,5)
        } else {
          Rmax <- NA
          Rmax_se <- NA
        }



        Bulkshift <- rep(NA, 5)
        Bulkshift_se <- rep(NA,5)

        R0 <- rep(NA,5)
        R0_se <- rep(NA,5)

        if (is.null(fits_list[[well_idx]]$result$FitResult)){
          return(c(Rmax = Rmax, Rmax_se = Rmax_se, ka = NA, ka_se = NA, kd = NA,
                   kd_se = NA, Bulkshift = Bulkshift, Bulkshift_se = Bulkshift_se, R0 = R0, R0_se = R0_se))

        }

        pars <- unlist(fits_list[[well_idx]]$result$FitResult$par)



        result_summary <- summary_fit_with_constraints(fits_list[[well_idx]]$result$FitResult)

        # first column of summary is the estimate. Second is standard error

        summary_names <- colnames(result_summary)
        result_summary %>% tibble::as_tibble() %>% dplyr::select("Estimate", "Std. Error") -> par_err_table

        colnames(par_err_table) <- summary_names[1:2]

        if (global_rmax){
          Rmax <- par_err_table[1,]$Estimate
          Rmax_se <- par_err_table[1,]$`Std. Error`
          curr_idx <- 2

        } else {
          Rmax[1:num_conc] <- par_err_table[1:num_conc,]$Estimate
          Rmax_se[1:num_conc] <- par_err_table[1:num_conc,]$`Std. Error`
          curr_idx <- num_conc + 1

        }

        ka <- par_err_table[curr_idx,]$Estimate
        ka_se <- par_err_table[curr_idx,]$`Std. Error`
        curr_idx <- curr_idx + 1

        if (!regenerated_surface){
          R0[1:num_conc] <- par_err_table[curr_idx:(curr_idx + num_conc - 1), ]$Estimate
          R0_se[1:num_conc] <- par_err_table[curr_idx:(curr_idx + num_conc - 1), ]$`Std. Error`
          curr_idx <- curr_idx + num_conc
        }

        kd <- par_err_table[curr_idx,]$Estimate
        kd_se <- par_err_table[curr_idx,]$`Std. Error`
        curr_idx <- curr_idx + 1

        if (bulkshift){
          Bulkshift[1:num_conc] <- par_err_table[curr_idx:(curr_idx + num_conc - 1), ]$Estimate
          Bulkshift_se[1:num_conc] <- par_err_table[curr_idx:(curr_idx + num_conc - 1), ]$`Std. Error`
        }

        KD <- kd/ka
        KD_se <- KD * ((ka_se/ka)^2 + (kd_se/kd)^2)^(1/2)

        if (kd == 10^(-5)){
          kd_se <- NA
          KD_se <- NA
        }


        c(ROI = sample_info[well_idx,]$ROI,
          Rmax = suppressWarnings(as.numeric(round(Rmax, 2))),
          Rmax_se = suppressWarnings(as.numeric(round(Rmax_se, 2))),
          ka = suppressWarnings(as.numeric(round(ka,2))),
          ka_se = suppressWarnings(as.numeric(round(ka_se, 2))),
          kd = suppressWarnings(as.numeric(format(signif(kd, 3),big.mark=",",decimal.mark=".", scientific = TRUE))),
          kd_se = suppressWarnings(as.numeric(format(signif(kd_se, 3),big.mark=",",decimal.mark=".", scientific = TRUE))),
          KD = suppressWarnings(as.numeric(format(signif(KD, 3),big.mark=",",decimal.mark=".", scientific = TRUE))),
          KD_se = suppressWarnings(as.numeric(format(signif(KD_se, 3),big.mark=",",decimal.mark=".", scientific = TRUE))),
          Bulkshift = suppressWarnings(as.numeric(round(Bulkshift, 2))),
          Bulkshift_se = suppressWarnings(as.numeric(round(Bulkshift_se, 2))),
          R0 = suppressWarnings(as.numeric(round(R0, 2))),
          R0_se = suppressWarnings(as.numeric(round(R0_se, 2))))
}
