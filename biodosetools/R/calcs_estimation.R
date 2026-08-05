# Incidence rare ratio (IRR) from data ----

#' Calculate IRR (this is the same as the Odds-Ratio calculation in IAEA2011)
#'
#' @param fit_coeffs Fitting coefficients matrix.
#' @param case_data Case data in data frame form.
#' @param badge_dose Suspected (e.g. badge) dose
#'
#' @return character variable representing IRR in the for (P(X=k|D=0 Gy):P(X=k|D=badge_dose)).
#'
calculate_aberr_IRR <- function(case_data,  fit_coeffs, badge_dose) {
  coef <- as.numeric(fit_coeffs[, "estimate"])
  aberr <- as.numeric(case_data[["X"]])
  cells <- as.numeric(case_data[["N"]])

  y.susp <- coef[1] + coef[2]*badge_dose + coef[3]*badge_dose*badge_dose
  y.bg <- coef[1]
  IRR <- (exp(-cells*y.bg)*(cells*y.bg)^aberr)/(exp(-cells*y.susp)*(cells*y.susp)^aberr)

  return(data.frame(N=cells, X=aberr, IRR=IRR))

}


# AIC and logLik from data ----

#' Calculate AIC (Akaike's 'An Information Criterion')
#'
#' @param general_fit_coeffs Generalised fit coefficients matrix.
#' @param data Data (dose, yield) to calculate AIC from.
#' @param dose_var Name of the dose variable (enquoted).
#' @param yield_var Name of the yield variable (enquoted).
#' @param fit_link A specification for the model link function.
#'
#' @return Numeric value of AIC.
AIC_from_data <- function(general_fit_coeffs, data, dose_var = "dose", yield_var = "yield", fit_link = "identity") {
  # Manual log-likelihood function
  loglik_from_data <- function(data, fit_link) {
    if (fit_link == "identity") {
      loglik <- -yield_fun(data[[dose_var]], general_fit_coeffs, 1) +
        log(yield_fun(data[[dose_var]], general_fit_coeffs, 1)) * data[[yield_var]] -
        lfactorial(data[[yield_var]])
    } else if (fit_link == "log") {
      loglik <- -exp(yield_fun(data[[dose_var]], general_fit_coeffs, 1)) +
        yield_fun(data[[dose_var]], general_fit_coeffs, 1) * data[[yield_var]] -
        lfactorial(data[[yield_var]])
    }
    return(sum(loglik))
  }

  logLik <- loglik_from_data(data, fit_link)

  num_params <- sum(general_fit_coeffs != 0)
  AIC <- 2 * num_params - 2 * logLik

  return(AIC)
}

# Dose estimation functions ----

#' Whole-body dose estimation (Merkle's method)
#'
#' Method based on the paper by Merkle, W. (1983). Statistical methods in
#' regression and calibration analysis of chromosome aberration data. Radiation
#' and Environmental Biophysics, 21(3), 217-233. <doi:10.1007/BF01323412>.
#'
#' @param num_cases number of cases.
#' @param case_data Case data in data frame form.
#' @param conf_int_yield Confidence interval of the yield, 83\% by default.
#' @param conf_int_curve Confidence interval of the curve, 83\% by default.
#' @param protracted_g_value Protracted \eqn{G(x)} value.
#' @param fit_coeffs Fitting coefficients matrix.
#' @param fit_var_cov_mat Fitting variance-covariance matrix.
#' @param genome_factor Genomic conversion factor used in translocations, else 1.
#' @param aberr_module Aberration module.
#'
#' @return List containing estimated doses data frame, AIC, and \code{conf_int_*} used.
#' @example man/examples/estimate_whole_body_merkle_example.R
#' @export
#'
estimate_whole_body_merkle <- function(num_cases, case_data, fit_coeffs, fit_var_cov_mat,
                                       conf_int_yield = 0.83, conf_int_curve = 0.83,
                                       protracted_g_value = 1, genome_factor = 1,
                                       aberr_module = c("dicentrics", "translocations", "micronuclei")) {
  results_list_all <- list()
  for (i in 1:num_cases){
    # Validate parameters
    aberr_module <- match.arg(aberr_module)

    # Parse aberrations and cells
    aberr <- case_data[i,"X"]
    cells <- case_data[i,"N"]
    disp <- case_data[i,"DI"]

    if (aberr_module == "translocations") {
      aberr <- correct_negative_vals(aberr - case_data[i,"Xc"])
      yield_est <- case_data[i,"Fg"]

    }else{
      yield_est <- case_data[i,"y"]
    }

    # Generalised fit coefficients and variance-covariance matrix
    general_fit_coeffs <- generalise_fit_coeffs(fit_coeffs[, "estimate"])
    general_fit_var_cov_mat <- generalise_fit_var_cov_mat(fit_var_cov_mat)

    # Correct CIs
    conf_int_curve <- conf_int_curve %>%
      correct_conf_int(general_fit_var_cov_mat, protracted_g_value, type = "curve")
    conf_int_yield <- conf_int_yield %>%
      correct_conf_int(general_fit_var_cov_mat, protracted_g_value, type = "yield")

    # Calculate CI using Exact Poisson tests
    aberr_row <- stats::poisson.test(x = round(as.numeric(aberr), 0), conf.level = conf_int_yield)[["conf.int"]]

    aberr_low <- aberr_row[1]
    aberr_upp <- aberr_row[2]

    # Correct value when there's no aberrations
    if (is.nan(as.numeric(disp)) | is.na(as.numeric(disp))) {
      disp <- Inf
    }

    if (disp >= 1 & is.finite(disp[[1]])) {

      yield_low <- (aberr_low / (cells * genome_factor))*(aberr_low/as.numeric(aberr))^sqrt(disp)
      yield_upp <- (aberr_upp / (cells * genome_factor))*(aberr_upp/as.numeric(aberr))^sqrt(disp)
    }else{
      yield_low <- aberr_low / (cells * genome_factor)
      yield_upp <- aberr_upp / (cells * genome_factor)
    }

    # TODO: possible modification IAEA§9.7.3

    # Correct "unrootable" yields
    yield_est_corr <- correct_yield(yield_est, "estimate", general_fit_coeffs, general_fit_var_cov_mat, conf_int_curve)
    if (yield_est_corr < yield_est) {
      yield_est <- 0
      yield_low <- 0
    }
    yield_upp_corr <- correct_yield(yield_upp, "upper", general_fit_coeffs, general_fit_var_cov_mat, conf_int_curve)
    if (yield_upp_corr < yield_upp){
      yield_upp <- 0
    }
    # Calculate projections
    dose_est <- project_yield(
      yield = yield_est,
      type = "estimate",
      general_fit_coeffs = general_fit_coeffs,
      general_fit_var_cov_mat = NULL,
      protracted_g_value = protracted_g_value,
      conf_int = 0
    )
    req(dose_est)

    dose_low <- project_yield(
      yield = yield_low,
      type = "lower",
      general_fit_coeffs = general_fit_coeffs,
      general_fit_var_cov_mat = general_fit_var_cov_mat,
      protracted_g_value = protracted_g_value,
      conf_int = conf_int_curve
    )

    req(dose_low)

    dose_upp <- project_yield(
      yield = yield_upp,
      type = "upper",
      general_fit_coeffs = general_fit_coeffs,
      general_fit_var_cov_mat = general_fit_var_cov_mat,
      protracted_g_value = protracted_g_value,
      conf_int = conf_int_curve
    )
    req(dose_upp)

    # Whole-body estimation results
        est_doses <- data.frame(
          lower = c(dose_low),
          estimate = c(dose_est),
          upper = c(dose_upp)
        )

        est_yield <- data.frame(
          lower = c(yield_low),
          estimate = c(yield_est),
          upper = c(yield_upp)
        )

        list.AIC <- list()
        list.AIC[["dose"]] <- est_doses$estimate
        list.AIC[["yield"]] <- est_yield$estimate
        # Calculate AIC as a GOF indicator
        AIC <- AIC_from_data(
          general_fit_coeffs, list.AIC,
          dose_var = "dose", yield_var = "yield", fit_link = "identity"
        )

        # Return objects
        results_list <- list(
          est_doses = est_doses,
          est_yield = est_yield,
          AIC = AIC,
          conf_int = c(yield = conf_int_yield, curve = conf_int_curve)
        )
        results_list_all[[i]] <- results_list

  }

  return(results_list_all)
}

#' Whole-body dose estimation (delta method)
#'
#' Method based on 2001 manual by the International Atomic Energy Agency (IAEA).
#' Cytogenetic Analysis for Radiation Dose Assessment, Technical Reports Series
#' (2001). Retrieved from \url{https://www.iaea.org/publications/6303/cytogenetic-analysis-for-radiation-dose-assessment}.
#'
#' @param num_cases number of cases.
#' @param case_data Case data in data frame form.
#' @param fit_coeffs Fitting coefficients matrix.
#' @param fit_var_cov_mat Fitting variance-covariance matrix.
#' @param conf_int Confidence interval, 95\% by default.
#' @param protracted_g_value Protracted \eqn{G(x)} value.
#' @param aberr_module Aberration module.
#'
#' @return List containing estimated doses data frame, AIC, and \code{conf_int} used.
#' @example man/examples/estimate_whole_body_delta_example.R
#' @export
estimate_whole_body_delta <- function(num_cases,case_data, fit_coeffs, fit_var_cov_mat,
                                      conf_int = 0.95, protracted_g_value = 1,
                                      aberr_module = c("dicentrics", "translocations", "micronuclei")) {

  results_list_all <- list()
  for (i in 1:num_cases){
  # Validate parameters
    aberr_module <- match.arg(aberr_module)

    # Parse parameters and coefficients
    if (aberr_module %in% c("dicentrics", "micronuclei")) {
      lambda_est <- case_data[i,"y"]
    } else if (aberr_module == "translocations") {
      lambda_est <- case_data[i,"Fg"]
    }

    # Calculate variance of lambda
    disp <- case_data[i,"DI"]

    # Correct value when there's no aberrations
    if (is.nan(disp) | is.na(disp)) {
      disp <- Inf
    }

    if (disp >= 1) {
      # Use empirical error sqrt(var / N) if disp >= 1
      if (aberr_module %in% c("dicentrics", "micronuclei")) {
        lambda_est_sd <- case_data[i,"y_err"]
      } else if (aberr_module == "translocations") {
        lambda_est_sd <- case_data[i,"Fg_err"]
      }
    } else {
      # Use Poisson error if disp < 1
      lambda_est_sd <- sqrt(case_data[i,"X"]) / case_data[i,"N"]
    }

    # Get confidence interval of lambda estimates
    lambda_low <- lambda_est - stats::qnorm(conf_int + (1 - conf_int) / 2) * lambda_est_sd
    lambda_upp <- lambda_est + stats::qnorm(conf_int + (1 - conf_int) / 2) * lambda_est_sd

    # Generalised fit coefficients and variance-covariance matrix
    general_fit_coeffs <- generalise_fit_coeffs(fit_coeffs[, "estimate"])
    general_fit_var_cov_mat <- generalise_fit_var_cov_mat(fit_var_cov_mat)

    coeff_C <- general_fit_coeffs[[1]]
    coeff_alpha <- general_fit_coeffs[[2]]
    coeff_beta <- general_fit_coeffs[[3]]

    # Calculate dose projection
    dose_est <- project_yield(
      yield = lambda_est,
      type = "estimate",
      general_fit_coeffs = general_fit_coeffs,
      general_fit_var_cov_mat = NULL,
      protracted_g_value = protracted_g_value,
      conf_int = 0
    )
    req(dose_est)

    # Get standard error of dose estimate by deltamethod()
    cov_extended <- matrix(0, nrow = 4, ncol = 4)
    cov_extended[1:3, 1:3] <- general_fit_var_cov_mat
    cov_extended[4, 4] <- lambda_est_sd^2

    dose_est_sd <- get_deltamethod_std_err(
      fit_is_lq = isFALSE(coeff_beta == 0),
      variable = "dose",
      mean_estimate = c(coeff_C, coeff_alpha, coeff_beta, lambda_est),
      cov_estimate = cov_extended,
      protracted_g_value = protracted_g_value
    )

    # Get confidence interval of dose estimates
    dose_low <- dose_est - stats::qnorm(conf_int + (1 - conf_int) / 2) * dose_est_sd
    dose_upp <- dose_est + stats::qnorm(conf_int + (1 - conf_int) / 2) * dose_est_sd

    # Correct negative values
    lambda_low <- correct_negative_vals(lambda_low)
    lambda_upp <- correct_negative_vals(lambda_upp)
    dose_low <- correct_negative_vals(dose_low)
    dose_est <- correct_negative_vals(dose_est)
    dose_upp <- correct_negative_vals(dose_upp)

    # Correct "unrootable" yields and respective doses
    lambda_est_corr <- correct_yield(lambda_est, "estimate", general_fit_coeffs, general_fit_var_cov_mat, conf_int = 0)
    if (lambda_est_corr < lambda_est) {
      lambda_est <- 0
      lambda_low <- 0
      lambda_upp <- 0
      dose_est <- 0
      dose_low <- 0
      dose_upp <- 0
    }

      est_doses <- data.frame(
        lower = c(dose_low),
        estimate = c(dose_est),
        upper = c(dose_upp)
      )

      est_yield <- data.frame(
        lower = c(lambda_low),
        estimate = c(lambda_est),
        upper = c(lambda_upp)
      )

      list.AIC <- list()
      list.AIC[["dose"]] <- est_doses$estimate
      list.AIC[["yield"]] <- est_yield$estimate
      # Calculate AIC as a GOF indicator
      AIC <- AIC_from_data(
        general_fit_coeffs, list.AIC,
        dose_var = "dose", yield_var = "yield", fit_link = "identity"
      )

      # Return objects
      results_list <- list(
        est_doses = est_doses,
        est_yield = est_yield,
        AIC = AIC,
        conf_int = conf_int
      )
      results_list_all[[i]] <- results_list

    }
    return(results_list_all)
}

#' Partial-body dose estimation (Dolphin's method)
#'
#' Method based on the paper by Dolphin, G. W. (1969). Biological Dosimetry with
#' Particular Reference to Chromosome Aberration Analysis: A Review of Methods.
#' International Atomic Energy Agency (IAEA) Retrieved from
#' \url{https://inis.iaea.org/search/search.aspx?orig_q=RN:45029080}.
#'
#' @param num_cases number of cases.
#' @param case_data Case data in data frame form.
#' @param fit_coeffs Fitting coefficients matrix.
#' @param fit_var_cov_mat Fitting variance-covariance matrix.
#' @param conf_int Confidence interval, 95\% by default.
#' @param protracted_g_value Protracted \eqn{G(x)} value.
#' @param genome_factor Genomic conversion factor used in translocations, else 1.
#' @param gamma Survival coefficient of irradiated cells.
#' @param aberr_module Aberration module.
#'
#' @return List containing estimated doses data frame, observed fraction of cells scored
#' which were irradiated, estimated fraction of irradiated blood data frame, AIC, and
#' \code{conf_int_*} used.
#' @example man/examples/estimate_partial_body_dolphin_example.R
#' @export
estimate_partial_body_dolphin <- function(num_cases, case_data, fit_coeffs, fit_var_cov_mat,
                                          conf_int = 0.95, protracted_g_value = 1,
                                          genome_factor = 1, gamma,
                                          aberr_module = c("dicentrics", "translocations", "micronuclei")) {
  # Validate parameters
  aberr_module <- match.arg(aberr_module)

  # Function to get the fisher information matrix
  get_cov_ZIP_ML <- function(lambda, pi, cells) {
    # For the parameters of a ZIP distribution (lambda and pi) where 1-p is the fraction of extra zeros
    aux_denominator <- pi + (1 - pi) * exp(lambda)
    info_mat <- matrix(NA, nrow = 2, ncol = 2)
    info_mat[1, 1] <- cells * pi * ((pi - 1) / aux_denominator + 1 / lambda)
    info_mat[1, 2] <- cells / aux_denominator
    info_mat[2, 1] <- info_mat[1, 2]
    info_mat[2, 2] <- cells * (exp(lambda) - 1) / (pi * aux_denominator)

    # Solve system
    cov_est <- solve(info_mat)

    return(cov_est)
  }

  results_list_all_partial <- list()
  for (i in 1:num_cases){
  # Input of the parameter gamma and its variance
    d0 <- 1 / gamma

    # Get fitting model variables
    aberr <- case_data[i,"X"]
    cells <- case_data[i,"N"]
    cells_0 <- case_data[i,"C0"]
    cells_1 <- case_data[i,"C1"]

    # Modify results for translocations
    if (aberr_module == "translocations") {
      aberr <- aberr - case_data[i,"Xc"]
    }

    # Generalised fit coefficients and variance-covariance matrix
    general_fit_coeffs <- generalise_fit_coeffs(fit_coeffs[, "estimate"])
    general_fit_var_cov_mat <- generalise_fit_var_cov_mat(fit_var_cov_mat)

    # Parse fitting coefficients
    coeff_C <- general_fit_coeffs[[1]]
    coeff_alpha <- general_fit_coeffs[[2]]
    coeff_beta <- general_fit_coeffs[[3]]

    # If there are no cells with > 1 dic, the results include only NAs
    # This should be handled somewhere downstream
    if (cells - (cells_0 + cells_1) == 0) {
        est_doses <- data.frame(
          lower = rep(NA),
          estimate = rep(NA),
          upper = rep(NA)
        )
        est_yield <- data.frame(
          lower = rep(NA),
          estimate = rep(NA),
          upper = rep(NA)
        )
        est_frac <- data.frame(
          lower = rep(NA),
          estimate = rep(NA),
          upper = rep(NA)
        )
        est_metaphases_frac <- data.frame(
          pi_estimate = rep(NA),
          pi_std_err = rep(NA)
        )
        results_list <- list(
          est_doses = est_doses,
          est_yield = est_yield,
          est_frac = est_frac,
          est_metaphases_frac = est_metaphases_frac,
          AIC = NA,
          conf_int = conf_int
        )

        results_list_all_partial[[i]] <- results_list


    }else{

      # Get estimates for pi and lambda
      lambda_est <- stats::uniroot(function(yield) {
        yield / (1 - exp(-yield)) - as.numeric(aberr) / (as.numeric(cells) - as.numeric(cells_0))
      }, c(1e-16, 100))$root

      if (aberr_module == "translocations") {
        lambda_est <- lambda_est / genome_factor
      }

      pi_est <- as.numeric(aberr) / (as.numeric(lambda_est) * as.numeric(cells))

      # Get the covariance matrix for the parameters of the ZIP distribution
      cov_est <- get_cov_ZIP_ML(lambda_est, pi_est, as.numeric(cells))
      lambda_est_sd <- sqrt(cov_est[1, 1])

      est_metaphases_frac <- data.frame(
        pi_estimate = pi_est,
        pi_std_err = sqrt(cov_est[2, 2])
      )

      # Get confidence interval of lambda estimates
      lambda_low <- lambda_est - stats::qnorm(conf_int + (1 - conf_int) / 2) * lambda_est_sd
      lambda_upp <- lambda_est + stats::qnorm(conf_int + (1 - conf_int) / 2) * lambda_est_sd

      # Calculate dose projection
      dose_est <- project_yield(
        yield = lambda_est,
        type = "estimate",
        general_fit_coeffs = general_fit_coeffs,
        general_fit_var_cov_mat = NULL,
        protracted_g_value = protracted_g_value,
        conf_int = 0
      )
      req(dose_est)

      # Get standard error of dose estimate by deltamethod()
      cov_extended <- matrix(0, nrow = 4, ncol = 4)
      cov_extended[1:3, 1:3] <- general_fit_var_cov_mat
      cov_extended[4, 4] <- lambda_est_sd^2

      dose_est_sd <- get_deltamethod_std_err(
        fit_is_lq = isFALSE(coeff_beta == 0),
        variable = "dose",
        mean_estimate = c(coeff_C, coeff_alpha, coeff_beta, lambda_est),
        cov_estimate = cov_extended,
        protracted_g_value = protracted_g_value
      )

      # Get confidence interval of dose estimates
      dose_low <- dose_est - stats::qnorm(conf_int + (1 - conf_int) / 2) * dose_est_sd
      dose_upp <- dose_est + stats::qnorm(conf_int + (1 - conf_int) / 2) * dose_est_sd

      # Correct negative values
      lambda_low <- correct_negative_vals(lambda_low)
      lambda_upp <- correct_negative_vals(lambda_upp)
      dose_low <- correct_negative_vals(dose_low)
      dose_est <- correct_negative_vals(dose_est)
      dose_upp <- correct_negative_vals(dose_upp)

        est_doses <- data.frame(
          lower = c(dose_low),
          estimate = c(dose_est),
          upper = c(dose_upp)
        )
        est_yield <- data.frame(
          lower = c(lambda_low),
          estimate = c(lambda_est),
          upper = c(lambda_upp)
        )

      list.AIC <- list()
      list.AIC[["dose"]] <- est_doses$estimate
      list.AIC[["yield"]] <- est_yield$estimate
      # Calculate AIC as a GOF indicator
      AIC <- AIC_from_data(
        general_fit_coeffs, list.AIC,
        dose_var = "dose", yield_var = "yield", fit_link = "identity"
      )

      # Get estimate for fraction irradiated
      F_est <- pi_est * exp(dose_est / d0) / (1 - pi_est + pi_est * exp(dose_est / d0))

      # Get standard error of fraction irradiated by deltamethod()
      cov_extended_F <- matrix(0, nrow = 5, ncol = 5)
      cov_extended_F[1:3, 1:3] <- general_fit_var_cov_mat
      cov_extended_F[4:5, 4:5] <- cov_est

      F_est_sd <- get_deltamethod_std_err(
        fit_is_lq = isFALSE(coeff_beta == 0),
        variable = "fraction_partial",
        mean_estimate = c(coeff_C, coeff_alpha, coeff_beta, lambda_est, pi_est),
        cov_estimate = cov_extended_F,
        d0 = d0
      )

      # Get confidence interval of fraction irradiated
      F_upp <- F_est + stats::qnorm(conf_int + (1 - conf_int) / 2) * F_est_sd
      F_low <- F_est - stats::qnorm(conf_int + (1 - conf_int) / 2) * F_est_sd

      # Set to zero if F < 0 and to 1 if F > 1
      F_low <- correct_boundary(F_low)
      F_est <- correct_boundary(F_est)
      F_upp <- correct_boundary(F_upp)

      # Estimated fraction
      est_frac <- data.frame(
        lower = c(F_low),
        estimate = c(F_est),
        upper = c(F_upp)
      )
      results_list <- list(
        est_doses = est_doses,
        est_yield = est_yield,
        est_frac = est_frac,
        est_metaphases_frac = est_metaphases_frac,
        AIC = AIC,
        conf_int = conf_int
      )

      results_list_all_partial[[i]] <- results_list
      }
  }
    return(results_list_all_partial)
}

#' Heterogeneous dose estimation (Mixed Poisson model)
#'
#' Method based on the paper by Pujol, M. et al. (2016). A New Model for
#' Biological Dose Assessment in Cases of Heterogeneous Exposures to Ionizing
#' Radiation. Radiation Research, 185(2), 151-162. <doi:10.1667/RR14145.1>.
#'
#' @param case_data Case data in data frame form.
#' @param fit_coeffs Fitting coefficients matrix.
#' @param fit_var_cov_mat Fitting variance-covariance matrix.
#' @param conf_int Confidence interval, 95\% by default.
#' @param protracted_g_value Protracted \eqn{G(x)} value.
#' @param gamma Survival coefficient of irradiated cells.
#' @param gamma_error Error of the survival coefficient of irradiated cells.
#'
#' @return List containing estimated mixing proportions data frame, estimated yields data
#' frame, estimated doses data frame, estimated fraction of irradiated blood data frame,
#' AIC, and \code{conf_int_*} used.
#' @example man/examples/estimate_hetero_mixed_poisson_example.R
#' @export
estimate_hetero_mixed_poisson <- function(case_data, fit_coeffs, fit_var_cov_mat,
                                          conf_int = 0.95, protracted_g_value = 1,
                                          gamma, gamma_error) {
  # Select translocation counts
  counts <- case_data[1, ] %>%
    dplyr::select(dplyr::contains("C")) %>%
    as.numeric()

  # Get fitting model variables
  cells <- case_data[["N"]]
  cells_0 <- case_data[["C0"]]
  cells_1 <- case_data[["C1"]]

  # Likelihood function
  loglik <- function(coeffs) {
    loglik <- sum(log(coeffs[1] * stats::dpois(y, coeffs[2]) + (1 - coeffs[1]) * stats::dpois(y, coeffs[3])))

    return(-loglik)
  }

  # Function to calculate fractions of irradiated blood
  get_fraction <- function(g, f, mu1, mu2) {
    dose1_est <- project_yield(
      yield = mu1,
      type = "estimate",
      general_fit_coeffs = general_fit_coeffs,
      general_fit_var_cov_mat = NULL,
      protracted_g_value = protracted_g_value,
      conf_int = 0
    )

    if (mu2 <= 0.01) {
      dose2_est <- 0
    } else {
      dose2_est <- project_yield(
        yield = mu2,
        type = "estimate",
        general_fit_coeffs = general_fit_coeffs,
        general_fit_var_cov_mat = NULL,
        protracted_g_value = protracted_g_value,
        conf_int = 0
      )
    }

    frac <- f / (f + (1 - f) * exp(g * (dose2_est - dose1_est)))

    return(frac)
  }

  # If there are no cells with > 1 dic, the results include only NAs
  # This should be handled somewhere downstream
  if (cells - (cells_0 + cells_1) == 0) {
    # Estimated yields
    est_yields <- data.frame(
      yield1 = rep(NA, 3),
      yield2 = rep(NA, 3)
    )

    # Estimated mixing proportion
    est_mixing_prop <- data.frame(
      y_estimate = rep(NA, 2),
      y_std_err = rep(NA, 2),
      f_estimate = rep(NA, 2),
      f_std_err = rep(NA, 2)
    )

    # Estimated doses
    est_doses <- data.frame(
      dose1 = rep(NA, 3),
      dose2 = rep(NA, 3)
    )

    # Estimated fraction
    est_frac <- data.frame(
      estimate = rep(NA, 2),
      std_err = rep(NA, 2)
    )
  } else {
    # Get cases data and store in vector y
    y <- rep(seq(0, length(counts) - 1, 1), counts)
    x <- c(rep(1, length(y)))

    fit <- mixtools::poisregmixEM(y, x, addintercept = FALSE, k = 2)

    # Generalised fit coefficients and variance-covariance matrix
    general_fit_coeffs <- generalise_fit_coeffs(fit_coeffs[, "estimate"])
    general_fit_var_cov_mat <- generalise_fit_var_cov_mat(fit_var_cov_mat)

    # Parse fitting coefficients
    coeff_C <- general_fit_coeffs[[1]]
    coeff_alpha <- general_fit_coeffs[[2]]
    coeff_beta <- general_fit_coeffs[[3]]

    # Calculate Maximum Likielihood Estimation
    MLE <- stats::optim(
      par = c(fit$lambda[1], exp(fit$beta)[1], exp(fit$beta)[2]),
      fn = loglik,
      method = c("L-BFGS-B"),
      lower = c(0.01, 0.01, 0.01),
      upper = c(0.99, Inf, Inf),
      hessian = TRUE
    )

    cov_fisher <- solve(MLE$hessian)
    frac1 <- MLE$par[1]
    yield1_est <- MLE$par[2]
    yield2_est <- MLE$par[3]

    if (yield1_est < yield2_est) {
      yield1_est <- MLE$par[3]
      yield2_est <- MLE$par[2]
      frac1 <- 1 - frac1
      cov_fisher <- cov_fisher[c(1, 3, 2), c(1, 3, 2)]
    }

    # Estimated parameters and its standard errors
    estim_fisher <- c(frac1, yield1_est, yield2_est)
    std_fisher <- sqrt(diag(cov_fisher))

    yield1_low <- yield1_est - stats::qnorm(conf_int + (1 - conf_int) / 2) * std_fisher[2]
    yield1_upp <- yield1_est + stats::qnorm(conf_int + (1 - conf_int) / 2) * std_fisher[2]

    yield2_low <- yield2_est - stats::qnorm(conf_int + (1 - conf_int) / 2) * std_fisher[3]
    yield2_upp <- yield2_est + stats::qnorm(conf_int + (1 - conf_int) / 2) * std_fisher[3]

    # Correct negative values
    yield1_est <- correct_negative_vals(yield1_est)
    yield1_low <- correct_negative_vals(yield1_low)
    yield1_upp <- correct_negative_vals(yield1_upp)
    yield2_est <- correct_negative_vals(yield2_est)
    yield2_low <- correct_negative_vals(yield2_low)
    yield2_upp <- correct_negative_vals(yield2_upp)

    est_yields <- data.frame(
      yield1 = c(yield1_low, yield1_est, yield1_upp),
      yield2 = c(yield2_low, yield2_est, yield2_upp)
    ) %>%
      `row.names<-`(c("lower", "estimate", "upper"))

    # Estimated mixing proportion
    est_mixing_prop <- data.frame(
      y_estimate = c(estim_fisher[2], estim_fisher[3]),
      y_std_err = c(std_fisher[2], std_fisher[3]),
      f_estimate = c(estim_fisher[1], 1 - estim_fisher[1]),
      f_std_err = rep(std_fisher[1], 2)
    ) %>%
      `row.names<-`(c("dose1", "dose2"))

    # Estimated received doses
    dose1_est <- project_yield(
      yield = yield1_est,
      type = "estimate",
      general_fit_coeffs = general_fit_coeffs,
      general_fit_var_cov_mat = NULL,
      protracted_g_value = protracted_g_value,
      conf_int = 0
    )

    dose2_est <- project_yield(
      yield = yield2_est,
      type = "estimate",
      general_fit_coeffs = general_fit_coeffs,
      general_fit_var_cov_mat = NULL,
      protracted_g_value = protracted_g_value,
      conf_int = 0
    )

    # Get standard error of dose estimate by deltamethod()
    cov_extended <- matrix(0, nrow = 4, ncol = 4)
    cov_extended[1:3, 1:3] <- general_fit_var_cov_mat
    cov_extended1 <- cov_extended2 <- cov_extended
    cov_extended1[4, 4] <- cov_fisher[2, 2]
    cov_extended2[4, 4] <- cov_fisher[3, 3]

    dose1_est_sd <- get_deltamethod_std_err(
      fit_is_lq = isFALSE(coeff_beta == 0),
      variable = "dose",
      mean_estimate = c(coeff_C, coeff_alpha, coeff_beta, yield1_est),
      cov_estimate = cov_extended1,
      protracted_g_value = protracted_g_value
    )

    dose2_est_sd <- get_deltamethod_std_err(
      fit_is_lq = isFALSE(coeff_beta == 0),
      variable = "dose",
      mean_estimate = c(coeff_C, coeff_alpha, coeff_beta, yield2_est),
      cov_estimate = cov_extended2,
      protracted_g_value = protracted_g_value
    )

    # Get confidence interval of dose estimates
    dose1_low <- dose1_est - stats::qnorm(conf_int + (1 - conf_int) / 2) * dose1_est_sd
    dose1_upp <- dose1_est + stats::qnorm(conf_int + (1 - conf_int) / 2) * dose1_est_sd
    dose2_upp <- dose2_est + stats::qnorm(conf_int + (1 - conf_int) / 2) * dose2_est_sd
    dose2_low <- dose2_est - stats::qnorm(conf_int + (1 - conf_int) / 2) * dose2_est_sd

    # Correct negative values
    dose1_est <- correct_negative_vals(dose1_est)
    dose1_low <- correct_negative_vals(dose1_low)
    dose1_upp <- correct_negative_vals(dose1_upp)
    dose2_est <- correct_negative_vals(dose2_est)
    dose2_low <- correct_negative_vals(dose2_low)
    dose2_upp <- correct_negative_vals(dose2_upp)

    est_doses <- data.frame(
      dose1 = c(dose1_low, dose1_est, dose1_upp),
      dose2 = c(dose2_low, dose2_est, dose2_upp)
    ) %>%
      `row.names<-`(c("lower", "estimate", "upper"))

    # Estimated fraction of irradiated blood for dose dose1
    F1_est <- get_fraction(gamma, frac1, yield1_est, yield2_est)
    F1_est <- correct_boundary(F1_est)
    F2_est <- 1 - F1_est

    # Get standard error of fraction irradiated by deltamethod()
    cov_extended_F <- matrix(0, nrow = 4, ncol = 4)
    diag(cov_extended_F) <- c(gamma_error^2, cov_fisher[1, 1], dose1_est_sd^2, dose2_est_sd^2)

    F1_est_sd <- get_deltamethod_std_err(
      fit_is_lq = NULL,
      variable = "fraction_hetero",
      mean_estimate = c(gamma, frac1, dose1_est, dose2_est),
      cov_estimate = cov_extended_F
    )

    est_frac <- data.frame(
      estimate = c(F1_est, F2_est),
      std_err = rep(F1_est_sd, 2)
    ) %>%
      `row.names<-`(c("dose1", "dose2"))

    # Calculate AIC as a GOF indicator
    est_doses_AIC <- data.frame(
      dose = as.numeric(est_doses["estimate", ]),
      yield = as.numeric(est_yields["estimate", ])
    )

    AIC <- AIC_from_data(
      general_fit_coeffs, est_doses_AIC,
      dose_var = "dose", yield_var = "yield", fit_link = "identity"
    )
  }

  # Return objects
  results_list <- list(
    est_mixing_prop = est_mixing_prop,
    est_yields = est_yields,
    est_doses = est_doses,
    est_frac = est_frac,
    AIC = AIC,
    conf_int = conf_int
  )

  return(results_list)
}
