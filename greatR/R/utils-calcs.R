#' Calculate Bayesian Information Criterion
#'
#' @noRd
calc_BIC <- function(logL, num_params, num_obs) {
  return((-2 * logL) + log(num_obs) * num_params)
}

#' Calculate log likelihood of the fitted model to the observed expression data
#'
#' @noRd
calc_loglik <- function(model, data) {
  # Read expression and variance
  expression_value <- data$expression_value
  sigma_squared <- data$var

  # Predict expressions
  pred_expression_value <- stats::predict(model, newdata = data)
  dist_squared <- (pred_expression_value - expression_value)^2

  # Calculate logLik
  loglik <- -sum(dist_squared / (2 * sigma_squared))

  return(loglik)
}

#' Fit using cubic spline with K+3 parameters
#'
#' @param data Input data
#' @param x Predictor variable, by default \code{timepoint}.
#' @param y Predictor variable, by default \code{expression_value}.
#' @param num_spline_params Number of parameters, or degrees of freedom, for each spline fitting. This is used to calculate the number of \code{knots}.
#' @param degree Degree of the piecewise polynomial, default is 3 for cubic splines.
#'
#' @noRd
fit_spline_model <- function(data, x = "timepoint", y = "expression_value", num_spline_params = 4, degree = 3) {
  fit_object <- stats::lm(
    stats::as.formula(
      paste(y, "~ splines::bs(", x, ", df = num_spline_params, degree = degree)")
    ),
    data = data
  )

  return(fit_object)
}

#' Calculate variance for the observed expression data
#'
#' @noRd
calc_variance <- function(all_data, exp_sd = NA) {
  # Suppress "no visible binding for global variable" note
  gene_id <- NULL
  accession <- NULL
  timepoint <- NULL
  expression_value <- NULL
  var_poisson <- NULL
  var_range <- NULL
  var <- NULL
  num_timepoints <- NULL
  has_replicates <- NULL

  if (all(length(exp_sd) == 1, !any(is.na(exp_sd)))) {
    cli::cli_alert_info("Using provided standard deviation {.var exp_sd} = {.val {exp_sd}}.")
    all_data[, var := exp_sd^2]
  } else {
    cli::cli_alert_info("Using estimated standard deviation, as no {.var exp_sd} was provided.")

    # Check if data has replicates
    all_data[, num_timepoints := .N, by = .(gene_id, accession, timepoint)]
    all_data[, has_replicates := any(num_timepoints > 1), by = .(gene_id, accession)]
    all_data[, var := numeric()]

    # Split data
    all_data_reps <- all_data[all_data$has_replicates]
    all_data_no_reps <- all_data[!all_data$has_replicates]

    # Calculate variance for data with replicates
    if (nrow(all_data_reps) > 0) {
      # Calculate Poisson estimate for expression variance
      all_data_reps[, var_poisson := max(expression_value), by = .(gene_id, accession, timepoint)]

      # Calculate global expression variance
      all_data_reps[, var_range := (diff(range(expression_value)) / 10)^2, by = .(gene_id, accession, timepoint)]

      # Select maximum expression variance for each time point
      all_data_reps[, var := pmax(var_poisson, var_range)]
      all_data_reps[, c("var_poisson", "var_range") := NULL]
    }

    # Set variance for data with no replicates
    if (nrow(all_data_no_reps) > 0) {
      all_data_no_reps[, var := pmax(expression_value / 10, 0.25), by = .(gene_id, accession, timepoint)]
    }

    # Combine data
    all_data <- rbind(all_data_reps, all_data_no_reps)
    all_data[, c("num_timepoints", "has_replicates") := NULL]
  }

  return(all_data)
}
