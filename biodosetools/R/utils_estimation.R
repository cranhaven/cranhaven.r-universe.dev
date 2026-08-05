# Protracted function ----

#' Calculate protracted function \eqn{G(x)}
#'
#' Calculation based on the paper by Lea, D. E. & Catcheside, D. G. (1942). The
#' mechanism of the induction by radiation of chromosome aberrations
#' in\emph{Tradescantia}. Journal of Genetics, 44(2-3), 216-245.
#' <doi:10.1007/BF02982830>.
#'
#' @param time Time over which the irradiation occurred.
#' @param time_0 The mean lifetime of the breaks, which has been shown to be on the order of ~ 2 hours (default value).
#'
#' @return Numeric value of \eqn{G(x)}.
#' @example man/examples/protracted_g_function_example.R
#' @export
protracted_g_function <- function(time, time_0 = 2) {
  x <- time / time_0
  g_value <- (2 / x^2) * (x - 1 + exp(-x))

  return(g_value)
}

# Generalised curves ----

#' Calculate yield
#'
#' @param dose Numeric value of dose.
#' @param general_fit_coeffs Generalised fit coefficients matrix.
#' @param protracted_g_value Protracted \eqn{G(x)} value.
#'
#' @return Numeric value of yield.
yield_fun <- function(dose, general_fit_coeffs, protracted_g_value = 1) {
  yield <- general_fit_coeffs[[1]] +
    general_fit_coeffs[[2]] * dose +
    general_fit_coeffs[[3]] * dose^2 * protracted_g_value

  return(yield)
}

#' Calculate R regression confidence factor
#'
#' Calculate R regression confidence factor depending on selected confidence interval and type of fit.
#'
#' @param conf_int Confidence interval, 95\% by default.
#' @param general_fit_coeffs Generalised fit coefficients matrix.
#'
#' @return Numeric value of R regression confidence factor.
R_factor <- function(general_fit_coeffs, conf_int = 0.95) {
  chisq_df <- sum(general_fit_coeffs != 0)
  r_factor <- sqrt(stats::qchisq(conf_int, df = chisq_df))

  return(r_factor)
}

#' Calculate yield error
#'
#' Calculate yield error using Merkle's method
#'
#' @param dose Numeric value of dose.
#' @param general_fit_var_cov_mat Generalised variance-covariance matrix.
#' @param protracted_g_value Protracted \eqn{G(x)} value.
#'
#' @return Numeric value of yield error.
yield_error_fun <- function(dose, general_fit_var_cov_mat = NULL, protracted_g_value = 1) {
  # Special case for yield estimate
  if (is.null(general_fit_var_cov_mat)) {
    return(0)
  }

  # Calculation for lower and upper yields
  res <- general_fit_var_cov_mat[["coeff_C", "coeff_C"]] +
    general_fit_var_cov_mat[["coeff_alpha", "coeff_alpha"]] * dose^2 +
    general_fit_var_cov_mat[["coeff_beta", "coeff_beta"]] * dose^4 * protracted_g_value^2 +
    2 * general_fit_var_cov_mat[["coeff_C", "coeff_alpha"]] * dose +
    2 * general_fit_var_cov_mat[["coeff_C", "coeff_beta"]] * dose^2 * protracted_g_value +
    2 * general_fit_var_cov_mat[["coeff_alpha", "coeff_beta"]] * dose^3 * protracted_g_value
  if (sum(res < 0) > 0) {
    rep(0, length(res))
  } else {
    return(sqrt(res))
  }
}

#' Calculate yield from dose
#'
#' @param dose Numeric value of dose.
#' @param type Type of yield calculation. Can be "estimate", "lower", or "upper".
#' @param general_fit_coeffs Generalised fit coefficients matrix.
#' @param general_fit_var_cov_mat Generalised variance-covariance matrix.
#' @param protracted_g_value Protracted \eqn{G(x)} value.
#' @param conf_int Curve confidence interval, 95\% by default.
#'
#' @return Numeric value of yield.
calculate_yield <- function(dose, type = c("estimate", "lower", "upper"), general_fit_coeffs, general_fit_var_cov_mat = NULL, protracted_g_value = 1, conf_int = 0.95) {
  # Validate parameters
  type <- match.arg(type)

  # Calculate factor per type
  type_factor <- switch(type,
    "estimate" = 0,
    "lower" = 1,
    "upper" = -1
  )

  # Calculate yield
  yield <- yield_fun(dose, general_fit_coeffs, protracted_g_value) +
    as.numeric(type_factor) *
      R_factor(general_fit_coeffs, conf_int) *
      yield_error_fun(dose, general_fit_var_cov_mat, protracted_g_value)

  return(yield)
}

#' Calculate theoretical yield infimum
#'
#' @param type Type of yield calculation. Can be "estimate", "lower", or "upper".
#' @param general_fit_coeffs Generalised fit coefficients matrix.
#' @param general_fit_var_cov_mat Generalised variance-covariance matrix.
#' @param conf_int Curve confidence interval, 95\% by default.
#'
#' @return Numeric value of yield infimum.
calculate_yield_infimum <- function(type = c("estimate", "lower", "upper"), general_fit_coeffs, general_fit_var_cov_mat = NULL, conf_int = 0.95) {
  # Calculate yield
  yield <- calculate_yield(0, type, general_fit_coeffs, general_fit_var_cov_mat, 1, conf_int)

  return(yield)
}

#' Correct yield confidence interval
#'
#' Correct yield confidence interval if simple method is required.
#'
#' @param conf_int Confidence interval.
#' @param protracted_g_value Protracted \eqn{G(x)} value.
#' @param type Type of yield calculation. Can be "estimate", "lower", or "upper".
#' @param dose Numeric value of dose.
#' @param general_fit_var_cov_mat Generalised variance-covariance matrix.
#'
#' @return Numeric value of corrected confidence interval.
correct_conf_int <- function(conf_int, general_fit_var_cov_mat, protracted_g_value = 1, type, dose = seq(0, 10, 0.2)) {
  res <- general_fit_var_cov_mat[["coeff_C", "coeff_C"]] +
    general_fit_var_cov_mat[["coeff_alpha", "coeff_alpha"]] * dose^2 +
    general_fit_var_cov_mat[["coeff_beta", "coeff_beta"]] * dose^4 * protracted_g_value^2 +
    2 * general_fit_var_cov_mat[["coeff_C", "coeff_alpha"]] * dose +
    2 * general_fit_var_cov_mat[["coeff_C", "coeff_beta"]] * dose^2 * protracted_g_value +
    2 * general_fit_var_cov_mat[["coeff_alpha", "coeff_beta"]] * dose^3 * protracted_g_value
  if (sum(res <= 0) > 1) {
    if (type == "curve") {
      conf_int <- 0
    } else if (type == "yield") {
      conf_int <- 0.95
    }
  }

  return(conf_int)
}

# Projection functions ----

#' Project yield into dose-effect fitting curve
#'
#' @param yield Yield to be projected.
#' @param type Type of yield calculation. Can be "estimate", "lower", or "upper".
#' @param general_fit_coeffs Generalised fit coefficients matrix.
#' @param general_fit_var_cov_mat Generalised variance-covariance matrix.
#' @param protracted_g_value Protracted \eqn{G(x)} value.
#' @param conf_int Curve confidence interval, 95\% by default.
#'
#' @export
#' @example man/examples/project_yield_example.R
#' @return Numeric value of projected dose.
project_yield <- function(yield, type = "estimate", general_fit_coeffs, general_fit_var_cov_mat = NULL, protracted_g_value = 1, conf_int = 0.95) {
  yield_inf <- calculate_yield_infimum(type, general_fit_coeffs, general_fit_var_cov_mat, conf_int)

  if (!is.numeric(yield)) {
    yield <- as.numeric(yield)
  }
  if (length(yield) > 1) {
    yield <- yield[[1]]
  }


  if (!is.numeric(yield_inf)) {
    yield_inf <- as.numeric(yield_inf)
  }
  if (length(yield_inf) > 1) {
    yield_inf <- yield_inf[[1]]
  }

  if (yield > yield_inf) {
    projected_dose <- tryCatch({
      stats::uniroot(function(dose) {
        calculate_yield(dose, type, general_fit_coeffs, general_fit_var_cov_mat, protracted_g_value, conf_int) - yield
      }, c(1e-16, 100))$root
    }, error = function(e) {
      # showModal(modalDialog(
      #   title = "Error",
      #   "Execution halted due to uniroot() failure. The projected dose can not be found.",
      #   easyClose = TRUE,
      #   footer = modalButton("Close")
      # ))
      projected_dose <- NULL
    })
  }else{
    projected_dose <- 0
  }


  return(projected_dose)
}

# Delta method functions ----

#' Get standard errors using delta method
#'
#' Delta method for approximating the standard error of a transformation \eqn{g(X)} of a random variable \eqn{X = (x1, x2, ...)}, given estimates of the mean and covariance matrix of \eqn{X}.
#'
#' @param fit_is_lq Whether the fit is linear quadratic (\code{TRUE}) or linear (\code{FALSE}).
#' @param variable Variable resulting of the transformation \eqn{g(X)}.
#' @param mean_estimate The estimated mean of \eqn{X}.
#' @param cov_estimate The estimated covariance matrix of \eqn{X}.
#' @param protracted_g_value Protracted \eqn{G(x)} value.
#' @param d0 Survival coefficient of irradiated cells.
#'
#' @return Numeric value containing the standard error of the dose estimate.
get_deltamethod_std_err <- function(fit_is_lq, variable = c("dose", "fraction_partial", "fraction_hetero"),
                                    mean_estimate, cov_estimate,
                                    protracted_g_value = NA, d0 = NA) {
  # Validate parameters
  variable <- match.arg(variable)

  if (variable == "dose") {
    if (fit_is_lq) {
      # Formula parameters: {x1, x2, x3, x4} = {C, alpha, beta, lambda_est}
      formula <- paste(
        "~", "(-x2 + sqrt(x2^2 + 4 * x3 *", protracted_g_value, "* (x4 - x1)))", "/",
        "(2 * x3 *", protracted_g_value, ")",
        sep = ""
      )
    } else {
      # Formula parameters: {x1, x2, x4} = {C, alpha, lambda_est}
      formula <- "~ (x4 - x1) / x2"
    }
  } else if (variable == "fraction_partial") {
    if (fit_is_lq) {
      # Formula parameters: {x1, x2, x3, x4, x5} = {C, alpha, beta, lambda_est, pi_est}
      formula <- paste(
        "~", "x5 * exp((-x2 + sqrt(x2^2 + 4 * x3 * (x4 - x1))) / (2 * x3 *", d0, "))", "/",
        "(1 - x5 + x5 * exp((-x2 + sqrt(x2^2 + 4 * x3 * (x4 - x1))) / (2 * x3 *", d0, ")))",
        sep = ""
      )
    } else {
      # Formula parameters: {x1, x2, x4, x5} = {C, alpha, lambda_est, pi_est}
      formula <- paste(
        "~", "x5 * exp((x4 - x1) / (x2 *", d0, "))", "/",
        "1 - x5 + x5 * exp((x4 - x1) / (x2 *", d0, "))",
        sep = ""
      )
    }
  } else if (variable == "fraction_hetero") {
    # Formula parameters: {x1, x2, x3, x4} =  {gamma, frac, D1, D2}
    formula <- "~ x2 / (x2 + (1 - x2) * exp(x1 * (x4 - x3)))"
  }

  dose_est_sd <- msm::deltamethod(
    g = stats::as.formula(formula),
    mean = mean_estimate,
    cov = cov_estimate
  )

  return(dose_est_sd)
}

# Correction functions ----

#' Correct negative values
#'
#' @param x Numeric value.
#'
#' @return Numeric value corrected to zero if negative.
correct_negative_vals <- function(x) {
  x_corrected <- ifelse(x < 0, 0, x)

  return(x_corrected)
}


#' Correct yields if they are below the curve
#'
#' @param yield Numeric value of yield.
#' @param type Type of yield calculation. Can be "estimate", "lower", or "upper".
#' @param general_fit_coeffs Generalised fit coefficients matrix.
#' @param general_fit_var_cov_mat Generalised variance-covariance matrix.
#' @param conf_int Curve confidence interval.
#'
#' @return Numeric value of corrected yield.
correct_yield <- function(yield, type = "estimate", general_fit_coeffs, general_fit_var_cov_mat, conf_int) {
  yield_inf <- calculate_yield_infimum(type, general_fit_coeffs, general_fit_var_cov_mat, conf_int)

  if (yield < yield_inf) {
    yield <- 0
  }
  yield <- correct_negative_vals(yield)

  return(yield)
}

#' Correct boundary of irradiated fractions to be bounded by 0 and 1
#'
#' @param x Numeric value.
#'
#' @return Numeric value in [0, 1] range.
correct_boundary <- function(x) {
  if (x > 1) {
    return(1)
  } else if (x < 0) {
    return(0)
  } else {
    return(x)
  }
}

# Curve function ----

#' Plot dose estimation curve
#'
#' @param est_doses List of dose estimations results from \code{estimate_*()} family of functions.
#' @param fit_coeffs Fitting coefficients matrix.
#' @param fit_var_cov_mat Fitting variance-covariance matrix.
#' @param protracted_g_value Protracted \eqn{G(x)} value.
#' @param conf_int_curve Confidence interval of the curve.
#' @param aberr_name Name of the aberration to use in the y-axis.
#' @param place UI or save.
#'
#' @return \code{ggplot2} object.
#' @example man/examples/plot_estimated_dose_curve_example.R
#' @export
plot_estimated_dose_curve <- function(est_doses, fit_coeffs, fit_var_cov_mat,
                                      protracted_g_value = 1, conf_int_curve,
                                      aberr_name, place) {

  # Validate est_doses names
  assessments <- names(est_doses) %>%
    match_names(c("whole", "partial", "hetero"))

  # Parse dose estimation list
  est_full_doses <- data.frame(
    dose = c(
      if ("whole" %in% assessments) t(est_doses$whole[[1]]$est_doses),
      if ("partial" %in% assessments) t(est_doses$partial[[1]]$est_doses),
      if ("hetero" %in% assessments) est_doses$hetero$est_doses[["dose1"]],
      if ("hetero" %in% assessments) est_doses$hetero$est_doses[["dose2"]]
    ),
    yield = c(
      if ("whole" %in% assessments) t(est_doses$whole[[1]]$est_yield),
      if ("partial" %in% assessments) t(est_doses$partial[[1]]$est_yield),
      if ("hetero" %in% assessments) est_doses$hetero$est_yields[["yield1"]],
      if ("hetero" %in% assessments) est_doses$hetero$est_yields[["yield2"]]
    ),
    type = c(
      if ("whole" %in% assessments) t(rep("Whole-body", 3)),
      if ("partial" %in% assessments) t(rep("Partial-body", 3)),
      if ("hetero" %in% assessments) rep("Heterogeneous 1", 3),
      if ("hetero" %in% assessments) rep("Heterogeneous 2", 3)
    ),
    level = c("Lower", "Estimate", "Upper")
  )


  # Parse confidence intervals
  conf_int_text_whole <- parse_conf_int_text(est_doses$whole[[1]]$conf_int)
  conf_int_text_partial <- parse_conf_int_text(est_doses$partial[[1]]$conf_intt)
  conf_int_text_hetero <- parse_conf_int_text(est_doses$hetero$conf_int)

  # Rightmost limit of the plot
  max_dose <- 1.05 * est_full_doses[["dose"]] %>%
    ifelse(is.na(.), 0, .) %>%
    max()

  # Generalised fit coefficients and variance-covariance matrix
  general_fit_coeffs <- generalise_fit_coeffs(fit_coeffs[, "estimate"])
  general_fit_var_cov_mat <- generalise_fit_var_cov_mat(fit_var_cov_mat)

  # Correct CIs
  # TODO: parse from est_doses
  conf_int_curve <- conf_int_curve %>%
    correct_conf_int(general_fit_var_cov_mat, protracted_g_value, type = "curve")

  # Plot data from curves
  curves_data <- data.frame(
    dose = seq(0, max_dose, length.out = 100)
  ) %>%
    dplyr::mutate(
      yield = calculate_yield(.data$dose, type = "estimate", general_fit_coeffs, NULL, protracted_g_value, 0),
      yield_low = calculate_yield(.data$dose, type = "lower", general_fit_coeffs, general_fit_var_cov_mat, protracted_g_value, conf_int_curve),
      yield_upp = calculate_yield(.data$dose, type = "upper", general_fit_coeffs, general_fit_var_cov_mat, protracted_g_value, conf_int_curve)
    )

  # Parse assessment legend
  color_breaks <- c("Whole-body", "Partial-body", "Heterogeneous 1", "Heterogeneous 2")
  color_labels <- c(
    paste("Whole-body", conf_int_text_whole),
    paste("Partial-body", conf_int_text_partial),
    paste("Heterogeneous 1", conf_int_text_hetero),
    paste("Heterogeneous 2", conf_int_text_hetero)
  )
  color_indices <- est_full_doses$type %>%
    unique() %>%
    paste(collapse = "|") %>%
    grep(color_breaks)

  # Make base plot
  gg_curve <- ggplot2::ggplot(curves_data) +
    # Fitted curve
    ggplot2::stat_function(
      data = data.frame(x = c(0, max_dose)),
      mapping = ggplot2::aes(x = .data$x),
      fun = function(x) yield_fun(x, general_fit_coeffs, protracted_g_value),
      linetype = "dashed"
    ) +
    # Confidence bands (Merkle, 1983)
    ggplot2::geom_ribbon(
      data = curves_data,
      mapping = ggplot2::aes(x = .data$dose, ymin = .data$yield_low, ymax = .data$yield_upp),
      alpha = 0.25
    ) +
    ggplot2::labs(x = "Dose (Gy)", y = paste0(aberr_name, "/cells")) +
    ggplot2::theme_bw()

  # Add doses to plot
  gg_curve <- gg_curve +
    # Estimated whole-body doses
    ggplot2::geom_point(
      data = est_full_doses,
      mapping = ggplot2::aes(x = .data$dose, y = .data$yield, color = .data$type, shape = .data$level),
      size = 2, na.rm = TRUE
    ) +
    # Assessment
    ggplot2::scale_color_manual(
      values = grDevices::hcl(
        h = seq(15, 375, length = 4 + 1),
        l = 65,
        c = 100
      ) %>%
        .[1:4] %>%
        `names<-`(c("Partial-body", "Heterogeneous 1", "Heterogeneous 2", "Whole-body")),
      breaks = color_breaks[color_indices],
      labels = color_labels[color_indices]
    ) +
    # Estimation level
    ggplot2::scale_shape_manual(
      values = c("Lower" = 15, "Estimate" = 16, "Upper" = 17),
      breaks = c("Lower", "Estimate", "Upper")
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(order = 1),
      shape = ggplot2::guide_legend(order = 2),
      fill = ggplot2::guide_legend(order = 3)
    ) +
    ggplot2::labs(color = "Assessment", shape = "Estimation") +
    # Tweak legend
    ggplot2::theme(
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 8),
      legend.spacing.y = ggplot2::unit(5, "points"),
      legend.key.height = ggplot2::unit(12, "points")
    )+
    if (place == "save") {
      list(
        ggplot2::labs(caption = "Created with Biodosetools"),
        ggplot2::theme(plot.caption = ggplot2::element_text(size = 8, colour = "black", hjust = 1))
      )
    } else {
      NULL
    }

  return(gg_curve)
}


#' Plot triage
#'
#' @param num_cases number of cases.
#' @param est_doses_whole dose estimation list of cases.
#' @param est_doses_partial dose estimation list of cases.
#' @param assessment partial or whole.
#' @param place UI or save.
#'
#' @import ggplot2
#' @return \code{ggplot2} object.
#' @example man/examples/plot_triage_example.R
#' @export


plot_triage <- function(num_cases, est_doses_whole, est_doses_partial, assessment, place) {

  case_labels <- as.character(est_doses_whole[, 1])
  case_index <- seq_along(case_labels)

  if (assessment == "whole") {
    data <- data.frame(
      x = case_index,
      case = case_labels,
      doses = est_doses_whole[, 3],
      lower = est_doses_whole[, 2],
      upper = est_doses_whole[, 4],
      type = "Whole"
    )
  }

  if (assessment == "partial") {
    data_whole <- data.frame(
      x = case_index - 0.15,
      case = case_labels,
      doses = est_doses_whole[, 3],
      lower = est_doses_whole[, 2],
      upper = est_doses_whole[, 4],
      type = "Whole"
    )

    data_partial <- data.frame(
      x = case_index + 0.15,
      case = case_labels,
      doses = est_doses_partial[, 3],
      lower = est_doses_partial[, 2],
      upper = est_doses_partial[, 4],
      type = "Partial"
    )

    data <- rbind(data_whole, data_partial)
  }

  background <- data.frame(
    ymin = c(0, 1, 2),
    ymax = c(1, 2, Inf),
    fill = c("low exposure", "medium exposure", "high exposure")
  )

  gg_curve <- ggplot(data) +
    geom_rect(data = background, aes(xmin = -Inf, xmax = Inf, ymin = .data$ymin, ymax = .data$ymax, fill = .data$fill), alpha = 0.2) +
    geom_point(aes(x = .data$x, y = .data$doses, color = .data$type), size = 2) +
    geom_errorbar(aes(x = .data$x, ymin = .data$lower, ymax = .data$upper, color = .data$type), width = 0.1) +
    scale_x_continuous(
      name = "Cases",
      breaks = case_index,
      labels = case_labels
    ) +
    scale_fill_manual(
      name = "Dose Ranges",
      values = c("low exposure" = "green", "medium exposure" = "orange", "high exposure" = "red")
    ) +
    scale_color_manual(
      name = "Assessment Type",
      values = c("Whole" = "blue", "Partial" = "red")
    ) +
    labs(
      y = "Dose (Gy)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 8)
    )

  if (place == "save") {
    gg_curve <- gg_curve +
      labs(caption = "Created with Biodosetools") +
      theme(plot.caption = element_text(size = 8, colour = "black", hjust = 1))
  }

  return(gg_curve)
}

