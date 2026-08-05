#' Plot fit dose curve
#'
#' @param fit_results_list List of fit results.
#' @param aberr_name Name of the aberration to use in the y-axis.
#' @param place Where the plot will be displayed.
#'
#' @return \code{ggplot2} object.
#' @example man/examples/plot_fit_dose_curve_example.R
#' @export
plot_fit_dose_curve <- function(fit_results_list, aberr_name, place) {
  # Read objects from fit results list
  count_data <- as.data.frame(fit_results_list[["fit_raw_data"]])
  fit_coeffs <- fit_results_list[["fit_coeffs"]]
  fit_var_cov_mat <- fit_results_list[["fit_var_cov_mat"]]

  # Generalized fit coefficients
  general_fit_coeffs <- generalise_fit_coeffs(fit_coeffs[, "estimate"])

  # Generalized variance-covariance matrix
  general_fit_var_cov_mat <- generalise_fit_var_cov_mat(fit_var_cov_mat)

  # Generalized curves
  yield_fun <- function(d) {
    general_fit_coeffs[["coeff_C"]] +
      general_fit_coeffs[["coeff_alpha"]] * d +
      general_fit_coeffs[["coeff_beta"]] * d^2
  }

  chisq_df <- nrow(fit_coeffs)
  R_factor <- sqrt(stats::qchisq(0.95, df = chisq_df))

  yield_error_fun <- function(d) {
    sqrt(
      general_fit_var_cov_mat[["coeff_C", "coeff_C"]] +
        general_fit_var_cov_mat[["coeff_alpha", "coeff_alpha"]] * d^2 +
        general_fit_var_cov_mat[["coeff_beta", "coeff_beta"]] * d^4 +
        2 * general_fit_var_cov_mat[["coeff_C", "coeff_alpha"]] * d +
        2 * general_fit_var_cov_mat[["coeff_C", "coeff_beta"]] * d^2 +
        2 * general_fit_var_cov_mat[["coeff_alpha", "coeff_beta"]] * d^3
    )
  }

  # Plot data
  plot_data <- count_data %>%
    dplyr::mutate(
      yield = .data$X / .data$N,
      dose = .data$D
    ) %>%
    dplyr::select("dose", "yield")

  curves_data <- data.frame(dose = seq(0, max(plot_data[["dose"]]), length.out = 100)) %>%
    dplyr::mutate(
      yield = yield_fun(.data$dose),
      yield_low = yield_fun(.data$dose) - R_factor * yield_error_fun(.data$dose),
      yield_upp = yield_fun(.data$dose) + R_factor * yield_error_fun(.data$dose)
    )

  # Make plot
  gg_curve <- ggplot2::ggplot(plot_data) +
    # Observed data
    ggplot2::geom_point(
      mapping = ggplot2::aes(x = .data$dose, y = .data$yield)
    ) +
    # Fitted curve
    ggplot2::stat_function(
      data = data.frame(x = c(0, max(plot_data[["dose"]]))),
      mapping = ggplot2::aes(.data$x),
      fun = function(x) yield_fun(x),
      linetype = "dashed"
    ) +
    # Confidence bands (Merkle, 1983)
    ggplot2::geom_ribbon(
      data = curves_data,
      ggplot2::aes(x = .data$dose, ymin = .data$yield_low, ymax = .data$yield_upp),
      alpha = 0.25
    ) +
    ggplot2::labs(
      x = "Dose (Gy)",
      y = paste0(aberr_name, "/cells")
    ) +
    ggplot2::theme_bw() +
    if (place == "save") {
      list(
        ggplot2::labs(caption = "Created with Biodosetools"),
        ggplot2::theme(plot.caption = ggplot2::element_text(size = 8, colour = "black", hjust = 1))
      )
    } else {
      NULL
    }

  # Return object
  return(gg_curve)
}
