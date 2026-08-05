#' Calculate the characteristic limits table
#'
#' @param input all input UI parameters
#' @param project_yield function for calculating yield, in utils_estimation.R
#' @param fit_coeffs curve coefficients
#'
#' @return list with all the table columns.
calculate_table <- function(input, project_yield, fit_coeffs) {

#Number of dics and cells-------------------------------------------------------
if (input$input_control == "full") {
  if (input$provide_cells_proband_check) {
    cells_proband <- c(10, input$num_cells_proband_full)
  } else {
    cells_proband <- c(10, 20, 50, 100, 200, 500, 1000)
  }

  formula_select <- input$formula_select
  control_data <- c(aberr = input$num_aberrs_control_full, cells = input$num_cells_control_full)
  proband_data <- c(aberr = NULL, cells = input$num_cells_proband_full)
  alpha <- input$typeI_error
  beta <- input$typeII_error


  tmp <- sapply(cells_proband, function(x) calculate_characteristic_limits(
    y0 = control_data["aberr"],
    n0 = control_data["cells"],
    n1 = x,
    type = "var",
    alpha = alpha,
    beta = beta
  ))

  if (input$no_curve || is.null(fit_coeffs)) {
    est_dec <- rep(NA, length(cells_proband))
    est_det <- rep(NA, length(cells_proband))
  } else {
    tmp_coeffs <- fit_coeffs[, "estimate"]
    if (length(tmp_coeffs) == 2) {
      tmp_coeffs <- c(tmp_coeffs, 0)
    }

    est_dec <- sapply((unlist(tmp["decision_threshold", ]) + 1) / cells_proband, function(x)
      project_yield(yield = x,
                    type = "estimate",
                    general_fit_coeffs = tmp_coeffs,
                    general_fit_var_cov_mat = NULL,
                    protracted_g_value = 1,
                    conf_int = 0))
    est_det <- sapply(unlist(tmp["detection_limit", ]) / cells_proband, function(x)
      project_yield(yield = x,
                    type = "estimate",
                    general_fit_coeffs = tmp_coeffs,
                    general_fit_var_cov_mat = NULL,
                    protracted_g_value = 1,
                    conf_int = 0))
  }

#Dics per cell------------------------------------------------------------------
} else if (input$input_control == "mu") {
  if (input$provide_cells_proband_check_mu) {
    cells_proband <- c(10, input$num_cells_proband_mu)
  } else {
    cells_proband <- c(10, 20, 50, 100, 200, 500, 1000)
  }

  formula_select <- input$formula_select
  mu_control <- input$mean_control
  alpha <- input$typeI_error
  beta <- input$typeII_error

  tmp <- sapply(cells_proband, function(x) calculate_characteristic_limits(
    mu0 = mu_control,
    n1 = x,
    type = "const",
    alpha = alpha,
    beta = beta
  ))

  if (input$no_curve || is.null(fit_coeffs)) {
    est_dec <- rep(NA, length(cells_proband))
    est_det <- rep(NA, length(cells_proband))
  } else {
    tmp_coeffs <- fit_coeffs[, "estimate"]
    if (length(tmp_coeffs) == 2) {
      tmp_coeffs <- c(tmp_coeffs, 0)
    }

    est_dec <- sapply((unlist(tmp["decision_threshold", ]) + 1) / cells_proband, function(x)
      project_yield(yield = x,
                    type = "estimate",
                    general_fit_coeffs = tmp_coeffs,
                    general_fit_var_cov_mat = NULL,
                    protracted_g_value = 1,
                    conf_int = 0))
    est_det <- sapply(unlist(tmp["detection_limit", ]) / cells_proband, function(x)
      project_yield(yield = x,
                    type = "estimate",
                    general_fit_coeffs = tmp_coeffs,
                    general_fit_var_cov_mat = NULL,
                    protracted_g_value = 1,
                    conf_int = 0))
  }
}

results_list <- list(
  dec_thresh = t(rbind(cells_proband, tmp, est_dec, est_det)),
  p_value = matrix(c(1, 1, 1), nrow = 1, ncol = 3)
)  }
