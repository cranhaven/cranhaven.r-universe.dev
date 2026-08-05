# Dicentrics dose estimation ----

test_that("processing case data works", {
  case_data <- app_sys("extdata", "cases-data-mixed.csv") %>%
    utils::read.csv(header = TRUE)

  case_data <- calculate_aberr_table(
    data = case_data,
    type = "case",
    assessment_u = 1,
    aberr_module = "dicentrics"
  )

  # Colnames validation
  case_data_cols <- colnames(case_data)
  case_data_cols_len <- length(case_data_cols)

  # Expected outcomes
  expect_equal(case_data_cols[1:3], c("ID", "N", "X"))

  expect_true(all(grepl("C", case_data_cols[seq(4, case_data_cols_len - 4, 1)])))
  expect_equal(case_data_cols[seq(case_data_cols_len - 3, case_data_cols_len, 1)], c("y", "y_err", "DI", "u"))

  # Dose estimation
  aberr_module <- "dicentrics"

  fit_results_list_gamma <- app_sys("extdata", "gamma_dicentrics-fitting-results.rds") %>%
    readRDS()

  # Parse fitting data
  fit_coeffs_gamma <- fit_results_list_gamma[["fit_coeffs"]]
  fit_var_cov_mat_gamma <- fit_results_list_gamma[["fit_var_cov_mat"]]
  fit_formula_tex_gamma <- fit_results_list_gamma[["fit_formula_tex"]]

  fit_results_list_neutron <- app_sys("extdata", "neutrons-mixed-dicentrics-fitting-results.rds") %>%
    readRDS()

  # Parse fitting data
  fit_coeffs_neutron <- fit_results_list_neutron[["fit_coeffs"]]
  fit_var_cov_mat_neutron <- fit_results_list_neutron[["fit_var_cov_mat"]]
  fit_formula_tex_neutron <- fit_results_list_neutron[["fit_formula_tex"]]

  p <- 0
  ratio <- 1.78
  num_cases <- nrow(case_data)
  case_data <- as.data.frame(case_data)

  expect_equal(case_data$ID, c("example1", "example2"))

  cells <- list()
  dics <- list()
  for( i in 1:num_cases){
    cells[[i]] <- case_data[i,"N"]
    dics[[i]] <- case_data[i,"X"]
  }

  # Calculations
  est_mixed <- fun.estimate.criticality(num_cases, dics, cells,  fit_coeffs_gamma[, 1], fit_var_cov_mat_gamma,
                                                fit_coeffs_neutron[, 1], fit_var_cov_mat_neutron, ratio, p)




  # Expected outputs (mixed)

  expect_equal(names(est_mixed[[1]]$gamma), c("est", "lwr", "upr"))
  expect_equal(names(est_mixed[[1]]$neutron), c("est", "lwr", "upr"))
  expect_equal(names(est_mixed[[1]]$total), c("est", "lwr", "upr"))

  expect_equal(names(est_mixed[[2]]$gamma), c("est", "lwr", "upr"))
  expect_equal(names(est_mixed[[2]]$neutron), c("est", "lwr", "upr"))
  expect_equal(names(est_mixed[[2]]$total), c("est", "lwr", "upr"))


  expect_true(all(round(est_mixed[[1]]$gamma, 3) == c(2.787, 2.250, 3.324)))
  expect_true(all(round(est_mixed[[1]]$neutron, 3) == c(1.566, 1.264, 1.867)))
  expect_true(all(round(est_mixed[[1]]$total, 3) == c(4.352, 3.514, 5.191)))


  expect_true(all(round(est_mixed[[2]]$gamma, 3) == c(3.715, 3.045, 4.386)))
  expect_true(all(round(est_mixed[[2]]$neutron, 3) == c(2.087, 1.711, 2.464)))
  expect_true(all(round(est_mixed[[2]]$total, 3) == c(5.803, 4.756, 6.849)))

  # Plot
  gg_curve <- plot_estimated_dose_curve_mx(
    name = "Example1",
    est_doses = est_mixed[[1]],
    fit_coeffs = fit_coeffs_gamma[, 1],
    fit_var_cov_mat = fit_var_cov_mat_gamma,
    curve_type = "gamma",
    protracted_g_value = 1,
    conf_int_curve = 0.95,
    place = "UI")


  # # Expected outcomes
  expect_equal(names(gg_curve$labels), c("x", "y", "title"))
  expect_equal(unname(unlist(gg_curve$labels)), c("Dose (Gy)", "dics/cells", "Curve plot Case number:  Example1"))


  gg_curve <- plot_estimated_dose_curve_mx(
    name = "Example1",
    est_doses = est_mixed[[1]],
    fit_coeffs = fit_coeffs_neutron[, 1],
    fit_var_cov_mat = fit_var_cov_mat_neutron,
    curve_type = "neutron",
    protracted_g_value = 1,
    conf_int_curve = 0.95,
    place = "UI")

  # # Expected outcomes
  expect_equal(names(gg_curve$labels), c("x", "y", "title"))
  expect_equal(unname(unlist(gg_curve$labels)), c("Dose (Gy)", "dics/cells", "Curve plot Case number:  Example1"))


  # Plot
  gg_curve <- plot_estimated_dose_curve_mx(
    name = "Example2",
    est_doses = est_mixed[[2]],
    fit_coeffs = fit_coeffs_gamma[, 1],
    fit_var_cov_mat = fit_var_cov_mat_gamma,
    curve_type = "gamma",
    protracted_g_value = 1,
    conf_int_curve = 0.95,
    place = "UI")


  # # Expected outcomes
  expect_equal(names(gg_curve$labels), c("x", "y", "title"))
  expect_equal(unname(unlist(gg_curve$labels)), c("Dose (Gy)", "dics/cells", "Curve plot Case number:  Example2"))


  gg_curve <- plot_estimated_dose_curve_mx(
    name = "Example2",
    est_doses = est_mixed[[2]],
    fit_coeffs = fit_coeffs_neutron[, 1],
    fit_var_cov_mat = fit_var_cov_mat_neutron,
    curve_type = "neutron",
    protracted_g_value = 1,
    conf_int_curve = 0.95,
    place = "UI")

  # # Expected outcomes
  expect_equal(names(gg_curve$labels), c("x", "y", "title"))
  expect_equal(unname(unlist(gg_curve$labels)), c("Dose (Gy)", "dics/cells", "Curve plot Case number:  Example2"))

})
