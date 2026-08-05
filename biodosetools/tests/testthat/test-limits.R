# Proband vs control ----

test_that("proband vs control works", {
  control_data <- c(aberr = 1, cells = 1000)

  proband_data <- c(aberr = 13, cells = 1000)


  result_data <- matrix(c(control_data["aberr"]/control_data["cells"],
                          proband_data["aberr"]/proband_data["cells"], stats::poisson.test(c(proband_data["aberr"],
                                                                                             control_data["aberr"]), c(proband_data["cells"], control_data["cells"]))$p.value),
                        ncol = 3, nrow = 1)

  colnames(result_data) <- c("dics/cell (control)", "dics/cell (case)",
                             "P-value")


  expect_true(all(round(result_data, 3) == c(0.001, 0.013, 0.002)))

})

# Characteristic limits ----

test_that("characteristic limits works", {
  cells_proband <- c(20, 50, 100, 200, 500, 1000)
  control_data <- c(aberr = 4,
                    cells = 1000)

  c_limits <- sapply(cells_proband, function(x) calculate_characteristic_limits(
    y0 = control_data["aberr"],
    n0 = control_data["cells"],
    n1 = x,
    alpha = 0.05,
    beta = 0.1,
    ymax = 100,
    type = "var"
  ))

  expect_equal(rownames(c_limits), c("decision_threshold", "detection_limit"))
  expect_true(all(c_limits["decision_threshold", ] == c(1, 1, 2, 3, 7, 12)))
  expect_true(all(round(unlist(c_limits["detection_limit", ]), 3) == c(3.890, 3.890, 5.322,  6.681, 11.771, 17.782)))

  # Limits with curve data

  fit_results_list <- app_sys("extdata", "dicentrics-fitting-results.rds") %>%
    readRDS()
  fit_coeffs <- fit_results_list$fit_coeffs[, "estimate"]

  est_dec <- sapply((unlist(c_limits["decision_threshold", ]) + 1) / cells_proband, function(x) project_yield(
    yield = x,
    type = "estimate",
    general_fit_coeffs = fit_coeffs,
    general_fit_var_cov_mat = NULL,
    protracted_g_value = 1,
    conf_int = 0))

  expect_true(all(round(est_dec, 3) == c(1.096, 0.634, 0.528, 0.403, 0.344, 0.295)))

  est_det <- sapply(unlist(c_limits["detection_limit", ]) / cells_proband, function(x) project_yield(
    yield = x,
    type = "estimate",
    general_fit_coeffs = fit_coeffs,
    general_fit_var_cov_mat = NULL,
    protracted_g_value = 1,
    conf_int = 0))
  expect_true(all(round(est_det, 3) == c(1.592, 0.947, 0.756, 0.566, 0.450, 0.371)))


})
