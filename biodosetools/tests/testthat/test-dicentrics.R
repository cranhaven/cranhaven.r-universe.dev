# Dicentrics fitting ----

test_that("fit with full count data works", {
  # Example from IAEA (2011)
  dic_count_data <- app_sys("extdata", "count-data-barquinero-1995.csv") %>%
    utils::read.csv() %>%
    calculate_aberr_table(type = "count")

  # Expected outputs
  expect_equal(dic_count_data$N, c(5000L, 5002L, 2008L, 2002L, 1832L, 1168L, 562L, 333L, 193L, 103L, 59L))
  expect_equal(dic_count_data$X, c(8L, 14L, 22L, 55L, 100L, 109L, 100L, 103L, 108L, 103L, 107L))
  expect_equal(which(unname(dic_count_data$u) > 1.96), c(3))

  # Fitting (glm)
  model_formula <- list_fitting_formulas()[[1]]
  model_family <- "automatic"
  aberr_module <- "dicentrics"

  fit_results_list <- fit(
    count_data = dic_count_data,
    model_formula,
    model_family,
    fit_link = "identity",
    aberr_module
  )

  gg_curve <- plot_fit_dose_curve(
    fit_results_list,
    aberr_name = to_title(aberr_module)
  )

  # Expected outputs
  expect_gt(ncol(dic_count_data), 3)
  expect_equal(fit_results_list$fit_raw_data, as.matrix(dic_count_data))
  expect_equal(fit_results_list$fit_algorithm, "glm")

  expect_true(all(dim(fit_results_list$fit_cor_mat) == c(3, 3)))
  expect_true(all(dim(fit_results_list$fit_coeffs) == c(3, 4)))
  expect_true(all(round(unname(fit_results_list$fit_coeffs[, "estimate"]), 4) == c(0.0013, 0.0210, 0.0630)))
  expect_true(all(round(unname(fit_results_list$fit_coeffs[, "std.error"]), 4) == c(0.0005, 0.0052, 0.0040)))
  expect_true(all(round(unname(diag(fit_results_list$fit_var_cov_mat)), 8) == c(2.2e-07, 2.66e-05, 1.606e-05)))

  expect_true(is.null(fit_results_list$fit_dispersion) & grepl("A Poisson", fit_results_list$fit_model_summary))
  expect_equal(round(unname(fit_results_list$fit_model_statistics[, "logLik"]), 2), -4.66)
  expect_equal(gg_curve$data$dose, dic_count_data$D)

  # Fitting (linear model)
  fit_results_list <- fit(
    count_data = dplyr::filter(dic_count_data, D < 1),
    model_formula = list_fitting_formulas()[[2]],
    model_family = "quasipoisson",
    fit_link = "identity",
    aberr_module
  )

  # Expected results
  expect_equal(fit_results_list$fit_algorithm, "glm")

  expect_true(all(dim(fit_results_list$fit_cor_mat) == c(2, 2)))
  expect_true(all(dim(fit_results_list$fit_coeffs) == c(2, 4)))
  expect_true(all(round(unname(fit_results_list$fit_coeffs[, "estimate"]), 4) == c(0.0010, 0.0541)))
  expect_true(all(round(unname(fit_results_list$fit_coeffs[, "std.error"]), 4) == c(0.0011, 0.0115)))
  expect_true(all(round(unname(diag(fit_results_list$fit_var_cov_mat)), 8) == c(1.32e-06, 1.3171e-04)))

  expect_true(fit_results_list$fit_dispersion > 1 & grepl("A quasi\\-Poisson", fit_results_list$fit_model_summary))

  # Fitting (maxlik)
  model_formula <- list_fitting_formulas()[[2]]

  # Expected glm warning in tryCatch()
  expect_warning(
    fit(
      count_data = dic_count_data,
      model_formula,
      model_family,
      fit_link = "identity",
      aberr_module
    )
  )
})

test_that("fit with aggregated count data works", {
  # Example from IAEA (2011)
  dic_count_data <- app_sys("extdata", "count-data-barquinero-1995-aggr.csv") %>%
    utils::read.csv() %>%
    dplyr::mutate(
      D = as.numeric(.data$D)
    )

  # Expected outputs
  expect_equal(ncol(dic_count_data), 3)
  expect_equal(dic_count_data$N, c(5000L, 5002L, 2008L, 2002L, 1832L, 1168L, 562L, 333L, 193L, 103L, 59L))
  expect_equal(dic_count_data$X, c(8L, 14L, 22L, 55L, 100L, 109L, 100L, 103L, 108L, 103L, 107L))

  # Fitting (glm)
  model_formula <- list_fitting_formulas()[[1]]
  model_family <- "automatic"
  aberr_module <- "dicentrics"

  fit_results_list <- fit(
    count_data = dic_count_data,
    model_formula,
    model_family,
    fit_link = "identity",
    aberr_module
  )

  gg_curve <- plot_fit_dose_curve(
    fit_results_list,
    aberr_name = to_title(aberr_module)
  )

  # Expected outputs
  expect_equal(fit_results_list$fit_raw_data, as.matrix(dic_count_data))
  expect_equal(fit_results_list$fit_algorithm, "glm")

  expect_true(all(dim(fit_results_list$fit_cor_mat) == c(3, 3)))
  expect_true(all(dim(fit_results_list$fit_coeffs) == c(3, 4)))
  expect_true(all(round(unname(fit_results_list$fit_coeffs[, "estimate"]), 4) == c(0.0013, 0.0210, 0.0630)))
  expect_true(all(round(unname(fit_results_list$fit_coeffs[, "std.error"]), 4) == c(0.0005, 0.0052, 0.0040)))
  expect_true(all(round(unname(diag(fit_results_list$fit_var_cov_mat)), 8) == c(2.2e-07, 2.66e-05, 1.606e-05)))

  expect_equal(round(unname(fit_results_list$fit_model_statistics[, "logLik"]), 2), -4.66)
  expect_equal(gg_curve$data$dose, dic_count_data$D)

  # Fitting (maxlik)
  model_formula <- list_fitting_formulas()[[2]]

  # Expected glm warning in tryCatch()
  expect_warning(
    fit(
      count_data = dic_count_data,
      model_formula,
      model_family,
      fit_link = "identity",
      aberr_module
    )
  )
})


# Dicentrics dose estimation ----

test_that("processing case data works", {
  case_data <- app_sys("extdata", "cases-data-partial.csv") %>%
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
  expect_equal(case_data_cols[1:2], c("N", "X"))
  expect_true(all(grepl("C", case_data_cols[seq(3, case_data_cols_len - 4, 1)])))
  expect_equal(case_data_cols[seq(case_data_cols_len - 3, case_data_cols_len, 1)], c("y", "y_err", "DI", "u"))

  # Dose estimation
  aberr_module <- "dicentrics"

  fit_results_list <- app_sys("extdata", "dicentrics-fitting-results.rds") %>%
    readRDS()

  # Parse fitting data
  fit_coeffs <- fit_results_list[["fit_coeffs"]]
  fit_var_cov_mat <- fit_results_list[["fit_var_cov_mat"]]
  fit_formula_tex <- fit_results_list[["fit_formula_tex"]]

  # Protraction (acute exposure)
  protracted_g_value <- 1

  # Parse genome fraction
  parsed_genome_factor <- 1

  # Calculations
  results_whole_merkle <- estimate_whole_body_merkle(
    case_data,
    fit_coeffs,
    fit_var_cov_mat,
    conf_int_yield = 0.83,
    conf_int_curve = 0.83,
    protracted_g_value,
    parsed_genome_factor,
    aberr_module
  )

  results_whole_delta <- estimate_whole_body_delta(
    case_data,
    fit_coeffs,
    fit_var_cov_mat,
    conf_int = 0.95,
    protracted_g_value,
    aberr_module
  )

  results_partial <- estimate_partial_body_dolphin(
    case_data,
    fit_coeffs,
    fit_var_cov_mat,
    conf_int = 0.95,
    protracted_g_value,
    genome_factor = parsed_genome_factor,
    gamma = 1 / 2.7,
    aberr_module
  )

  set.seed(1)
  results_hetero <- estimate_hetero_mixed_poisson(
    case_data,
    fit_coeffs,
    fit_var_cov_mat,
    conf_int = 0.95,
    protracted_g_value,
    gamma = 1 / 2.7,
    gamma_error = 0
  )

  # Expected outputs (whole-body)
  expect_equal(colnames(results_whole_merkle$est_doses), c("yield", "dose"))
  expect_equal(rownames(results_whole_merkle$est_doses), c("lower", "estimate", "upper"))
  expect_equal(round(results_whole_merkle$AIC, 3), 7.057)

  expect_equal(colnames(results_whole_delta$est_doses), c("yield", "dose"))
  expect_equal(rownames(results_whole_delta$est_doses), c("lower", "estimate", "upper"))
  expect_equal(round(results_whole_delta$AIC, 3), 7.057)

  expect_equal(results_whole_merkle$est_doses["estimate", "yield"], results_whole_delta$est_doses["estimate", "yield"])
  expect_gt(results_whole_merkle$est_doses["lower", "yield"], results_whole_delta$est_doses["lower", "yield"])
  expect_lt(results_whole_merkle$est_doses["upper", "yield"], results_whole_delta$est_doses["upper", "yield"])

  expect_true(all(round(results_whole_merkle$est_doses$yield, 3) == c(0.240, 0.277, 0.319)))
  expect_true(all(round(results_whole_merkle$est_doses$dose, 3) == c(1.712, 1.931, 2.187)))
  expect_true(all(round(results_whole_delta$est_doses$yield, 3) == c(0.205, 0.277, 0.349)))
  expect_true(all(round(results_whole_delta$est_doses$dose, 3) == c(1.648, 1.931, 2.214)))

  # Expected outputs (partial-body)
  expect_equal(colnames(results_partial$est_doses), c("yield", "dose"))
  expect_equal(rownames(results_partial$est_doses), c("lower", "estimate", "upper"))
  expect_equal(round(results_partial$AIC, 3), 8.133)

  expect_true(all(round(results_partial$est_doses$yield, 3) == c(0.835, 1.168, 1.500)))
  expect_true(all(round(results_partial$est_doses$dose, 3) == c(3.493, 4.138, 4.783)))

  # Expected outputs (heterogeneous)
  expect_equal(colnames(results_hetero$est_yields), c("yield1", "yield2"))
  expect_equal(rownames(results_hetero$est_yields), c("lower", "estimate", "upper"))
  expect_equal(colnames(results_hetero$est_doses), c("dose1", "dose2"))
  expect_equal(rownames(results_hetero$est_doses), c("lower", "estimate", "upper"))
  expect_equal(round(results_hetero$AIC, 3), 8.264)

  expect_true(all(round(results_hetero$est_yields$yield1, 3) == c(0.714, 1.210, 1.705)))
  expect_true(all(round(results_hetero$est_yields$yield2, 3) == c(0, 0.010, 0.092)))
  expect_true(all(round(results_hetero$est_doses$dose1, 3) == c(3.295, 4.215, 5.134)))
  expect_true(all(round(results_hetero$est_doses$dose2, 3) == c(0, 0.241, 1.835)))
  expect_true(all(round(results_hetero$est_frac$estimate, 3) == c(0.578, 0.422)))
  expect_true(all(round(results_hetero$est_frac$std_err, 3) == c(0.127, 0.127)))

  # Plot
  gg_curve <- plot_estimated_dose_curve(
    est_doses = list(
      whole = results_whole_merkle,
      hetero = results_hetero
    ),
    fit_coeffs,
    fit_var_cov_mat,
    protracted_g_value,
    conf_int_curve = 0.83,
    aberr_name = to_title(aberr_module)
  )

  # Expected outcomes
  expect_equal(names(gg_curve$labels), c("colour", "shape", "x", "y", "ymin", "ymax"))
  expect_equal(unname(unlist(gg_curve$labels)), c("Assessment", "Estimation", "Dose (Gy)", "Dicentrics/cells", "yield_low", "yield_upp"))
})
