# Translocation utils ----

test_that("calculate_trans_rate_sigurdson works", {
  # Default parameters for confounders
  default_rate <- calculate_trans_rate_sigurdson(
    cells = 100,
    genome_factor = 0.3,
    age_value = 25,
    sex_bool = FALSE,
    sex_value = "none",
    smoker_bool = FALSE,
    ethnicity_value = "none",
    region_value = "none"
  )

  # Adding confounders will result in a higher rate
  other_rate <- calculate_trans_rate_sigurdson(
    cells = 100,
    genome_factor = 0.3,
    age_value = 25,
    sex_bool = TRUE,
    sex_value = "male",
    smoker_bool = TRUE,
    ethnicity_value = "white",
    region_value = "w-europe"
  )

  # Expected output
  expect_lt(default_rate, other_rate)
})

test_that("calculate_trans_rate_manual works", {
  # Translocation frequency per cell
  rate <- calculate_trans_rate_manual(
    cells = 100,
    genome_factor = 0.3,
    expected_aberr_value = 0.00339
  )

  # Expected output
  expect_equal(round(rate, 3), 0.102)
})

test_that("calculate_genome_factor works", {
  # Example from IAEA (2011)
  genome_factor <- calculate_genome_factor(
    dna_table = dna_content_fractions_morton,
    chromosomes = c(1, 2, 4, 3, 5, 6),
    colors = c(rep("Red", 3), rep("Green", 3)),
    sex = "male"
  )

  # Expected output
  expect_equal(round(genome_factor, 3), 0.585)
})


# Translocations fitting ----

test_that("fit with full count data works", {
  # Example from IAEA (2011)
  trans_count_data <- app_sys("extdata", "count-data-rodriguez-2004.csv") %>%
    utils::read.csv() %>%
    calculate_aberr_table(type = "count")

  # Expected outputs
  expect_equal(trans_count_data$N, c(4356L, 3324L, 3069L, 3072L, 2111L, 2124L, 1043L, 718L, 781L, 397L, 395L))
  expect_equal(trans_count_data$X, c(6L, 15L, 18L, 33L, 40L, 50L, 56L, 71L, 157L, 147L, 180L))
  expect_equal(which(unname(trans_count_data$u) > 1.96), c(9))

  # Fitting (glm)
  model_formula <- list_fitting_formulas()[[1]]
  model_family <- "automatic"
  aberr_module <- "translocations"

  fit_results_list <- fit(
    count_data = trans_count_data,
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
  expect_gt(ncol(trans_count_data), 3)
  expect_equal(fit_results_list$fit_raw_data, as.matrix(trans_count_data))
  expect_equal(fit_results_list$fit_algorithm, "glm")

  expect_true(all(dim(fit_results_list$fit_cor_mat) == c(3, 3)))
  expect_true(all(dim(fit_results_list$fit_coeffs) == c(3, 4)))
  expect_true(all(round(unname(fit_results_list$fit_coeffs[, "estimate"]), 4) == c(0.0021, 0.0086, 0.0183)))
  expect_true(all(round(unname(fit_results_list$fit_coeffs[, "std.error"]), 4) == c(0.0006, 0.0031, 0.0015)))
  expect_true(all(round(unname(diag(fit_results_list$fit_var_cov_mat)), 8) == c(4.2e-07, 9.75e-06, 2.12e-06)))

  expect_equal(round(unname(fit_results_list$fit_model_statistics[, "logLik"]), 2), -2.52)
  expect_equal(gg_curve$data$dose, trans_count_data$D)

  # Fitting (maxlik)
  model_formula <- list_fitting_formulas()[[2]]

  # Expected glm warning in tryCatch()
  expect_warning(
    fit(
      count_data = trans_count_data,
      model_formula,
      model_family,
      fit_link = "identity",
      aberr_module
    )
  )
})

test_that("fit with aggregated count data works", {
  # Example from IAEA (2011)
  trans_count_data <- app_sys("extdata", "count-data-rodriguez-2004-aggr.csv") %>%
    utils::read.csv() %>%
    dplyr::mutate(
      D = as.numeric(.data$D)
    )

  # Expected outputs
  expect_equal(ncol(trans_count_data), 3)
  expect_equal(trans_count_data$N, c(4356L, 3324L, 3069L, 3072L, 2111L, 2124L, 1043L, 718L, 781L, 397L, 395L))
  expect_equal(trans_count_data$X, c(6L, 15L, 18L, 33L, 40L, 50L, 56L, 71L, 157L, 147L, 180L))

  # Fitting (glm)
  model_formula <- list_fitting_formulas()[[1]]
  model_family <- "automatic"
  aberr_module <- "translocations"

  fit_results_list <- fit(
    count_data = trans_count_data,
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
  expect_equal(fit_results_list$fit_raw_data, as.matrix(trans_count_data))
  expect_equal(fit_results_list$fit_algorithm, "glm")

  expect_true(all(dim(fit_results_list$fit_cor_mat) == c(3, 3)))
  expect_true(all(dim(fit_results_list$fit_coeffs) == c(3, 4)))
  expect_true(all(round(unname(fit_results_list$fit_coeffs[, "estimate"]), 4) == c(0.0021, 0.0086, 0.0183)))
  expect_true(all(round(unname(fit_results_list$fit_coeffs[, "std.error"]), 4) == c(0.0006, 0.0031, 0.0015)))
  expect_true(all(round(unname(diag(fit_results_list$fit_var_cov_mat)), 8) == c(4.2e-07, 9.75e-06, 2.12e-06)))

  expect_equal(round(unname(fit_results_list$fit_model_statistics[, "logLik"]), 2), -2.52)
  expect_equal(gg_curve$data$dose, trans_count_data$D)

  # Fitting (maxlik)
  model_formula <- list_fitting_formulas()[[2]]

  # Expected glm warning in tryCatch()
  expect_warning(
    fit(
      count_data = trans_count_data,
      model_formula,
      model_family,
      fit_link = "identity",
      aberr_module
    )
  )
})


# Translocations dose estimation ----

test_that("processing case data works", {
  case_data <- app_sys("extdata", "cases-data-hetero.csv") %>%
    utils::read.csv(header = TRUE)

  case_data <- calculate_aberr_table(
    data = case_data,
    type = "case",
    assessment_u = 1,
    aberr_module = "translocations"
  )

  # Specific to translocations
  genome_factor <- 0.585

  case_data <- case_data %>%
    dplyr::mutate(
      Xc = dplyr::case_when(
        # "sigurdson" ~ calculate_trans_rate_sigurdson(...),
        # "manual" ~ calculate_trans_rate_manual(...),
        TRUE ~ 0
      ),
      Fg = (.data$X - .data$Xc) / (.data$N * genome_factor),
      Fg_err = .data$Fp_err / sqrt(genome_factor)
    )

  # Colnames validation
  case_data_cols <- colnames(case_data)
  case_data_cols_len <- length(case_data_cols)

  # Expected outcomes
  expect_equal(case_data_cols[1:2], c("N", "X"))
  expect_true(all(grepl("C", case_data_cols[seq(3, case_data_cols_len - 7, 1)])))
  expect_equal(case_data_cols[seq(case_data_cols_len - 6, case_data_cols_len, 1)], c("Fp", "Fp_err", "DI", "u", "Xc", "Fg", "Fg_err"))

  # Dose estimation
  aberr_module <- "translocations"

  fit_results_list <- app_sys("extdata", "translocations-fitting-results.rds") %>%
    readRDS()

  # Parse fitting data
  fit_coeffs <- fit_results_list[["fit_coeffs"]]
  fit_var_cov_mat <- fit_results_list[["fit_var_cov_mat"]]
  fit_formula_tex <- fit_results_list[["fit_formula_tex"]]

  # Protraction (acute exposure)
  protracted_g_value <- 1

  # Parse genome fraction
  parsed_genome_factor <- 0.585

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

  # Expected outputs (whole-body)
  expect_equal(colnames(results_whole_merkle$est_doses), c("yield", "dose"))
  expect_equal(rownames(results_whole_merkle$est_doses), c("lower", "estimate", "upper"))
  expect_equal(round(results_whole_merkle$AIC, 3), 8.223)

  expect_equal(colnames(results_whole_delta$est_doses), c("yield", "dose"))
  expect_equal(rownames(results_whole_delta$est_doses), c("lower", "estimate", "upper"))
  expect_equal(round(results_whole_delta$AIC, 3), 8.223)

  expect_equal(results_whole_merkle$est_doses["estimate", "yield"], results_whole_delta$est_doses["estimate", "yield"])
  expect_gt(results_whole_merkle$est_doses["lower", "yield"], results_whole_delta$est_doses["lower", "yield"])
  expect_lt(results_whole_merkle$est_doses["upper", "yield"], results_whole_delta$est_doses["upper", "yield"])

  expect_true(all(round(results_whole_merkle$est_doses$yield, 3) == c(1.171, 1.295, 1.429)))
  expect_true(all(round(results_whole_merkle$est_doses$dose, 3) == c(4.027, 4.485, 5.048)))
  expect_true(all(round(results_whole_delta$est_doses$yield, 3) == c(1.106, 1.295, 1.484)))
  expect_true(all(round(results_whole_delta$est_doses$dose, 3) == c(4.068, 4.485, 4.902)))

  # Expected outputs (partial-body)
  expect_equal(colnames(results_partial$est_doses), c("yield", "dose"))
  expect_equal(rownames(results_partial$est_doses), c("lower", "estimate", "upper"))
  expect_equal(round(results_partial$AIC, 3), 8.838)

  expect_true(all(round(results_partial$est_doses$yield, 3) == c(2.147, 2.546, 2.945)))
  expect_true(all(round(results_partial$est_doses$dose, 3) == c(5.748, 6.388, 7.027)))

  # Plot
  gg_curve <- plot_estimated_dose_curve(
    est_doses = list(
      whole = results_whole_delta,
      partial = results_partial
    ),
    fit_coeffs,
    fit_var_cov_mat,
    protracted_g_value,
    conf_int_curve = 0.83,
    aberr_name = to_title(aberr_module)
  )

  # Expected outcomes
  expect_equal(names(gg_curve$labels), c("colour", "shape", "x", "y", "ymin", "ymax"))
  expect_equal(unname(unlist(gg_curve$labels)), c("Assessment", "Estimation", "Dose (Gy)", "Translocations/cells", "yield_low", "yield_upp"))
})
