# Basic utils ----

test_that("to_title works", {
  expect_equal(
    to_title("hello world"),
    "Hello World"
  )
})

test_that("match_names works", {
  # Correct name matching
  expect_no_error(
    match_names(
      c("whole", "hetero"),
      c("whole", "partial", "hetero")
    )
  )

  # Incorrect name matching
  expect_error(
    match_names(
      c("whole-body"),
      c("whole", "partial", "hetero")
    )
  )
})

test_that("names_from_model_formula works", {
  # Correct argument matching
  expect_error(names_from_model_formula("lon-quod"))

  # Expected names
  expect_equal(
    names_from_model_formula("lin-quad"),
    c("coeff_C", "coeff_alpha", "coeff_beta")
  )
  expect_equal(
    names_from_model_formula("lin"),
    c("coeff_C", "coeff_alpha")
  )
})

test_that("parse_model_formula works", {
  # Correct argument matching
  expect_error(parse_model_formula("lon-quod"))

  # Expected names
  parsed_model_formula <- parse_model_formula("lin-quad")
  expect_equal(parsed_model_formula$fit_formula_raw, "aberr ~ -1 + coeff_C + coeff_alpha + coeff_beta")
  expect_equal(parsed_model_formula$fit_formula_tex, "\\lambda = C + \\alpha D + \\beta D^{2}")

  parsed_model_formula <- parse_model_formula("lin")
  expect_equal(parsed_model_formula$fit_formula_raw, "aberr ~ -1 + coeff_C + coeff_alpha")
  expect_equal(parsed_model_formula$fit_formula_tex, "\\lambda = C + \\alpha D")
})


# Markdown utils ----

test_that("load_rmd_report works", {
  report_names <- c("estimation-report-docx.Rmd", "estimation-report-pdf.Rmd", "fitting-report-docx.Rmd", "fitting-report-pdf.Rmd")

  # Expected ouputs
  for (report in report_names) {
    expect_true(file.exists(load_rmd_report(report)))
  }
})

# Formatting fixes ----

fit_results <- app_sys("extdata", "dicentrics-fitting-results.rds") %>%
  readRDS()

test_that("fix_coeff_names works", {
  # Prepare data
  fit_var_cov_mat <- fit_results$fit_var_cov_mat %>%
    fix_coeff_names("rows", "kable") %>%
    fix_coeff_names("cols", "kable")

  # Expected outputs
  expect_equal(
    rownames(fit_var_cov_mat),
    c("$C$", "$\\alpha$", "$\\beta$")
  )
  expect_equal(
    colnames(fit_var_cov_mat),
    c("$C$", "$\\alpha$", "$\\beta$")
  )
})

test_that("fix_count_data_names for count data works", {
  # Prepare data
  count_data <- fit_results$fit_raw_data

  count_data_cols <- fix_count_data_names(count_data, type = "count", output = "kable") %>%
    colnames()
  count_data_cols_len <- length(count_data_cols)

  # Expected outputs
  expect_equal(
    count_data_cols[1:4],
    c("$D$ (Gy)", "$N$", "$X$", "$C_{0}$")
  )
  expect_equal(
    count_data_cols[seq(count_data_cols_len - 3, count_data_cols_len, 1)],
    c("$\\bar{y}$", "$\\hat{\\sigma}^{2}$", "$\\hat{\\sigma}^{2} / \\bar{y}$", "$u$")
  )
})

test_that("fix_count_data_names for case data works", {
  # Prepare data
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

  case_data_cols <- fix_count_data_names(case_data, type = "case", output = "kable") %>%
    colnames()

  case_data_cols_len <- length(case_data_cols)

  # Expected outputs
  expect_equal(
    case_data_cols[2:9],
    c("$N$", "$C_{0}$", "$C_{1}$","$C_{2}$","$C_{3}$","$C_{4}$","$C_{5}$","$X$")
  )
  expect_equal(
    case_data_cols[seq(case_data_cols_len - 6, case_data_cols_len, 1)],
    c("$F_{P}$", "$\\hat{\\sigma}_{P} / \\sqrt{N}$", "$\\hat{\\sigma}^{2} / \\bar{y}$", "$u$", "$X_{C}$", "$F_{G}$", "$\\hat{\\sigma}_{G} / \\sqrt{N}$")
  )
})


# Dose estimation ----

test_that("protracted_g_function works", {
  # Analytical solution
  product_log <- 0.2784645427610738

  # Expected output
  expect_equal(protracted_g_function(time = 4 * (1 + product_log), time_0 = 2), 0.5)
})

test_that("correct_boundary works", {
  # Expected outputs
  expect_equal(correct_boundary(-0.5), 0)
  expect_equal(correct_boundary(0.5), 0.5)
  expect_equal(correct_boundary(1.5), 1)
})
