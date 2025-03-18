brapa_sample_data <- data.table::fread(system.file("extdata/brapa_arabidopsis_data.csv", package = "greatR"))
reference <- "Ro18"
query <- "Col0"
gene_data <- brapa_sample_data[gene_id == "BRAA03G051930.3C"]
all_data <- suppressMessages(preprocess_data(gene_data, reference, query, scaling_method = "z-score"))

test_that("match_names works", {
  a <- LETTERS[1:3]
  b <- LETTERS[4:5]

  # Expected outputs
  expect_error(match_names(x = a, lookup = b))
  expect_error(match_names(x = a[1:2], lookup = a))
  expect_no_error(match_names(x = a, lookup = a))
})

test_that("validate_params works", {
  # Expected outputs
  expect_no_error(suppressMessages(validate_params(stretches = 1, shifts = 0, registration_type = "optimisation")))
  expect_no_error(suppressMessages(validate_params(stretches = NA, shifts = NA, registration_type = "optimisation")))
  expect_no_error(suppressMessages(validate_params(stretches = 1, shifts = NA, registration_type = "optimisation")))
  expect_no_error(suppressMessages(validate_params(stretches = 1, shifts = 0, registration_type = "manual")))
  expect_error(suppressMessages(validate_params(stretches = NA, shifts = NA, registration_type = "manual")))
  expect_error(suppressMessages(validate_params(stretches = 1, shifts = NA, registration_type = "manual")))
})

test_that("cross_join works", {
  dt_a <- data.table(x = 1:2, y = 4:5)
  dt_b <- data.table(z = 1:3)
  dt_cj <- cross_join(dt_a, dt_b)

  # Expected outputs
  expect_equal(dim(dt_cj)[1], nrow(dt_a) * nrow(dt_b))
  expect_equal(dim(dt_cj)[2], ncol(dt_a) + ncol(dt_b))
  expect_equal(colnames(dt_cj), c(colnames(dt_a), colnames(dt_b)))
  expect_equal(dt_cj$x, rep(dt_a$x, each = 3))
  expect_equal(dt_cj$y, rep(dt_a$y, each = 3))
  expect_equal(dt_cj$z, rep(dt_b$z, times = 2))
})

test_that("get_approximate_stretch works", {
  approximate_stretch <- get_approximate_stretch(
    brapa_sample_data,
    reference = "Ro18",
    query = "Col0"
  )

  # Expected outputs
  expect_equal(approximate_stretch, 2.6666666, tolerance = 1e-6)
  expect_gte(approximate_stretch, 0)
  expect_equal(class(approximate_stretch), "numeric")
})

test_that("get_search_space_limits (auto) works", {
  space_lims <- get_search_space_limits(all_data)

  # Expected outputs
  expect_equal(names(space_lims), c("stretch_init", "stretch_lower", "stretch_upper", "shift_init", "shift_lower", "shift_upper"))
  expect_equal(space_lims$stretch_init, 2.667, tolerance = 1e-2)
  expect_equal(space_lims$stretch_lower, 1.333, tolerance = 1e-2)
  expect_equal(space_lims$stretch_upper, 4, tolerance = 1e-2)
  expect_equal(space_lims$shift_init, -7.667, tolerance = 1e-2)
  expect_equal(space_lims$shift_lower, -41, tolerance = 1e-2)
  expect_equal(space_lims$shift_upper, 49.667, tolerance = 1e-2)
  expect_no_error(get_search_space_limits(all_data))
  expect_error(get_search_space_limits(gene_data))
})

test_that("get_search_space_limits (bound) works", {
  stretches <- c(1, 2, 3)
  shifts <- c(-4, 4)
  space_lims <- get_search_space_limits(all_data, stretches = stretches, shifts = shifts)

  # Expected outputs
  expect_equal(names(space_lims), c("stretch_init", "stretch_lower", "stretch_upper", "shift_init", "shift_lower", "shift_upper"))
  expect_equal(space_lims$stretch_init, mean(stretches), tolerance = 1e-2)
  expect_equal(space_lims$stretch_lower, min(stretches), tolerance = 1e-2)
  expect_equal(space_lims$stretch_upper, max(stretches), tolerance = 1e-2)
  expect_equal(space_lims$shift_init, 0, tolerance = 1e-2)
  expect_equal(space_lims$shift_lower, min(shifts), tolerance = 1e-2)
  expect_equal(space_lims$shift_upper, max(shifts), tolerance = 1e-2)
})

test_that("get_search_space_limits (init) works", {
  stretches <- 1
  shifts <- 4
  space_lims <- get_search_space_limits(all_data, stretches = stretches, shifts = shifts)

  # Expected outputs
  expect_equal(names(space_lims), c("stretch_init", "stretch_lower", "stretch_upper", "shift_init", "shift_lower", "shift_upper"))
  expect_equal(space_lims$stretch_init, stretches, tolerance = 1e-2)
  expect_equal(space_lims$stretch_lower, 0.5, tolerance = 1e-2)
  expect_equal(space_lims$stretch_upper, 1.5, tolerance = 1e-2)
  expect_equal(space_lims$shift_init, shifts, tolerance = 1e-2)
  expect_equal(space_lims$shift_lower, -1, tolerance = 1e-2)
  expect_equal(space_lims$shift_upper, 33, tolerance = 1e-2)
})

test_that("calc_overlapping_percent works", {
  all_data_reg <- apply_registration(all_data, 3.10, -12.58)
  overlapping_raw <- calc_overlapping_percent(all_data)
  overlapping_reg <- calc_overlapping_percent(all_data_reg)

  # Expected outputs
  expect_gte(overlapping_reg, overlapping_raw)
  expect_equal(overlapping_reg, 1, tolerance = 1e-1)
})

test_that("bind_results works", {
  registration_results <- suppressMessages(register(
    brapa_sample_data[gene_id %in% c("BRAA03G051930.3C", "BRAA04G005470.3C")],
    reference = "Ro18",
    query = "Col0",
    scaling_method = "z-score",
    stretches = 3.10,
    shifts = -12.58,
    use_optimisation = FALSE
  ))

  registration_results_a <- suppressMessages(register(
    brapa_sample_data[gene_id == "BRAA03G051930.3C"],
    reference = "Ro18",
    query = "Col0",
    scaling_method = "z-score",
    stretches = 3.10,
    shifts = -12.58,
    use_optimisation = FALSE
  ))

  registration_results_b <- suppressMessages(register(
    brapa_sample_data[gene_id == "BRAA04G005470.3C"],
    reference = "Ro18",
    query = "Col0",
    scaling_method = "z-score",
    stretches = 3.10,
    shifts = -12.58,
    use_optimisation = FALSE
  ))

  registration_results_c <- suppressMessages(register(
    brapa_sample_data[gene_id == "BRAA04G005470.3C", ][, accession := ifelse(accession == "Ro18", "DH", "Col0")][],
    reference = "DH",
    query = "Col0",
    scaling_method = "z-score",
    stretches = 3.10,
    shifts = -12.58,
    use_optimisation = FALSE
  ))

  registration_results_ab1 <- bind_results(registration_results_a, registration_results_b)
  registration_results_ab2 <- bind_results(list(registration_results_a, registration_results_b))

  # Expected outputs
  expect_s3_class(registration_results_ab1, "res_greatR")
  expect_equal(registration_results_ab1$data, registration_results$data)
  expect_equal(registration_results_ab1$model_comparison, registration_results$model_comparison)
  expect_equal(registration_results_ab1, registration_results_ab2)
  expect_error(bind_results(registration_results_a, registration_results_c))
})
