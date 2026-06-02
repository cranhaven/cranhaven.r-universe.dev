# tests/testthat/test-obesity_metrics.R

library(testthat)
library(tibble)

# Base synthetic data for testing (realistic units: kg, m, cm)
base_df <- tibble(
  wt    = c(80, 90),      # kg
  ht    = c(1.75, 1.65),  # m
  waist = c(90, 100),     # cm
  hip   = c(100, 110),    # cm
  sex   = c(0, 1)         # 0 = male, 1 = female
)

# 1) Core metrics without options
test_that("core metrics compute correctly without options", {
  out <- obesity_indices(
    data = base_df,
    weight = wt,
    height = ht,
    waist = waist,
    hip = hip,
    weight_unit = "kg",
    height_unit = "m",
    adjust_WHR = FALSE,
    include_RFM = FALSE
  )

  # weight_kg and height_m
  expect_equal(out$weight_kg, c(80, 90))
  expect_equal(out$height_m, c(1.75, 1.65))

  # BMI and category
  expect_equal(out$BMI, c(80 / 1.75^2, 90 / 1.65^2), tolerance = 1e-6)
  expect_equal(out$BMI_cat, c("Overweight", "Obesity Class I"))

  # WHR and waist_to_height_ratio (both in cm for WHtR)
  expect_equal(out$WHR, c(90 / 100, 100 / 110), tolerance = 1e-8)
  expect_equal(out$waist_to_height_ratio, c(90 / 175, 100 / 165), tolerance = 1e-8)

  # waist_to_BMI_ratio and weight_to_height_ratio
  expect_equal(out$waist_to_BMI_ratio, c(90 / (80 / 1.75^2), 100 / (90 / 1.65^2)), tolerance = 1e-6)
  expect_equal(out$weight_to_height_ratio, c(80 / 1.75, 90 / 1.65), tolerance = 1e-8)

  # Advanced indices produce finite numerics
  expect_true(all(is.finite(out$AVI)))
  expect_true(all(is.finite(out$BAI)))
  expect_true(all(is.finite(out$ABSI)))
  expect_true(all(is.finite(out$BRI)))
  expect_true(all(is.finite(out$CI)))
})

# 2) Test adjust_WHR and include_RFM
test_that("adjust_WHR adds WHRadjBMI and include_RFM adds RFM", {
  skip_on_cran()
  out2 <- obesity_indices(
    data = base_df,
    weight = wt,
    height = ht,
    waist = waist,
    hip = hip,
    sex = sex,
    weight_unit = "kg",
    height_unit = "m",
    adjust_WHR = TRUE,
    include_RFM = TRUE
  )
  # WHRadjBMI exists and numeric
  expect_true("WHRadjBMI" %in% names(out2))
  expect_type(out2$WHRadjBMI, "double")
  # Residuals sum to zero (by construction for linear model residuals)
  expect_equal(sum(out2$WHRadjBMI), 0)

  # RFM computation: 64 - 20*(height_m / (waist_cm/100)) + 12*sex
  expected_rfm <- c(
    64 - 20 * (1.75 / (90 / 100)) + 12 * 0,
    64 - 20 * (1.65 / (100 / 100)) + 12 * 1
  )
  expect_equal(out2$RFM, expected_rfm, tolerance = 1e-8)
})

# 3) Units conversion
test_that("unit conversion works for lb and cm", {
  skip_on_cran()
  df_units <- tibble(
    wt_lb = 220,    # lb
    ht_cm = 180,    # cm
    waist = 90,     # cm-like numeric
    hip   = 100     # cm-like numeric
  )
  out <- obesity_indices(
    data = df_units,
    weight = wt_lb,
    height = ht_cm,
    waist = waist,
    hip = hip,
    weight_unit = "lb",
    height_unit = "cm",
    adjust_WHR = FALSE,
    include_RFM = FALSE
  )
  expect_equal(out$weight_kg, 220 * 0.45359237, tolerance = 1e-8)
  expect_equal(out$height_m, 180 / 100, tolerance = 1e-8)
  expect_equal(out$WHR, 90 / 100, tolerance = 1e-8)
})

# 4) Error handling
test_that("errors on missing columns or missing sex when RFM requested", {
  skip_on_cran()
  # Missing waist column
  df_missing <- base_df[, setdiff(names(base_df), "waist")]
  expect_error(
    obesity_indices(df_missing, wt, ht, waist, hip),
    "missing required columns"
  )

  # include_RFM = TRUE without sex argument
  expect_error(
    obesity_indices(
      data = base_df,
      weight = wt,
      height = ht,
      waist = waist,
      hip = hip,
      include_RFM = TRUE
    ),
    "must be provided to compute RFM"
  )
})

# 5) na_action policies
test_that("na_action policies behave as expected", {
  skip_on_cran()
  df_na <- tibble(
    wt    = c(80, NA_real_),
    ht    = c(2, 2),
    waist = c(1, 2),
    hip   = c(1, 1)
  )
  # error -> abort (suppress high-missingness warning)
  expect_error(
    suppressWarnings(
      obesity_indices(df_na, wt, ht, waist, hip,
                      weight_unit = "kg", height_unit = "m",
                      na_action = "error")
    ),
    "required inputs contain missing values"
  )
  # omit -> drop rows with NA
  out_omit <- suppressWarnings(
    obesity_indices(df_na, wt, ht, waist, hip,
                    weight_unit = "kg", height_unit = "m",
                    na_action = "omit")
  )
  expect_equal(nrow(out_omit), 1L)
  expect_equal(out_omit$BMI, 80 / (2^2))

  # keep -> NA propagates into BMI
  out_keep <- suppressWarnings(
    obesity_indices(df_na, wt, ht, waist, hip,
                    weight_unit = "kg", height_unit = "m",
                    na_action = "keep")
  )
  expect_true(is.na(out_keep$BMI[2]))
})

# 6) Extreme values pass through unchanged (no check_extreme)
test_that("extreme input values pass through without error", {
  skip_on_cran()
  df_ext <- tibble(
    wt    = 80,
    ht    = 200,
    waist = 500,
    hip   = 100
  )
  out <- obesity_indices(df_ext, wt, ht, waist, hip, height_unit = "m")
  expect_true(is.finite(out$WHR))
})

# 7) Denominator-zero summary warning
test_that("denominator zero emits a single summary warning", {
  skip_on_cran()
  df_zero <- tibble(
    wt    = 80,
    ht    = 2,
    waist = 1,
    hip   = 0  # zero denominator for WHR
  )
  expect_warning(
    out <- obesity_indices(df_zero, wt, ht, waist, hip),
    "zero denominators detected"
  )
  expect_true(is.na(out$WHR))
})

# 8) include_RFM with invalid sex values warns and sets NA
test_that("include_RFM warns on invalid sex values and sets NA", {
  skip_on_cran()
  df_bad_sex <- tibble(
    wt    = c(80, 70),
    ht    = c(2, 1.8),
    waist = c(1, 0.9),
    hip   = c(1, 1.0),
    sex   = c(2, -1)  # invalid codes
  )
  expect_warning(
    out <- obesity_indices(df_bad_sex, wt, ht, waist, hip, sex, include_RFM = TRUE),
    "sex' contains .* values not in \\{0,1\\}"
  )
  expect_true(all(is.na(out$RFM)))
})

# 9) Verbose emits col_map and results messages
test_that("verbose emits preparing, col_map, and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  expect_message(
    obesity_indices(base_df, wt, ht, waist, hip, verbose = TRUE),
    "obesity_indices"
  )
  expect_message(
    obesity_indices(base_df, wt, ht, waist, hip, verbose = TRUE),
    "col_map"
  )
  expect_message(
    obesity_indices(base_df, wt, ht, waist, hip, verbose = TRUE),
    "results:"
  )
})

test_that("verbose double-fire guard", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  msgs <- testthat::capture_messages(
    obesity_indices(base_df, wt, ht, waist, hip, verbose = TRUE)
  )
  expect_gte(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})
