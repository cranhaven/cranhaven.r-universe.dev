library(testthat)

cm <- list(
  age = "Age", sex = "Sex",
  prior_fracture = "PriorFx", parent_fracture = "ParentFx", steroids = "Steroids",
  rheumatoid = "RA", secondary_op = "SecOP", smoker = "Smoker", alcohol = "Alcohol",
  bmd = "BMD"
)

test_that("mapping validation and missing columns error", {
  skip_on_cran()
  df <- data.frame(Age = 65, Sex = "Female")
  # partial col_map is supplemented by inference (sex inferred from 'Sex' column)
  expect_no_error(frax_score(df, list(age="Age")))
  expect_error(frax_score(df, list(age="Age", sex="SexX")), class = "healthmarkers_frax_error_missing_columns")
})

test_that("verbose emits col_map and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- data.frame(Age = 65, Sex = "Female")
  expect_message(frax_score(df, list(age = "Age", sex = "Sex"), verbose = TRUE), "frax_score")
  expect_message(frax_score(df, list(age = "Age", sex = "Sex"), verbose = TRUE), "col_map")
  expect_message(frax_score(df, list(age = "Age", sex = "Sex"), verbose = TRUE), "results:")
})

test_that("verbose double-fire guard", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- data.frame(Age = 65, Sex = "Female")
  msgs <- testthat::capture_messages(
    frax_score(df, list(age = "Age", sex = "Sex"), verbose = TRUE)
  )
  expect_gte(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})

test_that("NA policies: keep, omit, error, warn", {
  skip_on_cran()
  df <- data.frame(Age = c(70, NA), Sex = c("Male", "Female"))
  out_keep <- frax_score(df, list(age="Age", sex="Sex"), na_action = "keep")
  expect_equal(nrow(out_keep), 2L)
  expect_true(is.na(out_keep$frax_major_percent[2]))
  out_omit <- frax_score(df, list(age="Age", sex="Sex"), na_action = "omit")
  expect_equal(nrow(out_omit), 1L)
  expect_error(frax_score(df, list(age="Age", sex="Sex"), na_action = "error"),
               class = "healthmarkers_frax_error_missing_values")
  expect_warning(frax_score(df, list(age="Age", sex="Sex"), na_action = "warn"),
                 class = "healthmarkers_frax_warn_missing_inputs")
})

test_that("coercion warnings on numeric-like inputs", {
  skip_on_cran()
  df <- data.frame(Age = c("70", "oops"), Sex = c("Male", "Female"))
  expect_warning(frax_score(df, list(age="Age", sex="Sex")),
                 class = "healthmarkers_frax_warn_na_coercion")
})

test_that("sex normalization warning only", {
  skip_on_cran()
  df <- data.frame(Age = c(60, 70), Sex = c("X", "u"))  # ages in range
  expect_warning(
    frax_score(df, list(age = "Age", sex = "Sex")),
    class = "healthmarkers_frax_warn_unknown_sex"
  )
})

test_that("age range warning only", {
  skip_on_cran()
  df <- data.frame(Age = c(35, 95), Sex = c("male","female"))  # valid sex
  expect_warning(
    frax_score(df, list(age = "Age", sex = "Sex")),
    class = "healthmarkers_frax_warn_age_range"
  )
})

# If you want to assert both warnings together instead of splitting:
test_that("sex and age warnings together (optional)", {
  skip_on_cran()
  df <- data.frame(Age = c(35, 95), Sex = c("x","u"))
  w <- expect_warning(
    expect_warning(
      frax_score(df, list(age = "Age", sex = "Sex")),
      class = "healthmarkers_frax_warn_unknown_sex"
    ),
    class = "healthmarkers_frax_warn_age_range"
  )
})

test_that("risk increases with added risk factors; hip fraction behaved", {
  skip_on_cran()
  df <- data.frame(
    Age = rep(70, 3), Sex = rep("Female", 3),
    PriorFx = c(0,1,1), ParentFx = c(0,0,1),
    Steroids = 0, RA = 0, SecOP = 0, Smoker = 0, Alcohol = 0
  )
  out <- frax_score(df, list(age="Age", sex="Sex",
                             prior_fracture="PriorFx", parent_fracture="ParentFx"))
  expect_lt(out$frax_major_percent[1], out$frax_major_percent[2])
  expect_lt(out$frax_major_percent[2], out$frax_major_percent[3])
  # Hip is a fraction of major; at age 70 => frac = 0.4 + 0.2 = 0.6
  expect_equal(out$frax_hip_percent, round(out$frax_major_percent * 0.6, 1))
})

test_that("BMD T-score adjustment applies when in [-5, 0]", {
  skip_on_cran()
  df <- data.frame(Age = 70, Sex = "Female", BMD = -2.5)
  out_low <- frax_score(df, list(age="Age", sex="Sex", bmd="BMD"))
  df2 <- data.frame(Age = 70, Sex = "Female", BMD = -1.0)
  out_mild <- frax_score(df2, list(age="Age", sex="Sex", bmd="BMD"))
  expect_gt(out_low$frax_major_percent, out_mild$frax_major_percent)
})

test_that("capping at 95% works", {
  skip_on_cran()
  df <- data.frame(
    Age = 90, Sex = "Female",
    PriorFx = 1, ParentFx = 1, Steroids = 1, RA = 1, SecOP = 1, Smoker = 1, Alcohol = 1
  )
  out <- frax_score(df, cm)
  expect_lte(out$frax_major_percent, 95)
  expect_lte(out$frax_hip_percent, 95)
})

test_that("check_extreme removed: function passes through outlier rows", {
  skip_on_cran()
  cm <- list(age = "Age", sex = "Sex", bmd_t = "BMD")
  df <- data.frame(
    Age = c(20, 85, 100, 60),
    Sex = c("male","female","male","female"),
    BMD = c(-7, 0, 3, -10)
  )
  # age-range warning still fires; no extreme-scan error
  out <- suppressWarnings(frax_score(df, cm))
  expect_equal(nrow(out), 4L)
})

test_that("padding preserved for keep/warn", {
  skip_on_cran()
  df <- data.frame(Age = c(70, NA), Sex = c("Female", "Female"))
  out_keep <- frax_score(df, list(age="Age", sex="Sex"), na_action = "keep")
  out_warn <- suppressWarnings(frax_score(df, list(age="Age", sex="Sex"), na_action = "warn"))
  expect_equal(nrow(out_keep), 2L)
  expect_equal(nrow(out_warn), 2L)
})

test_that("country argument is accepted and does not error", {
  skip_on_cran()
  df <- data.frame(Age = 70, Sex = "Female")
  out <- frax_score(df, list(age="Age", sex="Sex"), country = "UK")
  expect_equal(nrow(out), 1L)
})
