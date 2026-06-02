# File: tests/testthat/test_calc_sds.R

ref <- data.frame(
  variable = c("bmi", "sbp"),
  mean     = c(25, 120),
  sd       = c(5, 15),
  stringsAsFactors = FALSE
)

df <- data.frame(
  id  = 1:6,
  bmi = c(24, 30, 26, 29, 20, 26),
  sbp = c(118, 130, 119, 121, 140, 120)
)

test_that("calc_sds computes z-scores and returns structure", {
  res <- calc_sds(
    data = df,
    vars = c("bmi", "sbp"),
    ref = ref,
    id_col = "id",
    na_action = "keep",
    check_extreme = TRUE,
    extreme_action = "cap",
    warn_thresholds = list(na_prop = 1, extreme_prop = 1),
    return = "list",
    verbose = FALSE
  )
  expect_type(res, "list")
  expect_true(all(c("data", "summary", "warnings") %in% names(res)))
  out <- res$data
  expect_true(all(c("bmi_sds", "sbp_sds") %in% names(out)))
  expect_equal(out$bmi_sds, (df$bmi - 25) / 5)
  expect_equal(out$sbp_sds, (df$sbp - 120) / 15)
  expect_equal(res$summary$rows_in, nrow(df))
  expect_equal(res$summary$rows_out, nrow(df))
})

test_that("NA actions: keep, omit, error", {
  skip_on_cran()
  df_na <- df
  df_na$bmi[c(2, 5)] <- NA_real_
  # keep -> same rows
  res_keep <- calc_sds(
    data = df_na, vars = c("bmi", "sbp"), ref = ref,
    na_action = "keep", warn_thresholds = list(na_prop = 1, extreme_prop = 1),
    return = "list", verbose = FALSE
  )
  expect_equal(res_keep$summary$rows_out, nrow(df_na))
  expect_true(is.na(res_keep$data$bmi_sds[2]))
  # omit -> fewer rows
  res_omit <- calc_sds(
    data = df_na, vars = c("bmi", "sbp"), ref = ref,
    na_action = "omit", warn_thresholds = list(na_prop = 1, extreme_prop = 1),
    return = "list", verbose = FALSE
  )
  expect_equal(res_omit$summary$omitted_rows, 2)
  # error -> throws
  expect_error(
    calc_sds(data = df_na, vars = c("bmi", "sbp"), ref = ref,
             na_action = "error", warn_thresholds = list(na_prop = 1, extreme_prop = 1),
             verbose = FALSE),
    "Missing values found in `vars`"
  )
})

test_that("SDS extremes: cap, NA, error; and disabling via check_extreme", {
  skip_on_cran()
  df_ext <- df
  df_ext$bmi[1] <- 50  # mean=25, sd=5 -> z=5
  # cap -> warning and capped to 3
  expect_warning(
    res_cap <- calc_sds(df_ext, vars = c("bmi"),
                        ref = ref[ref$variable == "bmi",],
                        sds_cap = 3, extreme_action = "cap", check_extreme = TRUE,
                        warn_thresholds = list(na_prop = 1, extreme_prop = 1),
                        return = "list", verbose = FALSE),
    "Capped 1 SDS beyond"
  )
  expect_equal(res_cap$data$bmi_sds[1], 3)
  # NA -> no cap; becomes NA
  res_na <- calc_sds(df_ext, vars = c("bmi"),
                     ref = ref[ref$variable == "bmi",],
                     sds_cap = 3, extreme_action = "NA", check_extreme = TRUE,
                     warn_thresholds = list(na_prop = 1, extreme_prop = 1),
                     return = "list", verbose = FALSE)
  expect_true(is.na(res_na$data$bmi_sds[1]))
  # error -> stops
  expect_error(
    calc_sds(df_ext, vars = c("bmi"),
             ref = ref[ref$variable == "bmi",], sds_cap = 3,
             extreme_action = "error", check_extreme = TRUE,
             warn_thresholds = list(na_prop = 1, extreme_prop = 1),
             verbose = FALSE),
    "Found 1 SDS beyond"
  )
  # check_extreme = FALSE -> leave value unchanged (> cap)
  res_off <- calc_sds(df_ext, vars = c("bmi"),
                      ref = ref[ref$variable == "bmi",],
                      sds_cap = 3, extreme_action = "cap", check_extreme = FALSE,
                      warn_thresholds = list(na_prop = 1, extreme_prop = 1),
                      return = "list", verbose = FALSE)
  expect_gt(res_off$data$bmi_sds[1], 3)
})

test_that("Input validation: refs and args", {
  skip_on_cran()
  # missing column in data
  expect_error(
    calc_sds(df, vars = c("bmi", "sbp", "missing"), ref = ref, verbose = FALSE),
    "Missing column\\(s\\) in `data`"
  )
  # missing stats for a var in ref
  ref_missing <- subset(ref, variable == "bmi")
  expect_error(
    calc_sds(df, vars = c("bmi", "sbp"), ref = ref_missing, verbose = FALSE),
    "`ref` is missing stats for: sbp"
  )
  # zero or non-finite SD
  bad_ref <- ref; bad_ref$sd[bad_ref$variable == "bmi"] <- 0
  expect_error(
    calc_sds(df, vars = c("bmi"), ref = bad_ref, verbose = FALSE),
    "Reference SD must be > 0 and finite"
  )
  # sds_cap must be positive finite
  expect_error(
    calc_sds(df, vars = c("bmi"), ref = ref, sds_cap = 0, verbose = FALSE),
    "must be a positive finite"
  )
  # warn_thresholds can omit elements; defaults are applied -> no error
  expect_error(
    calc_sds(df, vars = c("bmi"), ref = ref, warn_thresholds = list(na_prop = 0.1), verbose = FALSE),
    NA
  )
})

test_that("Coercion of non-numeric vars warns and introduces NA", {
  skip_on_cran()
  df_chr <- df
  df_chr$bmi <- c("24", "30", "not-a-number", "29", "20", "26")
  expect_warning(
    res <- calc_sds(df_chr, vars = c("bmi"),
                    ref = ref[ref$variable == "bmi",],
                    warn_thresholds = list(na_prop = 1, extreme_prop = 1),
                    na_action = "keep",
                    return = "list", verbose = FALSE),
    "was coerced to numeric"
  )
  expect_true(is.na(res$data$bmi_sds[3]))
})

test_that("verbose = TRUE emits starting, column map, and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  expect_message(
    calc_sds(df, vars = c("bmi", "sbp"), ref = ref, verbose = TRUE,
             warn_thresholds = list(na_prop = 1, extreme_prop = 1)),
    "calc_sds"
  )
  expect_message(
    calc_sds(df, vars = c("bmi", "sbp"), ref = ref, verbose = TRUE,
             warn_thresholds = list(na_prop = 1, extreme_prop = 1)),
    "col_map"
  )
  expect_message(
    calc_sds(df, vars = c("bmi", "sbp"), ref = ref, verbose = TRUE,
             warn_thresholds = list(na_prop = 1, extreme_prop = 1)),
    "results:"
  )
})

test_that("verbose double-fire guard: each message fires exactly once", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  msgs <- testthat::capture_messages(
    calc_sds(df, vars = c("bmi", "sbp"), ref = ref, verbose = TRUE,
             warn_thresholds = list(na_prop = 1, extreme_prop = 1))
  )
  expect_equal(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})
