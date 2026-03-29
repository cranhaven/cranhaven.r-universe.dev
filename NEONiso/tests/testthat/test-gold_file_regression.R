# test-gold_file_regression.R
# Snapshot-based regression tests for calibration outputs.
# Tests ensure calibration results remain stable across code changes.
#
# Carbon snapshots: baseline behavior from CRAN NEONiso 0.7.2.
# Water snapshots: current dev version (water data extraction changed
#   intentionally via useFasttime/runLocal in stackEddy).
#
# Cross-validation columns (cv5mae, cv5rmse, cvloo) are excluded because:
# - 5-fold CV uses random fold assignment (not reproducible)
# - cvloo for carbon differs between CRAN (caret) and dev (manual CV)
#
# To update snapshots after intentional changes: testthat::snapshot_accept()

fin <- system.file("extdata",
  "NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.packed.h5",
  package = "NEONiso", mustWork = TRUE)

# Columns to exclude: 5-fold cross-validation metrics use random fold
# assignment so they are not reproducible across runs. Leave-one-out CV
# (cvloo) is deterministic but differs between CRAN and dev for carbon.
cv5_cols <- c("cv5mae_12C", "cv5rmse_12C", "cv5mae_13C", "cv5rmse_13C",
              "d13C_cv5mae", "d13C_cv5rmse",
              "co2_cv5mae", "co2_cv5rmse",
              "cv5mae_18O", "cv5rmse_18O", "cv5mae_2H", "cv5rmse_2H",
              "cv5mae", "cv5rmse", "CVcalUcrt")
# Carbon comparisons additionally skip cvloo columns since the CRAN version
# used caret which computed CV differently.
carbon_cv_cols <- c(cv5_cols,
                    "cvloo_12C", "cvloo_13C",
                    "d13C_cvloo", "co2_cvloo",
                    "cvloo")

# Helper: remove unstable columns before snapshotting
remove_cols <- function(df, skip_cols) {
  df[, setdiff(names(df), skip_cols), drop = FALSE]
}

# Helper: remove unstable columns from ambient list (list of data frames)
remove_cols_ambient <- function(ambient_list, skip_cols) {
  lapply(ambient_list, function(df) remove_cols(df, skip_cols))
}

# ---- Carbon: gain-offset method ----

test_that("carbon gainoffset cal_df matches baseline", {

  suppressWarnings({
    co2_go <- calibrate_carbon(fin, tempfile(), "ONAQ",
      method = "gainoffset", calibration_half_width = 0.5,
      write_to_file = FALSE)
  })

  # Remove non-deterministic CV columns before snapshotting
  stable_cal_df <- remove_cols(co2_go$cal_df, carbon_cv_cols)
  expect_snapshot_value(stable_cal_df, style = "serialize")
})

test_that("carbon gainoffset ambient matches baseline", {

  suppressWarnings({
    co2_go <- calibrate_carbon(fin, tempfile(), "ONAQ",
      method = "gainoffset", calibration_half_width = 0.5,
      write_to_file = FALSE)
  })

  # Remove non-deterministic CV columns before snapshotting
  stable_ambient <- remove_cols_ambient(
    co2_go$ciso_subset_cal$`000_010_09m`,
    carbon_cv_cols
  )
  expect_snapshot_value(stable_ambient, style = "serialize")
})

# ---- Carbon: linear regression method ----

test_that("carbon linreg cal_df matches baseline", {

  co2_lr <- calibrate_carbon(fin, tempfile(), "ONAQ",
    method = "linreg", calibration_half_width = 0.5,
    write_to_file = FALSE)

  # Remove non-deterministic CV columns before snapshotting
  stable_cal_df <- remove_cols(co2_lr$cal_df, carbon_cv_cols)
  expect_snapshot_value(stable_cal_df, style = "serialize")
})

test_that("carbon linreg ambient matches baseline", {

  co2_lr <- calibrate_carbon(fin, tempfile(), "ONAQ",
    method = "linreg", calibration_half_width = 0.5,
    write_to_file = FALSE)

  # Remove non-deterministic CV columns before snapshotting
  stable_ambient <- remove_cols_ambient(
    co2_lr$ciso_subset_cal$`000_010_09m`,
    carbon_cv_cols
  )
  expect_snapshot_value(stable_ambient, style = "serialize")
})

# ---- Water ----

test_that("water cal_df matches baseline", {

  h2o <- calibrate_water(fin, tempfile(), "ONAQ", write_to_file = FALSE)

  # Remove non-deterministic CV columns before snapshotting
  stable_cal_df <- remove_cols(h2o$cal_df, cv5_cols)
  expect_snapshot_value(stable_cal_df, style = "serialize")
})

test_that("water ambient matches baseline", {

  h2o <- calibrate_water(fin, tempfile(), "ONAQ", write_to_file = FALSE)

  # Remove non-deterministic CV columns before snapshotting
  stable_ambient <- remove_cols_ambient(
    h2o$wiso_subset_cal$`000_010_09m`,
    cv5_cols
  )
  expect_snapshot_value(stable_ambient, style = "serialize")
})
