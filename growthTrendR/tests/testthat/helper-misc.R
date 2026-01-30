get_test_samples_trt <- function() {

  raw_data <- system.file("extdata", "dt.samples.csv", package = "growthTrendR")
  trt_data <- system.file("extdata", "dt.samples_trt.rds", package = "growthTrendR")

  raw_ok <- nzchar(raw_data) && file.exists(raw_data)
  trt_ok <- nzchar(trt_data) && file.exists(trt_data)

  # stop if BOTH are missing
  if (!raw_ok && !trt_ok) {
    stop("Neither raw_data nor trt_data could be found.")
  }

  if (trt_ok) {
    dt.samples_trt <- readRDS(trt_data)
  } else {
    dt.samples <- data.table::fread(raw_data)

    dt.samples_trt <- CFS_format(
      data    = list(dt.samples, 39:68),
      usage   = 1,
      out.csv = NULL
    )
  }

  dt.samples_trt
}





check_cfs_model <- function(m) {

  # basic structure
  testthat::expect_gt(length(names(m)), 0)
  testthat::expect_s3_class(m, "cfs_model")

  # expected names (contains, not exact match)
  testthat::expect_true(
    all(c("model", "fitting", "ptable", "stable", "pred") %in% names(m))
  )

  # conditional ML check
  if (length(m$model$m.candidates) > 1) {
    testthat::expect_true("fitting_ML" %in% names(m))
  }

  # element classes

  testthat::expect_true(inherits(m$model, "gamm") || inherits(m$model, "gam"))


  testthat::expect_s3_class(m$fitting, "data.table")
  testthat::expect_s3_class(m$ptable, "data.table")
  testthat::expect_s3_class(m$stable, "data.table")
  testthat::expect_s3_class(m$pred, "data.table")

  if (length(m$model$m.candidates) > 1) {
    testthat::expect_s3_class(m$fitting_ML, "data.table")
  }

}

