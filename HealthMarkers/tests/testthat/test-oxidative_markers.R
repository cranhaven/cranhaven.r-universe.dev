test_that("oxidative_markers computes GSH/GSSG ratio", {
  skip_on_cran()
  df <- data.frame(GSH = c(1000, 1500), GSSG = c(10, 15))
  res <- oxidative_markers(df, col_map = list(GSH = "GSH", GSSG = "GSSG"))
  expect_s3_class(res, "tbl_df")
  expect_true("GSH_GSSG_Ratio" %in% names(res))
  expect_equal(res$GSH_GSSG_Ratio, c(100, 100), tolerance = 1e-10)
})

test_that("oxidative_markers safe division returns NA on zero denom", {
  skip_on_cran()
  df <- data.frame(GSH = c(10, 10), GSSG = c(0, 10))
  res <- oxidative_markers(df, col_map = list(GSH = "GSH", GSSG = "GSSG"))
  expect_true(is.na(res$GSH_GSSG_Ratio[1]))
  expect_equal(res$GSH_GSSG_Ratio[2], 1)
})

test_that("verbose emits preparing, column map, and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- data.frame(GSH = c(1000, 1500), GSSG = c(10, 15))
  expect_message(oxidative_markers(df, col_map = list(GSH = "GSH", GSSG = "GSSG"), verbose = TRUE), "oxidative_markers")
  expect_message(oxidative_markers(df, col_map = list(GSH = "GSH", GSSG = "GSSG"), verbose = TRUE), "col_map")
  expect_message(oxidative_markers(df, col_map = list(GSH = "GSH", GSSG = "GSSG"), verbose = TRUE), "results:")
})

test_that("verbose double-fire guard", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- data.frame(GSH = c(1000, 1500), GSSG = c(10, 15))
  msgs <- testthat::capture_messages(
    oxidative_markers(df, col_map = list(GSH = "GSH", GSSG = "GSSG"), verbose = TRUE)
  )
  expect_gte(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})

test_that("na_action='keep' propagates NA", {
  skip_on_cran()
  df <- data.frame(GSH = c(5, NA), GSSG = c(1, 2))
  res <- oxidative_markers(df, col_map = list(GSH = "GSH", GSSG = "GSSG"), na_action = "keep")
  expect_equal(res$GSH_GSSG_Ratio[1], 5)
  expect_true(is.na(res$GSH_GSSG_Ratio[2]))
})

test_that("na_action='omit' drops rows with NA", {
  skip_on_cran()
  df <- data.frame(GSH = c(5, NA, 3), GSSG = c(1, 2, 0.5))
  res <- oxidative_markers(df, col_map = list(GSH = "GSH", GSSG = "GSSG"), na_action = "omit")
  expect_equal(nrow(res), 2L)
  expect_equal(res$GSH_GSSG_Ratio, c(5, 6), tolerance = 1e-10)
})

test_that("na_action='error' aborts when NA present", {
  skip_on_cran()
  df <- data.frame(GSH = c(5, NA), GSSG = c(1, 2))
  expect_error(
    oxidative_markers(df, col_map = list(GSH = "GSH", GSSG = "GSSG"), na_action = "error"),
    "required inputs contain missing values"
  )
})

test_that("missing column in data errors with clear message", {
  skip_on_cran()
  df <- data.frame(GSH = c(5, 3))
  expect_error(
    oxidative_markers(df, col_map = list(GSH = "GSH", GSSG = "GSSG")),
    "GSSG"
  )
})

test_that("Inf GSSG yields NA ratio", {
  skip_on_cran()
  df <- data.frame(GSH = c(10, 10), GSSG = c(Inf, 5))
  res <- oxidative_markers(df, col_map = list(GSH = "GSH", GSSG = "GSSG"))
  expect_true(is.na(res$GSH_GSSG_Ratio[1]))
  expect_equal(res$GSH_GSSG_Ratio[2], 2)
})
