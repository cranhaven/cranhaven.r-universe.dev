test_that("tiders work as expected", {
  mod <- readRDS(test_path("fixtures", "ln_fit_with_covariates.rds"))
  obtained <- tidy(mod, conf.int = TRUE)
  expected <- structure(
    list(
      term = c("(Intercept)_1", "x1_1", "(Intercept)_2", "x1_2"),
      estimate = c(3.53, 0.685, 4.04, 6.16),
      std.error = c(0.0139, 0.0178, 0.0074, 0.066),
      cred.low = c(3.52, 0.66, 4.03, 6.1),
      cred.high = c(3.55, 0.71, 4.05, 6.28)
    ),
    row.names = c(NA, -4L), class = c("draws_summary", "tbl_df", "tbl", "data.frame"),
    num_args = list()
  )
  expect_equal(obtained, expected, tolerance = 1)
})

test_that("tiders work as expected (EM)", {
  mod <- readRDS(test_path("fixtures", "em_fit_with_covariates.rds"))
  obtained <- tidy(mod)
  expected <- structure(
    list(
      term = c("(Intercept)_1", "x1_1", "(Intercept)_2", "x1_2"),
      estimate = c(3.47, 0.75, 3.97, 6.25)
    ),
    row.names = c(NA, -4L), 
    class = c("tbl_df", "tbl", "data.frame")
  )
  expect_equal(obtained, expected, tolerance = 1)
})
