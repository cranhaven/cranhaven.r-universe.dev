context("initialize")

test_that("Errors", {
 err_args = list(
   "dta_factors" = c("dta" = iris, "eps" = rnorm(2), "pts" = c(3, 4)),
   "dta_empty" = c("dta" = data.frame(), "eps" = runif(2), "pts" = c(3, 4)),
   "dta_class" = c("dta" = rnorm(100), "eps" = runif(2), "pts" = c(3, 4)),
   "eps_sm0" = c("dta" = matrix(rnorm(100), ncol = 4), "eps" = runif(2, -2, -1), "pts" = c(3, 4)),
   "eps_chr" = c("dta" = matrix(rnorm(100), ncol = 4), "eps" = letters[1:2], "pts" = c(3, 4)),
   "eps_len" = c("dta" = matrix(rnorm(100), ncol = 4), "eps" = runif(3), "pts" = c(3, 4)),
   "eps_NA" = c("dta" = matrix(rnorm(100), ncol = 4), "eps" = NA, "pts" = c(3, 4)),
   "pts_sm0" = c("dta" = matrix(rnorm(100), ncol = 4), "eps" = runif(2), "pts" = c(-3, 4)),
   "pts_double" = c("dta" = matrix(rnorm(100), ncol = 4), "eps" = runif(2), "pts" = runif(2)),
   "pts_len" = c("dta" = matrix(rnorm(100), ncol = 4), "eps" = runif(2), "pts" = 3:5),
   "pts_NA" = c("dta" = matrix(rnorm(100), ncol = 4), "eps" = runif(2), "pts" = NA)
 )
 lapply(err_args, function(x) expect_error(do.call(FuzzyDBScan$new, x)))
})

test_that("Output", {
  eps = c(0.15, 0)
  pts = c(4, 8)
  data = factoextra::multishapes[, 1:2]
  fdbscan = FuzzyDBScan$new(data, eps, pts)
  test_character(fdbscan$point_def, len = nrow(data))
  test_numeric(fdbscan$dense, len = nrow(data))
  test_integerish(fdbscan$clusters, len = nrow(data))
})
