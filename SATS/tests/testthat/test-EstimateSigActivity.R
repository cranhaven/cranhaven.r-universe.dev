
context("EstimateSigActivity")

dir <- system.file("extdata", package="SATS", mustWork=TRUE)

# Load the baseline objects obj0 and matrices L, V, W
rdafile <- file.path(dir, "test_objects", "test_EstimateSigActivity.rda")
load(rdafile)

# Load simulated data
data(SimData, package="SATS")

set.seed(123, kind = "Mersenne-Twister", normal.kind = "Inversion") 

# Call the function to test
obj <- SATS::EstimateSigActivity(V, L, W)

# Compare result to the baseline. 
test_that("EstimateSigActivity",
{
  expect_equal(obj0$H, obj$H, tolerance=1e-4)
  expect_equal(obj0$loglike, obj$loglike, tolerance=1e-4)
  expect_equal(obj0$converged, obj$converged)
})


